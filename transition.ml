(*
- also take hints files as input

- generate hints
   > One can't specify binary packages. The syntax
   > "source-package/arch/source-version" is used for migrating
   > binNMUs and roughly translates as "the set of binaries built from
   > (source) version <source-version> of <source-package> on <arch>".

- Cannot we focus on just some relevant subrepositories?
  ===> any package changed between testing and unstable,
       all packages they depend on, and their immediate conflicts,
       all packages that depend directly on them

- Fixes:
  ==> what about removal and bugs?
  ==> make sure each bug is associated to an existing package
  ==> check age comparison (off by one or not?)
  ==> age and new packages?
  ==> Do not propagate empty source files

- Improved analysis of new packages: consider not upgrading some packages
  if this breaks a new package.
- Check : do not downgrade any package (source + bin)
- Generic solver
- Different policies: conservative, greedy
  (behavior when disjunction: skip all/one)
- Option to not remove any package (for proposed updates?)

- More constraints?
  ==> all release architectures are in sync


- we can generate problems (Deb_lib.generate_rules) in a smart way
  by starting from the set of packages we consider and following dependencies upwards

*)

let dir = Filename.concat (Sys.getenv "HOME") "debian-dists"
let archs = ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let sects = ["main"; "contrib"; "non-free"]
let ext = ".bz2"

(****)

let atomic = true

let verbose = false

(****)

let urgency_delay u =
  match u with
    "low"       -> 10
  | "medium"    -> 5
  | "high"      -> 2
  | "critical"  -> 0
  | "emergency" -> 0
  | _           -> assert false

let default_urgency = "low"

(****)

let rec make_directories f =
  let f = Filename.dirname f in
  if not (Sys.file_exists f) then begin
    try
      Unix.mkdir f (0o755)
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      make_directories f;
      Unix.mkdir f (0o755)
  end

let cached files cache f =
  let should_compute =
    try
      let cache_time = (Unix.stat cache).Unix.st_mtime in
      List.exists
        (fun file -> (Unix.stat file).Unix.st_mtime > cache_time)
        files
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      true
  in
  if should_compute then begin
    let res = f () in
    let tmp = cache ^ ".tmp" in
    make_directories tmp;
    let ch = open_out tmp in
    Marshal.to_channel ch res [];
    close_out ch;
    Sys.rename tmp cache;
    res
  end else begin
    let ch = open_in cache in
    let res = Marshal.from_channel ch in
    close_in ch;
    res
  end

module StringSet = Upgrade_common.StringSet

let _ =
Gc.set {(Gc.get ())
        with Gc.space_overhead = 300; Gc.max_overhead = 1000000}

module Timer = Util.Timer

(****)

let whitespaces = Str.regexp "[ \t]+"
let comma = Str.regexp ","

let now = truncate ((Unix.time () /. 3600. -. 15.) /. 24.)

let read_package_info file f =
  let ch = open_in file in
  let h = Hashtbl.create 101 in
  begin try
    while true do
      let l = input_line ch in
      match Str.split whitespaces l with
        [name; version; info] ->
          let version = Deb_lib.parse_version version in
          Hashtbl.add h (name, version) (f info)
      | _ ->
          assert false
    done;
    with End_of_file -> ()
  end;
  close_in ch;
  h

let read_dates file = read_package_info file int_of_string

let read_urgencies file =
  let cache = Filename.concat dir "cache/Urgencies" in
  cached [file] cache (fun () -> read_package_info file urgency_delay)

let read_bugs file =
  let ch = open_in file in
  let h = Hashtbl.create 101 in
  begin try
    while true do
      let l = input_line ch in
      match Str.split whitespaces l with
        [name; bugs] ->
          Hashtbl.add h name
            (List.fold_right StringSet.add
               (Str.split comma bugs) StringSet.empty)
      | _ ->
          assert false
    done;
    with End_of_file -> ()
  end;
  close_in ch;
  h

let read_extra_info () =
  let britney_file f = Filename.concat (Filename.concat dir "britney") f in
  let dates = read_dates (britney_file "Dates") in
  let urgencies = read_urgencies (britney_file "Urgency") in
  let testing_bugs = read_bugs (britney_file "testing_BugsV") in
  let unstable_bugs = read_bugs (britney_file "unstable_BugsV") in
  (dates, urgencies, testing_bugs, unstable_bugs)

(****)

module M = Deb_lib
module Repository = Repository.F(M)
open Repository

let package_files suite arch sect =
  [Filename.concat dir
     (Format.sprintf "%s/%s/binary-%s/Packages%s" suite sect arch ext);
   Filename.concat dir
     (Format.sprintf "%s/%s/debian-installer/binary-%s/Packages%s"
        suite sect arch ext)]

let bin_package_files suite arch =
  List.flatten (List.map (fun sect -> package_files suite arch sect) sects)

let load_bin_packages suite arch =
  let files = bin_package_files suite arch in
  let dist = M.new_pool () in
  List.iter
    (fun file ->
       let ch = File.open_in file in
       M.parse_packages dist [] ch;
       close_in ch)
    files;
  M.only_latest dist

let src_package_file suite sect =
  Filename.concat dir (Format.sprintf "%s/%s/source/Sources%s" suite sect ext)

let src_package_files suite =
  List.map (fun sect -> src_package_file suite sect) sects

let load_src_packages suite =
  let files = src_package_files suite in
  let dist = Hashtbl.create 101 in
  List.iter
    (fun file ->
       let ch = File.open_in file in
       M.parse_src_packages dist ch;
       close_in ch)
    files;
  M.src_only_latest dist

(****)

type reason =
  | Unchanged
  | Too_young
  | More_bugs
  | Conflict
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated of (string * M.version)
  | Binary_not_propagated of ((string * M.version) * string)

let print_reason f reason =
  match reason with
    Unchanged ->
      Format.fprintf f "no update"
  | Too_young ->
      Format.fprintf f "not old enough"
  | More_bugs ->
      Format.fprintf f "has more bugs"
  | Conflict ->
      Format.fprintf f "add conflicts"
  | Not_yet_built (src, v1, v2) ->
      Format.fprintf f "not yet rebuilt (source %s %a rather than %a)"
        src M.print_version v1 M.print_version v2
  | Source_not_propagated (src, v) ->
      Format.fprintf f "source package %s (%a) cannot be propagated"
        src M.print_version v
  | Binary_not_propagated ((src, v), arch) ->
      Format.fprintf f "binary package %s (%a / %s) cannot be propagated"
        src M.print_version v arch

let unchanged = Hashtbl.create 101

let propagation_rules = Hashtbl.create 101

let rec no_change pkg reason =
  let ((nm, version), arch) = pkg in
  if not (Hashtbl.mem unchanged (nm, arch)) then begin
    if verbose && reason <> Unchanged then
      Format.eprintf "Skipping %s (%a / %s): %a@."
        nm M.print_version version arch print_reason reason;
    Hashtbl.replace unchanged (nm, arch) ();
    let l = Hashtbl.find_all propagation_rules (nm, arch) in
    List.iter (fun (pkg', reason') -> no_change pkg' reason') l
  end

(* if pkg2 is unchanged, then pkg1 should be unchanged as well. *)
let associates pkg1 pkg2 reason =
  let ((nm, version), arch) = pkg2 in
  if Hashtbl.mem unchanged (nm, arch) then
    no_change pkg1 reason
  else
    Hashtbl.add propagation_rules (nm, arch) (pkg1, reason)

(*
Expectation:
   all binary packages have a corresponding source package of the same name
   (if not, in unstable, not propagated)
*)

let add_unstable_constraints arch src dist =
  Hashtbl.iter
    (fun _ p ->
      let (nm, v) = p.M.source in
      assert (Hashtbl.mem src nm);
      let v' = Hashtbl.find src nm in
      if M.compare_version v v' <> 0 then begin
(*
        Format.eprintf "%s/%s/%s (%s): %a / %a@."
          suite arch nm p.M.package M.print_version v M.print_version v'
*)
        no_change
          ((p.M.package, p.M.version), arch) (Not_yet_built (nm, v, v'))
      end else begin
        (* if source changed, move in block *)
      end)
    dist.M.packages_by_num

let check_sources suite arch src dist bin_of_source =
  Hashtbl.iter
    (fun _ p ->
      let (nm, v) = p.M.source in
      assert (Hashtbl.mem src nm);
         (* FIX: we do not deal with fake source packages *)
      let v' = Hashtbl.find src nm in
      if M.compare_version v v' <> 0 then begin
        Format.eprintf "%s/%s/%s (%s): %a / %a@."
          suite arch nm p.M.package M.print_version v M.print_version v'
      end else begin
        Hashtbl.add bin_of_source nm (p.M.package, arch)
      end)
    dist.M.packages_by_num

let check_binaries suite src bin_of_source =
  Hashtbl.iter
    (fun nm v ->
       if not (Hashtbl.mem bin_of_source nm) then begin
         Format.eprintf "%s/%s has no corresponding binary package@."
           suite nm
       end)
    src

(****)

let source_version src nm =
  try Some (Hashtbl.find src nm) with Not_found -> None
let same_source_version t u nm =
  match source_version t nm, source_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' = 0
  | _               -> false
let bin_version dist nm =
  try
    Some (List.hd !(Hashtbl.find dist.M.packages_by_name nm)).M.version
  with Not_found ->
    None
let same_bin_version t u nm =
  match bin_version t nm, bin_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' = 0
  | _               -> false

(****)

let stats l =
  let n = ref 0 in
  List.iter
    (fun (arch, t', u') ->
       let s = ref StringSet.empty in
       let consider_package _ p =
         let nm = p.M.package in
         if
           not (same_bin_version t' u' nm ||
                Hashtbl.mem unchanged (nm, arch))
         then begin
           if not (StringSet.mem nm !s) then begin
(*
             let pr_vers dist f nm =
               match bin_version dist nm with
                   None -> Format.fprintf f "ABSENT"
                 | Some v -> M.print_version f v
             in
             Format.printf "Change: %s %a -> %a@."
               nm (pr_vers t') nm (pr_vers u') nm
*)
           end;
           s := StringSet.add nm !s
         end
       in
       Hashtbl.iter consider_package t'.M.packages_by_num;
       Hashtbl.iter consider_package u'.M.packages_by_num;
       n := !n + StringSet.cardinal !s)
    l;
  Format.eprintf "Maybe changed: %d@." !n

(****)

let f() =
  let load_t = Timer.start () in
  let (dates, urgencies, testing_bugs, unstable_bugs) = read_extra_info () in
  let files =
    List.flatten
      (List.map
         (fun arch ->
            bin_package_files "testing" arch @
            bin_package_files "unstable" arch)
         archs)
  in
  let cache = Filename.concat dir "cache/Packages" in
  let l =
    cached files cache (fun () ->
    List.map
      (fun arch ->
         (arch,
          load_bin_packages "testing" arch,
          load_bin_packages "unstable" arch))
      archs)
  in
  let files =
    src_package_files "testing" @ src_package_files "unstable" in
  let cache = Filename.concat dir "cache/Sources" in
  let (t, u) =
    cached files cache (fun () ->
      (load_src_packages "testing", load_src_packages "unstable"))
  in
  Format.eprintf "Loading: %f@." (Timer.stop load_t);

  let init_t = Timer.start () in
  let old_enough src =
    let d = try Hashtbl.find dates src with Not_found -> now in
    let u = try Hashtbl.find urgencies src with Not_found -> 10 in
(*
  Format.eprintf ">>> %s (= %a) => %d %d@." p.M.package M.print_version p.M.version (now - d) u;
*)
    now > d + u
  in
  let get_bugs bugs p =
    try Hashtbl.find bugs p with Not_found -> StringSet.empty
  in
  let no_new_bugs p =
    StringSet.subset (get_bugs unstable_bugs p) (get_bugs testing_bugs p)
  in
  Hashtbl.iter
    (fun nm v ->
       if same_source_version t u nm then
         no_change ((nm, v), "source") Unchanged
       else begin
         (* Do not propagate a source package if not old enough *)
         if not (old_enough (nm, v)) then
           no_change ((nm, v), "source") Too_young;
         (* Do not propagate a source package if it has more bugs *)
         if not (no_new_bugs nm && no_new_bugs ("src:" ^ nm)) then
           no_change ((nm, v), "source") More_bugs
       end)
    u;
  (*FIX: do not downgrade *)
  List.iter
    (fun (arch, t', u') ->
       Hashtbl.iter
         (fun _ p ->
            let pkg = ((p.M.package, p.M.version), arch) in
            let (nm, v) = p.M.source in
            assert (Hashtbl.mem u nm);
            let v' = Hashtbl.find u nm in
            if same_bin_version t' u' p.M.package then
              no_change pkg Unchanged
            else begin
              (* Do not add a binary package if its source is not
                 the most up to date source file. *)
              if M.compare_version v v' <> 0 then
                no_change pkg (Not_yet_built (nm, v, v'));
              (* Do not upgrade a package if it has more bugs *)
              if not (no_new_bugs p.M.package) then
                no_change pkg More_bugs;
              (* We cannot add a binary package without also adding
                 its source. *)
              if not (same_source_version t u (fst p.M.source)) then begin
                associates pkg (p.M.source, "source")
                  (Source_not_propagated p.M.source);
              if atomic then
                associates (p.M.source, "source") pkg
                  (Binary_not_propagated pkg);
              end
            end)
         u'.M.packages_by_num;
       Hashtbl.iter
         (fun _ p ->
            (* We cannot remove a source package if a corresponding
               binary package still exists. *)
            if not (same_source_version t u (fst p.M.source)) then
              associates
                (p.M.source, "source") ((p.M.package, p.M.version), arch)
                (Binary_not_propagated ((p.M.package, p.M.version), arch)))
         t'.M.packages_by_num)
    l;
Format.eprintf "Initial constraints: %f@." (Timer.stop init_t);

  stats l;

  let l' =
    List.map
      (fun (arch, t', u') -> (arch, Upgrade_common.prepare_analyze t', u'))
      l
  in
  while
    let changed = ref false in
    List.iter
      (fun (arch, t', u') ->
         Format.printf "==================== %s@." arch;
         while
let step_t = Timer.start () in
           let s =
             Upgrade_common.find_problematic_packages
               ~check_new_packages:true t' u'
               (fun nm -> Hashtbl.mem unchanged (nm, arch))
           in
let t = Timer.start () in
           StringSet.iter
             (fun nm ->
                let p =
                  try
                    List.hd !(Hashtbl.find u'.M.packages_by_name nm)
                  with Not_found -> try
                    List.hd !(Hashtbl.find t'.Upgrade_common.dist.M.packages_by_name nm)
                  with Not_found ->
                    assert false
                in
                no_change ((nm, p.M.version), arch) Conflict)
             s;
Format.eprintf "  New constraints: %f@." (Timer.stop t);
Format.eprintf "Step duration: %f@." (Timer.stop step_t);
           let non_empty = not (StringSet.is_empty s) in
           if non_empty then changed := true;
           non_empty
         do () done)
      l';
stats l;
    !changed
  do () done;

  Hashtbl.iter
    (fun nm v ->
       if not (Hashtbl.mem unchanged (nm, "source")) then
         try
           let v' = Hashtbl.find u nm in
           Format.eprintf "Upgrade source package %s from %a to %a@." nm
             M.print_version v M.print_version v'
         with Not_found ->
           Format.eprintf "Remove source package %s@." nm)
    t;
  Hashtbl.iter
    (fun nm v ->
       if not (Hashtbl.mem t nm || Hashtbl.mem unchanged (nm, "source")) then
         Format.eprintf "Adding source package %s@." nm)
    u;
  List.iter
    (fun (arch, t', u') ->
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            let v = p.M.version in
            if not (Hashtbl.mem unchanged (nm, arch)) then
              try
                let v' =
                  (List.hd !(Hashtbl.find u'.M.packages_by_name nm)).M.version
                in
                Format.eprintf "Upgrade binary package %s/%s from %a to %a@."
                  nm arch M.print_version v M.print_version v'
              with Not_found ->
                Format.eprintf "Remove binary package %s/%s@." nm arch)
         t'.M.packages_by_num;
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if not (Hashtbl.mem unchanged (nm, arch)) then
              if not (Hashtbl.mem t'.M.packages_by_name nm) then
              Format.eprintf "Adding binary package %s/%s@." nm arch)
         u'.M.packages_by_num)
    l;
()

let _ = f()

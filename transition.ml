(*
- generate small easy hints
- when reducing the repositories, do not build intermediate tables (?)
  except for conflicts

RULES
=====
- old binary of libhunspell-1.2-0 still in unstable ==> what should we do?
  (do as if it was not there?)
- should be aware of arch:all packages (or not?)

- report current problems
  ==> what packages was not propagated just due to problems
  ==> what packages will not be propagated


- Different policies: conservative, greedy
  (behavior when disjunction: skip all/one)
- Improved analysis of new packages: consider not upgrading some packages
  if this breaks a new package.
- Generic solver
- We may want to reduce the repositories several times, and possibly
  reduce the number of packages in each repository

TESTS
=====
export TIMEFORMAT="%R"; for i in t/*; do echo -n "$i: "; time ~/Mancoosi/transition -input $i/var/data/ -heidi /tmp/foo >& /tmp/log`basename $i` && diff -u  $i/expected /tmp/foo; done
*)

let dir = ref (Filename.concat (Sys.getenv "HOME") "debian-dists/britney")
let archs = ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let sects = ["main"; "contrib"; "non-free"]
let ext = ".bz2"

(****)

let hint_file = ref "-"
let heidi_file = ref ""
let offset = ref 0

(****)

let print_stats = false

let atomic = true
let atomic_bin_nmus = atomic
let no_removal = true

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

let default_urgency = urgency_delay "low"

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
module ListTbl = Util.ListTbl

(****)

let whitespaces = Str.regexp "[ \t]+"
let comma = Str.regexp ","
let slash = Str.regexp "/"

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
          Hashtbl.add h name (version, f info)
      | _ ->
          assert false
    done;
  with End_of_file -> () end;
  close_in ch;
  h

let read_dates file = read_package_info file int_of_string

let read_urgencies file =
  let cache = Filename.concat !dir "cache/Urgencies" in
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
  with End_of_file -> () end;
  close_in ch;
  h

type hint =
  { h_block : (string, unit) Hashtbl.t;
    h_block_udeb : (string, unit) Hashtbl.t;
    h_urgent : (string, Deb_lib.version) Hashtbl.t;
    h_age_days : (string, Deb_lib.version option * int) Hashtbl.t }

let read_hints dir =
  let hints =
    { h_block = Hashtbl.create 16;
      h_block_udeb = Hashtbl.create 16;
      h_urgent = Hashtbl.create 16;
      h_age_days = Hashtbl.create 16 }
  in
  let files = Sys.readdir dir in
  Array.sort compare files;
  Array.iter
    (fun f ->
       let ch = open_in (Filename.concat dir f) in
       begin try
         while true do
           let l = input_line ch in
           let l = Str.split whitespaces l in
           begin match l with
             [] ->
               ()
           | s :: _ when s.[0] = '#' ->
               ()
           | "finished" :: _ ->
               raise End_of_file
           | _ ->
               Format.eprintf "# %s@." (String.concat " " l)
           end;
           match l with
           | "block" :: l ->
               List.iter (fun p -> Hashtbl.replace hints.h_block p ()) l
           | "block-udeb" :: l ->
               List.iter (fun p -> Hashtbl.replace hints.h_block_udeb p ()) l
           | "urgent" :: l ->
               List.iter
                 (fun p ->
                    match Str.split slash p with
                      [nm; v] -> Hashtbl.replace hints.h_urgent
                                   nm (Deb_lib.parse_version v)
                    | _       -> ())
                 l
           | "age-days" :: n :: l ->
               let n = int_of_string n in
               List.iter
                 (fun p ->
                    match Str.split slash p with
                      [nm; v] ->
                        let v =
                          if v = "-" then None else
                          Some (Deb_lib.parse_version v)
                        in
                        Hashtbl.replace hints.h_age_days nm (v, n)
                    | _ ->
                       ())
                 l
           | _ ->
               ()
         done
       with End_of_file -> () end;
       close_in ch
       )
    files;
  hints

let read_extra_info () =
  let britney_file f = Filename.concat !dir f in
  let dates = read_dates (britney_file "testing/Dates") in
  let urgencies = read_urgencies (britney_file "testing/Urgency") in
  let testing_bugs = read_bugs (britney_file "testing/BugsV") in
  let unstable_bugs = read_bugs (britney_file "unstable/BugsV") in
  let hints = read_hints  (britney_file "unstable/Hints") in
  (dates, urgencies, testing_bugs, unstable_bugs, hints)

(****)

module M = Deb_lib
module Repository = Repository.F(M)
open Repository

let bin_package_files suite arch =
  [Filename.concat !dir (Format.sprintf "%s/Packages_%s" suite arch)]

let src_package_files suite =
  [Filename.concat !dir (Format.sprintf "%s/Sources" suite)]

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
  | Blocked
  | Too_young
  | More_bugs
  | Conflict
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated of (string * M.version)
  | Binary_not_propagated of ((string * M.version) * string)
  | Atomic of ((string * M.version) * string) list
  | No_removal

let print_reason f reason =
  match reason with
    Unchanged ->
      Format.fprintf f "no update"
  | Blocked ->
      Format.fprintf f "blocked"
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
  | Atomic l ->
      Format.fprintf f "binary packages";
      List.iter
        (fun ((src, v), arch) ->
           Format.fprintf f " %s (%a / %s)" src M.print_version v arch)
        l;
      Format.fprintf f " cannot be propagated all at once"
  | No_removal ->
      Format.fprintf f "would be removed"

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

let all_or_none pkgs reason =
  List.iter
    (fun p1 ->
       List.iter
         (fun p2 ->
            if p1 <> p2 then associates p1 p2 reason)
         pkgs)
    pkgs

(****)

let source_version src nm =
  try Some (Hashtbl.find src nm) with Not_found -> None
let same_source_version t u nm =
  match source_version t nm, source_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' = 0
  | _               -> false
let no_new_source t u nm =
  match source_version t nm, source_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' >= 0
  | _               -> false

let bin_version dist nm =
  match ListTbl.find dist.M.packages_by_name nm with
    p :: _ -> Some p.M.version
  | []     -> None
let same_bin_version t u nm =
  match bin_version t nm, bin_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' = 0
  | _               -> false
let no_new_bin t u nm =
  match bin_version t nm, bin_version u nm with
    None, None      -> true
  | Some v, Some v' -> M.compare_version v v' >= 0
  | _               -> false

(****)

let reduce_repository_pair (arch, t, u) =
  let conflicts = ListTbl.create 101 in
  let add_conflicts dist p confls =
    List.iter
      (fun l ->
         List.iter
           (fun cstr ->
              List.iter
                (fun q ->
                   ListTbl.add conflicts p.M.package q.M.package;
                   ListTbl.add conflicts q.M.package p.M.package)
                (M.resolve_package_dep_raw dist cstr))
           l)
      confls
  in
  let compute_package_conflicts d1 d2 =
    Hashtbl.iter
      (fun _ p ->
         add_conflicts d2 p p.M.conflicts;
         add_conflicts d2 p p.M.breaks)
      d1.M.packages_by_num
  in
  compute_package_conflicts t t; compute_package_conflicts t u;
  compute_package_conflicts u t; compute_package_conflicts u u;

  let changed_packages = Hashtbl.create 101 in
  let consider_package _ p =
    let nm = p.M.package in
    if not (Hashtbl.mem unchanged (nm, arch)) then
      Hashtbl.replace changed_packages nm ()
  in
  Hashtbl.iter consider_package t.M.packages_by_num;
  Hashtbl.iter consider_package u.M.packages_by_num;

  let pkgs = Hashtbl.create 1024 in
  let rec add_package p =
    if not (Hashtbl.mem pkgs p) then begin
      Hashtbl.add pkgs p ();
      List.iter add_package (ListTbl.find conflicts p);
      List.iter follow_deps (ListTbl.find t.M.packages_by_name p);
      if Hashtbl.mem changed_packages p then
        List.iter follow_deps (ListTbl.find u.M.packages_by_name p)
    end
  and follow_deps p =
    follow_deps_2 t p; follow_deps_2 u p
  and follow_deps_2 d p =
    follow_deps_3 d p.M.depends; follow_deps_3 d p.M.pre_depends
  and follow_deps_3 d deps =
    List.iter
      (fun l ->
         List.iter
           (fun (nm, _) ->
              List.iter
                (fun q -> add_package q.M.package)
                (ListTbl.find d.M.provided_packages nm))
           l)
      deps
  in

  (* Changed packages should be kept. *)
  Hashtbl.iter (fun p _ -> add_package p) changed_packages;

  (* Packages unchanged but with stronger dependencies should be kept
     as well *)
  let stronger_deps l =
    (* Check whether there is a package that satisfies the dependency,
       that might not satisfy the dependency anymore. *)
    List.exists
      (fun d ->
         List.exists
           (fun cstr ->
              List.exists
                (fun p ->
                   (* If the package is left unchanged, the dependency
                      will remain satisfied *)
                   Hashtbl.mem changed_packages p.M.package &&
                   (* Otherwise, we check whether a replacement exists,
                      that still satisfies the dependency *)
                   List.for_all
                     (fun cstr' ->
                        List.for_all
                          (fun p' -> p.M.package <> p'.M.package)
                          (M.resolve_package_dep_raw u cstr'))
                     d)
                (M.resolve_package_dep_raw t cstr))
           d)
      l
  in
  Hashtbl.iter
    (fun _ p ->
       if not (Hashtbl.mem pkgs p.M.package) then
         if stronger_deps p.M.depends || stronger_deps p.M.pre_depends then
           add_package p.M.package)
    t.M.packages_by_num;

  let n = ref 0 in
  let m = ref 0 in
  let filter p =
    incr m;
    let nm = p.M.package in
    let keep = Hashtbl.mem pkgs nm in
    if keep then incr n;
    keep
  in
  let t' = M.new_pool () in
  M.merge2 t' filter t;
  let u' = M.new_pool () in
  M.merge2 u' filter u;
  Format.eprintf "==> %d/%d@." !n !m;
  (arch, t', u')

let reduce_repositories l =
  let t = Timer.start () in
  let l = List.map reduce_repository_pair l in
  Format.eprintf "Reducing repositories: %f@." (Timer.stop t);
  l


let stats l =
if print_stats then begin
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
           s := StringSet.add nm !s
         end
       in
       Hashtbl.iter consider_package t'.M.packages_by_num;
       Hashtbl.iter consider_package u'.M.packages_by_num;
       n := !n + StringSet.cardinal !s)
    l;
  Format.eprintf "Maybe changed: %d@." !n
end

(****)

let f() =
  let load_t = Timer.start () in
  let (dates, urgencies, testing_bugs, unstable_bugs, hints) =
    read_extra_info () in
  let files =
    List.flatten
      (List.map
         (fun arch ->
            bin_package_files "testing" arch @
            bin_package_files "unstable" arch)
         archs)
  in
  let cache = Filename.concat !dir "cache/Packages" in
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
  let cache = Filename.concat !dir "cache/Sources" in
  let (t, u) =
    cached files cache (fun () ->
      (load_src_packages "testing", load_src_packages "unstable"))
  in
  Format.eprintf "Loading: %f@." (Timer.stop load_t);

  let init_t = Timer.start () in
  let old_enough nm uv tv =
    let d =
      try
        let (v, d) = Hashtbl.find dates nm in
        if M.compare_version uv v = 0 then d else now
      with Not_found ->
        now
    in
    let u =
      if
        try
          M.compare_version uv (Hashtbl.find hints.h_urgent nm) = 0
        with Not_found ->
          false
      then
        0
      else
        try
          let (v, d) = Hashtbl.find hints.h_age_days nm in
          if
            match v with
              Some v -> M.compare_version v uv <> 0
            | None   -> false
          then
            raise Not_found;
          d
        with Not_found ->
          match tv with
            None ->
              default_urgency
          | Some v ->
              let l =
                List.filter
                  (fun (v', d) ->
                     M.compare_version v v' < 0 &&
                     M.compare_version v' uv <= 0)
                  (Hashtbl.find_all urgencies nm)
              in
              List.fold_left (fun u (_, u') -> min u u') default_urgency l
    in
(*
  Format.eprintf ">>> %s (= %a) => %d %d@." p.M.package M.print_version p.M.version (now - d) u;
*)
    now + !offset >= d + u
  in
  let get_bugs src bugs p =
    try Hashtbl.find bugs p with Not_found -> StringSet.empty
  in
  let no_new_bugs is_new p =
    if is_new then
      StringSet.is_empty (get_bugs u unstable_bugs p)
    else
      StringSet.subset (get_bugs u unstable_bugs p) (get_bugs t testing_bugs p)
  in
  let is_blocked nm =
    Hashtbl.mem hints.h_block nm || Hashtbl.mem hints.h_block_udeb nm
  in
  Hashtbl.iter
    (fun nm v ->
       (* We only propagate source packages with a larger version *)
       if no_new_source t u nm then
         no_change ((nm, v), "source") Unchanged
       else if is_blocked nm then
         no_change ((nm, v), "source") Blocked
       else begin
         (* Do not propagate a source package if not old enough *)
         let v' = source_version t nm in
         if not (old_enough nm v v') then
           no_change ((nm, v), "source") Too_young;
         (* Do not propagate a source package if it has more bugs *)
         let is_new = v' = None in
         if
           not (no_new_bugs is_new nm && no_new_bugs is_new ("src:" ^ nm))
         then
           no_change ((nm, v), "source") More_bugs
       end)
    u;
  if no_removal then begin
    Hashtbl.iter
      (fun nm v ->
         if source_version u nm = None then
           no_change ((nm, v), "source") No_removal)
      t
  end;
  List.iter
    (fun (arch, t', u') ->
       let bin_nmus = ListTbl.create 101 in
       Hashtbl.iter
         (fun _ p ->
            let pkg = ((p.M.package, p.M.version), arch) in
            let (nm, v) = p.M.source in
            assert (Hashtbl.mem u nm);
            let v' = Hashtbl.find u nm in
            (* We only propagate binary packages with a larger version *)
            if no_new_bin t' u' p.M.package then
              no_change pkg Unchanged
            else begin
              (* Do not add a binary package if its source is not
                 the most up to date source file. *)
              if M.compare_version v v' <> 0 then
                no_change pkg (Not_yet_built (nm, v, v'));
              (* Do not upgrade a package if it has more bugs *)
              let is_new = bin_version t' p.M.package = None in
              if not (no_new_bugs is_new p.M.package) then
                no_change pkg More_bugs;
              if not (same_source_version t u (fst p.M.source)) then begin
                (* We cannot add a binary package without also adding
                   its source. *)
                associates pkg (p.M.source, "source")
                  (Source_not_propagated p.M.source);
                (* If a source if propagated, all its binaries should
                   be propagated as well *)
                if atomic then
                  associates (p.M.source, "source") pkg
                    (Binary_not_propagated pkg);
              end else if atomic_bin_nmus then
                ListTbl.add bin_nmus p.M.source pkg
            end)
         u'.M.packages_by_num;
       ListTbl.iter
         (fun _ pkgs -> all_or_none pkgs (Atomic pkgs)) bin_nmus;
       Hashtbl.iter
         (fun _ p ->
            let pkg = ((p.M.package, p.M.version), arch) in
(*FIX: compare with package source version?*)
            if not (same_source_version t u (fst p.M.source)) then begin
              (* We cannot remove a source package if a corresponding
                 binary package still exists. *)
 (*FIX: disable this only for libraries?*)
              associates
                (p.M.source, "source") pkg (Binary_not_propagated pkg);
              (* We cannot remove a binary without removing its source *)
              if atomic then
                associates pkg (p.M.source, "source")
                  (Source_not_propagated p.M.source)
            end)
         t'.M.packages_by_num)
    l;
Format.eprintf "Initial constraints: %f@." (Timer.stop init_t);

  let l0 = l in
  let l = reduce_repositories l in
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
                  match ListTbl.find u'.M.packages_by_name nm with
                    p :: _ ->
                      p
                  | [] ->
                      match
                        ListTbl.find
                          t'.Upgrade_common.dist.M.packages_by_name nm
                      with
                        p :: _ ->
                          p
                      | [] ->
                          assert false
                in
                no_change ((nm, p.M.version), arch) Conflict)
             s;
Format.eprintf "  New constraints: %f@." (Timer.stop t);
Format.eprintf "Step duration: %f@." (Timer.stop step_t);
           let non_empty = not (StringSet.is_empty s) in
           if non_empty then changed := true;
stats l;
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
              match bin_version u' nm with
                Some v' ->
                  Format.eprintf "Upgrade binary package %s/%s from %a to %a@."
                    nm arch M.print_version v M.print_version v'
              | None ->
                  Format.eprintf "Remove binary package %s/%s@." nm arch)
         t'.M.packages_by_num;
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if not (Hashtbl.mem unchanged (nm, arch)) then
              if not (ListTbl.mem t'.M.packages_by_name nm) then
                Format.eprintf "Adding binary package %s/%s@." nm arch)
         u'.M.packages_by_num)
    l;

  let changes = ListTbl.create 101 in
  List.iter
    (fun (arch, t', u') ->
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if not (Hashtbl.mem unchanged (nm, arch)) then begin
              let (src, v) = p.M.source in
              ListTbl.add changes src (arch, v)
            end)
         u'.M.packages_by_num)
    l;
  let print_hints f =
    Format.fprintf f "easy";
    ListTbl.iter
      (fun nm l ->
         let (_, v) = List.hd l in
         if not (Hashtbl.mem unchanged (nm, "source")) then
           Format.fprintf f " %s/%a" nm M.print_version v
         else
           List.iter
             (fun (arch, v) ->
                Format.fprintf f " %s/%s/%a" nm arch M.print_version v)
             l)
      changes;
    Format.fprintf f "@."
  in
  print_hints Format.std_formatter;
  if !hint_file <> "-" then begin
    let ch = open_out !hint_file in
    print_hints (Format.formatter_of_out_channel ch);
    close_out ch
  end;
  let print_heidi ch =
    let lines = ref [] in
    let add_line nm vers arch =
      let b = Buffer.create 80 in
      Format.bprintf b "%s %a %s@." nm M.print_version vers arch;
      lines := Buffer.contents b :: !lines
    in
    let output_lines ch =
      List.iter (output_string ch) (List.sort compare !lines); lines := []
    in
    List.iter
      (fun (arch, t, u) ->
         let is_preserved nm = Hashtbl.mem unchanged (nm, arch) in
         Hashtbl.iter
           (fun _ p ->
              let nm = p.M.package in
              if is_preserved nm then add_line nm p.M.version arch)
           t.M.packages_by_num;
         Hashtbl.iter
           (fun _ p ->
              let nm = p.M.package in
              if not (is_preserved nm) then add_line nm p.M.version arch)
           u.M.packages_by_num;
         output_lines ch)
      (List.sort (fun (arch, _, _) (arch', _, _) -> compare arch arch') l0);
    let is_preserved nm = Hashtbl.mem unchanged (nm, "source") in
    Hashtbl.iter
      (fun nm vers -> if is_preserved nm then add_line nm vers "source")
      t;
    Hashtbl.iter
      (fun nm vers -> if not (is_preserved nm) then add_line nm vers "source")
      u;
    output_lines ch; flush ch
  in
  if !heidi_file <> "" then begin
    let ch = open_out !heidi_file in
    print_heidi ch;
    close_out ch
  end
    

let _ =
Arg.parse
  ["-input",
   Arg.String (fun d -> dir := d),
   "DIR       Select directory containing britney data";
   "-hints",
   Arg.String (fun f -> hint_file := f),
   "FILE      Output hints to FILE";
   "-heidi",
   Arg.String (fun f -> heidi_file := f),
   "FILE      Output Heidi results to FILE";
   "-offset",
   Arg.Int (fun n -> offset := n),
   "N      Move N days into the future"]
  (fun p -> ())
  ("Usage: " ^ Sys.argv.(0) ^ " OPTIONS\n\
    \n\
    Options:");
f()
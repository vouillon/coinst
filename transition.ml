(*
- Generate explanations

- Find what it takes to install a package
  ==> iterative: relax the problem until we can install the package
  ==> with clause learning
  ==> we could save learnt rules...

- parallelise the program: one process per architecture


XXX What if some bin packages are removed and there are no bin packages
to replace them?

- generate small easy hints
- when reducing the repositories, do not build intermediate tables (?)
  except for conflicts

REPORTS
=======
- when printing a report, put constraints such as bugs and age last,
  to get more details
- report core reasons for each packages (recursively follow reasons)

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
*)

let dir = ref (Filename.concat (Sys.getenv "HOME") "debian-dists/britney")
let archs = ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let sects = ["main"; "contrib"; "non-free"]
let ext = ".bz2"

let options = Hashtbl.create 17
let get_option key def =
  try
    match Hashtbl.find options key with
      [s] -> s
    | _   -> assert false
  with Not_found ->
    def

let testing () = get_option "TESTING" (Filename.concat !dir "testing")
let unstable () = get_option "UNSTABLE" (Filename.concat !dir "unstable")

(****)

let hint_file = ref "-"
let heidi_file = ref ""
let offset = ref 0
let small_hints = ref false

(****)

let print_stats = false

let atomic = true
let atomic_bin_nmus = atomic
let no_removal = ref true

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
    Util.make_directories tmp;
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
      | [] ->
          ()
      | _ ->
          assert false
    done;
  with End_of_file -> () end;
  close_in ch;
  h

let read_dates file = read_package_info file int_of_string

let read_urgencies file =
  let cache = Filename.concat (unstable ()) ".cache/Urgencies" in
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
  let dates = read_dates (Filename.concat (testing ()) "Dates") in
  let urgencies = read_urgencies (Filename.concat (testing ()) "Urgency") in
  let testing_bugs = read_bugs (Filename.concat (testing ()) "BugsV") in
  let unstable_bugs = read_bugs (Filename.concat (unstable ()) "BugsV") in
  let hints = read_hints  (Filename.concat (unstable ()) "Hints") in
  (dates, urgencies, testing_bugs, unstable_bugs, hints)

(****)

module M = Deb_lib
module Repository = Repository.F(M)
open Repository

let bin_package_files suite arch =
  [Filename.concat suite (Format.sprintf "Packages_%s" arch)]

let src_package_files suite = [Filename.concat suite "Sources"]

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
    (* Source *)
  | Blocked
  | Too_young of int * int
  | Binary_not_propagated of ((string * M.version) * string)
  | No_removal
  (* Both *)
  | More_bugs
  (* Binaries *)
  | Conflict of StringSet.t * StringSet.t
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated of (string * M.version)
  | Atomic of ((string * M.version) * string) list

let print_reason f reason =
  match reason with
    Unchanged ->
      Format.fprintf f "no update"
  | Blocked ->
      Format.fprintf f "blocked"
  | Too_young _ ->
      Format.fprintf f "not old enough"
  | More_bugs ->
      Format.fprintf f "has new bugs"
  | Conflict (s, s') ->
      if StringSet.is_empty s then
        Format.fprintf f "cannot be installed"
      else begin
        Format.fprintf f "relies propagation of binary packages";
        StringSet.iter (fun nm -> Format.fprintf f " %s" nm) s
      end;
      Format.fprintf f " (would break";
      StringSet.iter (fun nm -> Format.fprintf f " %s" nm) s';
      Format.fprintf f ")"
  | Not_yet_built (src, v1, v2) ->
      Format.fprintf f "not yet rebuilt (source %s %a rather than %a)"
        src M.print_version v1 M.print_version v2
  | Source_not_propagated (src, v) ->
      Format.fprintf f "source package %s (%a) cannot be propagated"
        src M.print_version v
  | Binary_not_propagated ((bin, v), arch) ->
      Format.fprintf f "binary package %s (%a / %s) cannot be propagated"
        bin M.print_version v arch
  | Atomic l ->
      Format.fprintf f "binary packages";
      List.iter
        (fun ((src, v), arch) ->
           Format.fprintf f " %s (%a / %s)" src M.print_version v arch)
        l;
      Format.fprintf f " cannot be propagated all at once"
  | No_removal ->
      Format.fprintf f "would be removed"

let unchanged = ListTbl.create 101

let propagation_rules = Hashtbl.create 101

let rec no_change pkg reason =
  let ((nm, version), arch) = pkg in
  let already = ListTbl.mem unchanged (nm, arch) in
  ListTbl.add unchanged (nm, arch) reason;
  if not already then begin
    if verbose && reason <> Unchanged then
      Format.eprintf "Skipping %s (%a / %s): %a@."
        nm M.print_version version arch print_reason reason;
    let l = Hashtbl.find_all propagation_rules (nm, arch) in
    List.iter (fun (pkg', reason') -> no_change pkg' reason') l
  end

(* if pkg2 is unchanged, then pkg1 should be unchanged as well. *)
let associates pkg1 pkg2 reason =
  let ((nm, version), arch) = pkg2 in
  if ListTbl.mem unchanged (nm, arch) then
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

(* XXXX Output explanations

Group explanations per source file

*)
let ignored_reason reason =
  match reason with
    Unchanged | Source_not_propagated _ | Binary_not_propagated _ | Atomic _ ->
      true
  | _ ->
      false

let rec interesting_reason nm nm' r =
  match r with
    Unchanged ->
      false
  | Binary_not_propagated ((bin, _), arch) ->
      List.exists (interesting_reason nm bin)
        (ListTbl.find unchanged (bin, arch))
  | Source_not_propagated _ ->
      false
  | More_bugs when nm = nm' ->
      false
  | _ ->
      true

let sort_and_uniq compare l =
  let rec uniq v l =
    match l with
      []      -> [v]
    | v' :: r -> if compare v v' = 0 then uniq v r else v :: uniq v' r
  in
  match List.sort compare l with
    []     -> []
  | v :: r -> uniq v r

let output_reasons filename =
  let ch = open_out filename in
  let f = Format.formatter_of_out_channel ch in

  let sources = ref [] in
(* XXX Some sources are missing (bin-NMU)... *)
  ListTbl.iter
    (fun (nm, arch) reasons ->
       if arch = "source" && List.exists (interesting_reason nm "") reasons then
         sources := (nm, reasons) :: !sources)
    unchanged;

  if !sources <> [] then begin
    Format.fprintf f "<ul>@.";
    List.iter
      (fun (nm, reasons) ->
         Format.fprintf f "<li>%s@." nm;
         Format.fprintf f "<ul>@.";
         let binaries = ref [] in
         List.iter
           (fun r ->
              if interesting_reason nm "" r then
              match r with
                Blocked ->
                  Format.fprintf f "<li>Left unchanged due to block request@."
              | Too_young (cur_ag, req_ag) ->
                  Format.fprintf f
                    "<li>Only %d days old.  Must be %d days old to go in@."
                    cur_ag req_ag
              | Binary_not_propagated ((bin, v), arch) ->
                  binaries := (bin, arch) :: !binaries
              | No_removal ->
                  Format.fprintf f "<li>Would have to be removed@."
              | More_bugs ->
                  Format.fprintf f "<li>Has new bugs@."
              | _ ->
                  assert false)
           reasons;
         List.iter
           (fun (nm', arch) ->
              Format.fprintf f "<li>Binary package %s/%s not propagated@."
                nm' arch;
             Format.fprintf f "<ul>@.";
             List.iter
               (fun r ->
                  if interesting_reason nm nm' r then
                  match r with
                    Conflict (s, s') ->
                      if StringSet.is_empty s then
                        Format.fprintf f "<li>Dependency not satisfied"
                      else begin
                        Format.fprintf f "<li>Needs binary packages";
                        StringSet.iter (fun nm -> Format.fprintf f " %s" nm) s
                      end;
                      if StringSet.cardinal s' = 1 && StringSet.mem nm' s' then
                        Format.fprintf f "@."
                      else begin
                        Format.fprintf f " (would break binary packages";
                        StringSet.iter (fun nm -> Format.fprintf f " %s" nm) s';
                        Format.fprintf f ")@."
                      end
                  | More_bugs ->
                      Format.fprintf f "<li>Has new bugs@."
                  | Not_yet_built _ ->
                      Format.fprintf f "<li>Not yet built@."
                  | Atomic _ ->
                      Format.fprintf f "<li>Other binary not migrated@."
                  | _ ->
                      assert false)
               (ListTbl.find unchanged (nm', arch));
             Format.fprintf f "</ul>@.")
           (sort_and_uniq compare !binaries);
         Format.fprintf f "</ul>@.")
      (List.sort (fun (nm1, _) (nm2, _) -> compare nm1 nm2) !sources);


(*
  | Conflict of StringSet.t
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated of (string * M.version)
  | Atomic of ((string * M.version) * string) list
*)

    Format.fprintf f "</ul>@."
  end;
  close_out ch

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
    if not (ListTbl.mem unchanged (nm, arch)) then
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
                ListTbl.mem unchanged (nm, arch))
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

type easy_hint =
  { mutable h_names : (string * string) list;
    mutable h_pkgs : (string * string) list;
    mutable h_live : bool }

module Union_find = struct

type 'a link =
    Link of 'a t
  | Value of 'a

and 'a t =
  { mutable state : 'a link }

let rec repr t =
  match t.state with
    Link t' ->
      let r = repr t' in
      t.state <- Link r;
      r
  | Value _ ->
      t

let rec get t =
  match (repr t).state with
    Link _  -> assert false
  | Value v -> v

let merge t t' =
  let t = repr t in
  let t' = repr t' in
  t'.state <- Link t

let elt v = { state = Value v }

end

let generate_small_hints buckets l =
  let to_consider = ref [] in
  ListTbl.iter
    (fun (src, arch) l ->
       let info = {h_names = [(src, arch)]; h_pkgs = l; h_live = true} in
       let elt = Union_find.elt info in
       to_consider := (info, elt) :: !to_consider)
    buckets;
  let package_repr = Hashtbl.create 101 in
  List.iter
    (fun (info, elt) ->
       List.iter (fun p -> Hashtbl.add package_repr p elt) info.h_pkgs)
    !to_consider;

  let l =
    List.map
      (fun (arch, t', u') ->
         (arch,
          Upgrade_common.prepare_analyze t',
          Upgrade_common.prepare_analyze u'))
      l
  in
  let n = List.length !to_consider in
  let i = ref 0 in
  List.iter
    (fun (info, elt) ->
incr i;
if info.h_live then begin
Format.eprintf "%d/%d:" !i n;
List.iter (fun (p, arch) -> Format.eprintf " %s/%s" p arch) info.h_names;
Format.eprintf "@."
end;
       while
         info.h_live &&
         List.exists
           (fun (arch, t, u) ->
              let h = Hashtbl.create 101 in
              let empty = ref true in
              List.iter
                (fun (p, arch') ->
                   if arch' = arch then begin
                     Hashtbl.add h p (); empty := false
                   end)
                info.h_pkgs;
              if !empty then false else begin
                let filter p =
                  let res =
                    not (Hashtbl.mem h p)
                  in
  (*
                  if not res then begin
                  let v1 =
                    (List.hd (ListTbl.find u.M.packages_by_name p)).M.version in
                    try
                  let v2 = match ListTbl.find t.Upgrade_common.dist.M.packages_by_name p with [p] -> p.M.version | _ -> raise Not_found
                  in
                  Format.eprintf "%s %a %a@." p M.print_version v1 M.print_version v2
                    with Not_found ->
                  Format.eprintf "%s %a@." p M.print_version v1

                  end;
  *)
                  res
                in
                let problems =
                  Upgrade_common.find_problematic_packages
                    ~check_new_packages:true t u filter
                in
                let pkgs =
                  List.fold_left
                    (fun s (cl, _) -> StringSet.union s cl.Upgrade_common.neg)
                    StringSet.empty problems
                in
                if not (StringSet.is_empty pkgs) then begin
                  Format.eprintf "BROKEN: ";
                  StringSet.iter (fun p -> Format.eprintf " %s" p) pkgs;
                  Format.eprintf "@."
                end;
                StringSet.iter
                  (fun nm ->
                    let elt' = Hashtbl.find package_repr (nm, arch) in
                    if Union_find.repr elt != Union_find.repr elt' then begin
                      let info' = Union_find.get elt' in
                      assert (info'.h_live);
                      Union_find.merge elt elt';
  (*
  List.iter (fun (p, arch) -> Format.printf " %s/%s" p arch) info.h_pkgs;
  Format.printf " /";
  List.iter (fun (p, arch) -> Format.printf " %s/%s" p arch) info'.h_pkgs;
  Format.printf "@.";
  *)
                      info.h_names <- info'.h_names @ info.h_names;
                      info.h_pkgs <- info'.h_pkgs @ info.h_pkgs;
                      info'.h_live <- false
                    end)
                  pkgs;
                not (StringSet.is_empty pkgs)
              end)
           l
       do () done)
    !to_consider;
  (*XXX Should iterate one more time to check that previous upgrade do
    not break further ones... *)

  let l = List.filter (fun info -> info.h_live) (List.map fst !to_consider) in
  List.map (fun info -> info.h_names) l

let generate_hints l =
  let l =
    List.map
      (fun (arch, t, u) ->
         let filter p = ListTbl.mem unchanged (p.M.package, arch) in
         let u' = M.new_pool () in
         M.merge2 u' (fun p -> not (filter p)) u;
         M.merge2 u' filter t;
         (arch, t, u'))
      l
  in
  let changes = ListTbl.create 101 in
  List.iter
    (fun (arch, t, u) ->
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if not (ListTbl.mem unchanged (nm, arch)) then begin
              let (src, v) = p.M.source in
              ListTbl.add changes src (arch, nm, Some v)
            end)
         u.M.packages_by_num;
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if
              not (ListTbl.mem unchanged (nm, arch))
                &&
              not (ListTbl.mem u.M.packages_by_name nm)
            then begin
              let (src, v) = p.M.source in
              ListTbl.add changes src (arch, nm, None)
            end)
         t.M.packages_by_num)
    l;
  let buckets = ListTbl.create 101 in
  let versions = Hashtbl.create 101 in
  ListTbl.iter
    (fun src l ->
       if not (ListTbl.mem unchanged (src, "source")) then
         List.iter
           (fun (arch, nm, v) ->
              begin match v with
                Some v -> Hashtbl.replace versions (src, "source") v
              | None   -> ()
              end;
              ListTbl.add buckets (src, "source") (nm, arch))
           l
       else
         List.iter
           (fun (arch, nm, v) ->
              begin match v with
                Some v -> Hashtbl.replace versions (src, arch) v
              | None   -> ()
              end;
              ListTbl.add buckets (src, arch) (nm, arch))
           l)
    changes;
  let hints =
    if !small_hints then
      generate_small_hints buckets (reduce_repositories l)
    else
      []
  in
  let print_pkg f src arch =
try
    let vers = Hashtbl.find versions (src, arch) in
    if arch = "source" then
      Format.fprintf f " %s/%a" src M.print_version vers
    else
      Format.fprintf f " %s/%s/%a" src arch M.print_version vers
with Not_found ->
Format.fprintf f " ***%s/%s***" src arch
  in
  let print_hint f l =
    Format.fprintf f "easy";
    List.iter (fun (src, arch) -> print_pkg f src arch) l;
    Format.fprintf f "@."
  in
  let print_hints f =
    if not !small_hints then begin
      Format.fprintf f "easy";
      ListTbl.iter (fun (src, arch) _ -> print_pkg f src arch) buckets;
      Format.fprintf f "@."
    end;
    List.iter (fun names -> print_hint f names) hints
  in
  print_hints Format.std_formatter;
  if !hint_file <> "-" then begin
    let ch = open_out !hint_file in
    print_hints (Format.formatter_of_out_channel ch);
    close_out ch
  end

(****)

let f () =
  let archs =
    try Hashtbl.find options "ARCHITECTURES" with Not_found -> archs in

  let load_t = Timer.start () in
  let (dates, urgencies, testing_bugs, unstable_bugs, hints) =
    read_extra_info () in
  let files =
    List.flatten
      (List.map
         (fun arch ->
            bin_package_files (testing ()) arch @
            bin_package_files (unstable ()) arch)
         archs)
  in
  let cache = Filename.concat (unstable ()) ".cache/Packages" in
  let l =
    cached files cache (fun () ->
    List.map
      (fun arch ->
         (arch,
          load_bin_packages (testing ()) arch,
          load_bin_packages (unstable ()) arch))
      archs)
  in
  let files =
    src_package_files (testing ()) @ src_package_files (unstable ()) in
  let cache = Filename.concat (unstable ()) ".cache/Sources" in
  let (t, u) =
    cached files cache (fun () ->
      (load_src_packages (testing ()), load_src_packages (unstable ())))
  in
  Format.eprintf "Loading: %f@." (Timer.stop load_t);

  let init_t = Timer.start () in
  let compute_ages nm uv tv =
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
    (now + !offset - d, u)
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
         let (cur_ag, req_ag) = compute_ages nm v v' in
         if cur_ag < req_ag then
           no_change ((nm, v), "source") (Too_young (cur_ag, req_ag));
         (* Do not propagate a source package if it has new bugs *)
         let is_new = v' = None in
         if
           not (no_new_bugs is_new nm && no_new_bugs is_new ("src:" ^ nm))
         then
           no_change ((nm, v), "source") More_bugs
       end)
    u;
  if !no_removal then begin
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
              (* Do not upgrade a package if it has new bugs *)
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
      (fun (arch, t', u') ->
         (arch,
          Upgrade_common.prepare_analyze t',
          Upgrade_common.prepare_analyze u'))
      l
  in
  while
    let changed = ref false in
    List.iter
      (fun (arch, t', u') ->
         Format.printf "==================== %s@." arch;
         while
let step_t = Timer.start () in
           let problems =
             Upgrade_common.find_problematic_packages
               ~check_new_packages:true t' u'
               (fun nm -> ListTbl.mem unchanged (nm, arch))
           in
let t = Timer.start () in
           let has_singletons =
             List.exists
               (fun (cl, _) -> StringSet.cardinal cl.Upgrade_common.pos = 1)
               problems
           in
           let arch_changed = ref false in
           List.iter
             (fun ({Upgrade_common.pos = pos;  neg = neg}, s) ->
                if
                  not (has_singletons && StringSet.cardinal pos > 1)
                then begin
                  let nm = StringSet.choose pos in
                  let p =
                    match
                      ListTbl.find u'.Upgrade_common.dist.M.packages_by_name nm
                    with
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
                  arch_changed := true;
                  no_change ((nm, p.M.version), arch) (Conflict (neg, s))
                end)
             problems;
Format.eprintf "  New constraints: %f@." (Timer.stop t);
Format.eprintf "Step duration: %f@." (Timer.stop step_t);
           if !arch_changed then changed := true;
stats l;
           !arch_changed
         do () done)
      l';
stats l;
    !changed
  do () done;


  Hashtbl.iter
    (fun nm v ->
       if not (ListTbl.mem unchanged (nm, "source")) then
         try
           let v' = Hashtbl.find u nm in
           Format.eprintf "Upgrade source package %s from %a to %a@." nm
             M.print_version v M.print_version v'
         with Not_found ->
           Format.eprintf "Remove source package %s@." nm)
    t;
  Hashtbl.iter
    (fun nm v ->
       if not (Hashtbl.mem t nm || ListTbl.mem unchanged (nm, "source")) then
         Format.eprintf "Adding source package %s@." nm)
    u;
  List.iter
    (fun (arch, t', u') ->
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            let v = p.M.version in
            if not (ListTbl.mem unchanged (nm, arch)) then
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
            if not (ListTbl.mem unchanged (nm, arch)) then
              if not (ListTbl.mem t'.M.packages_by_name nm) then
                Format.eprintf "Adding binary package %s/%s@." nm arch)
         u'.M.packages_by_num)
    l;

  generate_hints l;

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
         let is_preserved nm = ListTbl.mem unchanged (nm, arch) in
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
    let is_preserved nm = ListTbl.mem unchanged (nm, "source") in
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

let read_conf f =
  let ch = open_in f in
  begin try
    while true do
      let l = input_line ch in
      let l = Str.split whitespaces l in
      match l with
        [] ->
          ()
      | s :: _ when s.[0] = '#' ->
         ()
      | k :: "=" :: l ->
          Hashtbl.replace options k l
      | _ ->
          assert false
    done
  with End_of_file -> () end;
  close_in ch;
  heidi_file := get_option "HEIDI_OUTPUT" !heidi_file;
  no_removal := false

let _ =
let excuse_file = ref "" in
let spec =
  Arg.align
  ["--input",
   Arg.String (fun d -> dir := d),
   "DIR       Select directory containing britney data";
   "-c",
   Arg.String read_conf,
   "FILE      Read britney config FILE";
   "--hints",
   Arg.String (fun f -> hint_file := f),
   "FILE      Output hints to FILE";
   "--small",
   Arg.Unit (fun () -> small_hints := true),
   "          Generate small hints";
   "--heidi",
   Arg.String (fun f -> heidi_file := f),
   "FILE      Output Heidi results to FILE";
   "--excuses",
   Arg.String (fun f -> excuse_file := f),
   "FILE      Output excuses to FILE";
   "--offset",
   Arg.Int (fun n -> offset := n),
   "N         Move N days into the future";
   "--control-files",
   Arg.Unit (fun () -> ()),
   "          Currently ignored";
   "--auto-hinter",
   Arg.Unit (fun () -> ()),
   "          Currently ignored";
   "-v",
   Arg.Unit (fun () -> ()),
   "          Currently ignored";
   "--compatible",
   Arg.Unit (fun () -> ()),
   "          Currently ignored"]
in
Arg.parse spec (fun p -> ())
  ("Usage: " ^ Sys.argv.(0) ^ " OPTIONS\n\
    Computes which packages can migrate from sid to testing.\n\
    Takes as input either a britney data directory (option -input)\n\
    or a britney config file (option -c).\n\
    \n\
    Options:");
f();
if !excuse_file <> "" then output_reasons !excuse_file

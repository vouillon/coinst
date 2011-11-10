(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * These programs are free software; you can redistribute them and/or
 * modify them under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(*
- option to disable caching
- option to set the number of processors available
- should start finding conflicts while reducing repositories
  ==> if not yet available when finding conflucts, compute
  ==> if threads available, use them to reduce the repository
  
PRIORITIES
  ==> graphs for reporting co-installability issues:
      should include "problematic packages" in the graph,
      and illustrate how the issue changes between testing and unstable
      - packages may be in testing, sid, or both; in the latter case,
        they may have different version between the two
      - dependencies can be in testing, sid, or both
      - dependency targets may be in testing, sid, or both
      - same thing for each side of a conflict (!)
      indicate which set of packages are made non-coinstallable

  ==> option to indicate which packages would be propagated by britney
      but not by this tool
  ==> find what it takes to install a package:
      iterative (relax the problem until we can install the package);
      with clause learning
  ==> add possibility to migrate packages with co-installability issues
      (find out the right kind of hints)

PERFORMANCE
  ==> parallelise the program: one process per architecture

EXPLANATIONS
  ==> link to build logs / merge packages
  ==> link to http://packages.qa.debian.org (for source files)
  ==> link to bugs
  ==> show source and binary versions
  ==> three step: no age/bug constraints, bugs added, all
  ==> summaries; in particular, show packages that only wait for age,
      for bugs
  ==> explanation of co-installability issues
*)

let dir = ref (Filename.concat (Sys.getenv "HOME") "debian-dists/britney")
let archs = ref ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let smooth_updates = ref ["libs"; "oldlibs"]

let cache_dir = Filename.concat (Sys.getenv "HOME") ".coinst"

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

let hint_file = ref ""
let heidi_file = ref ""
let excuse_file = ref ""
let offset = ref 0
let all_hints = ref false

(****)

let debug = Debug.make "normal" "Set normal debug output level." []
let verbose =
  Debug.make "explain" "Explain why packages are not propagated." ["normal"]
let debug_time = Debug.make "time" "Print execution times" ["normal"]
let debug_reduction =
  Debug.make "reduction" "Debug repository size reduction" ["normal"]
let debug_coinst =
  Debug.make "coinst" "Debug co-installability issue analyse" ["normal"]
let debug_outcome =
  Debug.make "outcome" "Print the possible changes" ["normal"]
let debug_hints =
  Debug.make "hints" "Output suggested hints to standard output" ["normal"]

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

let make_uid () =
  let magic1 = 0xcab4ea850533f24dL in
  let magic2 = 0xb517d4f5440b7995L in
  Format.sprintf "%16Lx"
    (Int64.logxor
       (Int64.mul magic1 (Int64.of_float (1e6 *. Unix.gettimeofday ())))
       (Int64.mul magic2 (Int64.of_int (Unix.getpid ()))))

let cache_disabled = ref false

let cached ?(force=false) files cache magic f =
  let magic =
    Format.sprintf
      "This cache file can be safely removed at any time.\n%s\n%s\n\n"
      magic (String.concat "\n" files)
  in
  let ch = try Some (open_in cache) with Sys_error _ -> None in
  let should_compute =
    !cache_disabled || force ||
    match ch with
      None ->
        true
    | Some ch ->
        (try
           let cache_time =
             (Unix.fstat (Unix.descr_of_in_channel ch)).Unix.st_mtime in
             List.exists
               (fun file -> (Unix.stat file).Unix.st_mtime > cache_time)
               files
         with Unix.Unix_error (Unix.ENOENT, _, _) ->
           true)
          ||
        (try
           let l = String.length magic in
           let s = String.create l in
           really_input ch s 0 l;
           s <> magic
         with End_of_file ->
           true)
  in
  if should_compute then begin
    begin match ch with Some ch -> close_in ch | None -> () end;
    let res = f () in
    let uid = make_uid () in
    if not !cache_disabled then begin
      let tmp = cache ^ ".tmp" in
      Util.make_directories tmp;
      let ch = open_out tmp in
      output_string ch magic;
      output_string ch uid;
      Marshal.to_channel ch res [];
      close_out ch;
      Sys.rename tmp cache
    end;
    (res, uid)
  end else begin
    match ch with
      Some ch ->
        let uid = String.create 16 in
        really_input ch uid 0 16;
        let res = Marshal.from_channel ch in
        close_in ch;
        (res, uid)
    | None ->
        assert false
  end

module StringSet = Upgrade_common.StringSet

let _ =
Printexc.record_backtrace true;
Gc.set { (Gc.get ())
         with Gc.space_overhead = 200; max_overhead = 1000000;
              major_heap_increment = 5 * 1024 * 1024 }

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
  let cache = Filename.concat cache_dir "Urgencies" in
  fst (cached [file] cache "version 1"
         (fun () -> read_package_info file urgency_delay))

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

let debug_read_hints = Debug.make "read_hints" "Show input hints." ["normal"]

exception Ignored_hint

let read_hints dir =
  let hints =
    { h_block = Hashtbl.create 16;
      h_block_udeb = Hashtbl.create 16;
      h_urgent = Hashtbl.create 16;
      h_age_days = Hashtbl.create 16 }
  in
  if debug_read_hints () then
    Format.eprintf "Reading hints:@.";
  let files = Sys.readdir dir in
  Array.sort compare files;
  Array.iter
    (fun f ->
       let file = Filename.concat dir f in
       if Sys.is_directory file || f = "README" || f = "index.html" || f.[0] = '.' then
         ()
       else
       let ch = open_in (Filename.concat dir f) in
       begin try
         while true do
           let l = input_line ch in
           let l = Str.split whitespaces l in
           try
             begin match l with
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
             | "finished" :: _ ->
                 raise End_of_file
             | [] ->
                 raise Ignored_hint
             | s :: _ when s.[0] = '#' ->
                 raise Ignored_hint
             | _ ->
               if debug_read_hints () then
                 Format.eprintf "> (ignored) %s@." (String.concat " " l);
                 raise Ignored_hint
             end;
             if debug_read_hints () then
               Format.eprintf "> %s@." (String.concat " " l)
           with Ignored_hint ->
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
  let hints = read_hints  (Filename.concat (unstable ()) "Hints") in
  (dates, urgencies, hints)

let read_bugs () =
  let testing_bugs = read_bugs (Filename.concat (testing ()) "BugsV") in
  let unstable_bugs = read_bugs (Filename.concat (unstable ()) "BugsV") in
  (testing_bugs, unstable_bugs)

(****)

module M = Deb_lib
module Repository = Repository.F(M)
open Repository

let bin_package_file suite arch =
  Filename.concat suite (Format.sprintf "Packages_%s" arch)

let src_package_file suite = Filename.concat suite "Sources"

let load_bin_packages suite arch =
  let file = bin_package_file suite arch in
  let dist = M.new_pool () in
  assert (not (Sys.is_directory file));
  let ch = File.open_in file in
  M.parse_packages dist [] ch;
  close_in ch;
  M.only_latest dist

let load_src_packages suite =
  let file = src_package_file suite in
  let dist = Hashtbl.create 101 in
  assert (not (Sys.is_directory file));
  let ch = File.open_in file in
  M.parse_src_packages dist ch;
  close_in ch;
  M.src_only_latest dist

(****)

type reason =
  | Unchanged
    (* Source *)
  | Blocked
  | Too_young of int * int
  | Binary_not_propagated of (string * string)
  | No_binary
  (* Both *)
  | More_bugs
  (* Binaries *)
  | Conflict of StringSet.t * StringSet.t
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated of (string * M.version)
  | Atomic of (string * string) list

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
        Format.fprintf f "relies on propagation of binary packages";
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
  | Binary_not_propagated (bin, arch) ->
      Format.fprintf f "binary package %s (%s) cannot be propagated"
        bin arch
  | No_binary ->
      Format.fprintf f "no associated binary package"
  | Atomic l ->
      Format.fprintf f "binary packages";
      List.iter
        (fun (src, arch) -> Format.fprintf f " %s (%s)" src arch)
        l;
      Format.fprintf f " cannot be propagated all at once"

let unchanged_reasons = ListTbl.create 101
let unchanged = Hashtbl.create 17

let propagation_rules = Hashtbl.create 101

let init_changes () =
  List.iter (fun arch -> Hashtbl.add unchanged arch (Hashtbl.create 1001))
    ("source" :: !archs)

let rec no_change pkg reason =
  ListTbl.add unchanged_reasons pkg reason;
  let (nm, arch) = pkg in
  let unchanged' = Hashtbl.find unchanged arch in
  if not (Hashtbl.mem unchanged' nm) then begin
    Hashtbl.add unchanged' nm ();
    if reason <> Unchanged && verbose () then begin
      Format.eprintf "Skipping %s (%s): %a@." nm arch print_reason reason
    end;
    let l = Hashtbl.find_all propagation_rules pkg in
    List.iter (fun (pkg', reason') -> no_change pkg' reason') l
  end

(* if pkg2 is unchanged, then pkg1 should be unchanged as well. *)
let associates pkg1 pkg2 reason =
  let (nm, arch) = pkg2 in
  if Hashtbl.mem (Hashtbl.find unchanged arch) nm then
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

let learnt_rules = ref []

let learn_rule nm arch neg s =
  learnt_rules := (nm, arch, neg, s) :: !learnt_rules

let load_rules uids =
  let cache = Filename.concat cache_dir "Rules" in
  let uids = String.concat "\n" uids in
  let (rules, _) = cached [] cache ("version 2\n" ^ uids) (fun () -> []) in
  List.iter
    (fun (nm, arch, neg, s) ->
       if StringSet.is_empty neg then
         no_change (nm, arch) (Conflict (neg, s))
       else if StringSet.cardinal neg = 1 then
         associates
           (nm, arch) (StringSet.choose neg, arch) (Conflict (neg, s)))
    rules;
  learnt_rules := rules

let save_rules uids =
  let cache = Filename.concat cache_dir "Rules" in
  let uids = String.concat "\n" uids in
  ignore (cached ~force:true [] cache ("version 2\n" ^ uids)
            (fun () -> List.rev !learnt_rules))

(****)

let sort_and_uniq compare l =
  let rec uniq v l =
    match l with
      []      -> [v]
    | v' :: r -> if compare v v' = 0 then uniq v r else v :: uniq v' r
  in
  match List.sort compare l with
    []     -> []
  | v :: r -> uniq v r

let compare_pair compare1 compare2 (a1, a2) (b1, b2) =
  let c = compare1 a1 b1 in
  if c = 0 then compare2 a2 b2 else c

let group compare l =
  match l with
    [] ->
      []
  | (a, b) :: r ->
      let rec group_rec a bl l =
        match l with
          [] ->
            [(a, List.rev bl)]
        | (a', b) :: r ->
            if compare a a' = 0 then
              group_rec a (b :: bl) r
            else
              (a, List.rev bl) :: group_rec a' [b] r
      in
      group_rec a [b] r

(****)

type st =
  { arch : string;
    testing : M.pool;
    unstable : M.pool;
    testing_srcs : (string, M.s) Hashtbl.t;
    unstable_srcs : (string, M.s) Hashtbl.t;
    unstable_bugs : (string, StringSet.t) Hashtbl.t;
    testing_bugs : (string, StringSet.t) Hashtbl.t;
    mutable upgrade_state :
      (Upgrade_common.state * Upgrade_common.state) option;
    uid : string }

(****)

let rec interesting_reason r =
  match r with
    Unchanged ->
      false
  | Binary_not_propagated (bin, arch) ->
      List.exists interesting_reason
        (ListTbl.find unchanged_reasons (bin, arch))
  | Source_not_propagated _ ->
      false
  | Atomic _ ->
      false
  | _ ->
      true

let binary_sources st l =
  List.map
    (fun nm ->
       let source =
         match ListTbl.find st.unstable.M.packages_by_name nm with
           p :: _ -> fst p.M.source
         | []     -> match ListTbl.find st.testing.M.packages_by_name nm with
                       p :: _ -> fst p.M.source
                     | []     -> assert false
       in
       (nm, source))
    l

let binary_sources = Task.funct binary_sources

let output_reasons l filename =
  let t = Timer.start () in
  let ch = open_out filename in
  let f = Format.formatter_of_out_channel ch in

  let blocked_source = Hashtbl.create 1024 in
  let sources = ref [] in
  let binaries = ListTbl.create 17 in
  let add_binaries arch s =
    StringSet.iter (fun p -> ListTbl.add binaries arch p) s in
  ListTbl.iter
    (fun (nm, arch) reasons ->
       if arch = "source" && List.exists interesting_reason reasons then begin
         sources := (nm, reasons) :: !sources;
         Hashtbl.add blocked_source nm ()
       end;
       List.iter
         (fun r ->
            match r with
              Conflict (s, s') -> add_binaries arch s; add_binaries arch s'
            | _                -> ())
         reasons)
    unchanged_reasons;

  let source_of_binary = Hashtbl.create 101 in
  Task.iteri l
    (fun (arch, st) -> (arch, binary_sources st (ListTbl.find binaries arch)))
    (fun arch l ->
       List.iter
         (fun (nm, src) -> Hashtbl.add source_of_binary (nm, arch) src) l);

  let print_binary f (nm, arch) =
    let source = Hashtbl.find source_of_binary (nm, arch) in
    if not (Hashtbl.mem blocked_source source) then begin
      if source = nm then
        Format.fprintf f "%s" nm
      else
        Format.fprintf f "%s (from %s)" nm source
    end else begin
      if source = nm then
        Format.fprintf f "<a href=\"#%s\">%s</a>" source nm
      else
        Format.fprintf f "<a href=\"#%s\">%s (from %s)</a>" source nm source
    end
  in

  if !sources <> [] then begin
    Format.fprintf f "<ul>@.";
    List.iter
      (fun (nm, reasons) ->
         Format.fprintf f "<li><a id=\"%s\">%s</a>@." nm nm;
         Format.fprintf f "<ul>@.";
         let binaries = ref [] in
         List.iter
           (fun r ->
              if interesting_reason r then
              match r with
                Blocked ->
                  Format.fprintf f "<li>Left unchanged due to block request.@."
              | Too_young (cur_ag, req_ag) ->
                  Format.fprintf f
                    "<li>Only %d days old. Must be %d days old to go in.@."
                    cur_ag req_ag
              | Binary_not_propagated (bin, arch) ->
                  binaries := (bin, arch) :: !binaries
              | More_bugs ->
                  Format.fprintf f "<li>Has new bugs.@."
              | No_binary ->
                  Format.fprintf f "<li>No associated binary package.@."
              | _ ->
                  assert false)
           reasons;
         let binaries =
           sort_and_uniq (compare_pair compare compare) !binaries in
         let not_yet_built =
           group compare
             (List.filter
                (fun bin ->
                  List.exists
                    (fun r -> match r with Not_yet_built _ -> true | _ -> false)
                    (ListTbl.find unchanged_reasons bin))
                binaries)
         in
         List.iter
           (fun (nm', l) ->
              Format.fprintf f "<li>Binary package %s not yet built on %a.@."
                nm'
                (Util.print_list
                   (fun f arch -> Format.fprintf f "%s" arch) ", ")
                l)
           not_yet_built;
         let has_new_bugs =
           group compare
             (List.filter
                (fun bin ->
                  List.exists
                    (fun r -> match r with More_bugs -> true | _ -> false)
                    (ListTbl.find unchanged_reasons bin))
                binaries)
         in
         List.iter
           (fun (nm', _) ->
              if nm' <> nm then
                Format.fprintf f "<li>Binary package %s has new bugs.@." nm')
           has_new_bugs;
         let binaries =
           List.filter
             (fun bin ->
                List.exists
                  (fun r ->
                     interesting_reason r &&
                     match r with
                       Not_yet_built _ | More_bugs -> false
                     | _                           -> true)
                  (ListTbl.find unchanged_reasons bin))
                binaries
         in
         List.iter
           (fun (nm', arch) ->
              Format.fprintf f "<li>Binary package %s/%s not propagated:@."
                nm' arch;
             Format.fprintf f "<ul>@.";
             List.iter
               (fun r ->
                  if interesting_reason r then
                  match r with
                    Conflict (s, s') ->
                      begin match StringSet.cardinal s with
                        0 ->
                          Format.fprintf f "<li>Dependency not satisfied"
                      | 1 ->
                          Format.fprintf f "<li>Needs binary package %a"
                            print_binary (StringSet.choose s, arch)
                      | _ ->
                          Format.fprintf f "<li>Needs binary packages";
                          StringSet.iter
                            (fun nm ->
                               Format.fprintf f " %a" print_binary (nm, arch))
                            s
                      end;
                      if StringSet.cardinal s' = 1 then begin
                        if StringSet.mem nm' s' then
                          Format.fprintf f ".@."
                        else begin
                          Format.fprintf f
                            ": would break binary package %a.@."
                            print_binary (StringSet.choose s', arch)
                        end
                      end else begin
                        Format.fprintf f ": would break binary packages";
                        StringSet.iter
                          (fun nm ->
                             Format.fprintf f " %a" print_binary (nm, arch))
                          s';
                        Format.fprintf f ".@."
                      end
                  | More_bugs ->
                      ()
                  | Not_yet_built _ ->
                      ()
                  | _ ->
                      assert false)
               (ListTbl.find unchanged_reasons (nm', arch));
             Format.fprintf f "</ul>@.")
           binaries;
         Format.fprintf f "</ul>@.")
      (List.sort (fun (nm1, _) (nm2, _) -> compare nm1 nm2) !sources);
    Format.fprintf f "</ul>@."
  end;
  close_out ch;
  if debug_time () then
    Format.eprintf "Writing excuse file: %f@." (Timer.stop t)

(****)

let source_version src nm =
  try Some (Hashtbl.find src nm).M.s_version with Not_found -> None
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

let allow_smooth_updates p =
  List.mem "ALL" !smooth_updates || List.mem p.M.section !smooth_updates

(****)

let compute_conflicts t u =
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
  conflicts

let prepare_repository st unchanged =
  let t = st.testing in
  let u = st.unstable in
  let conflicts = compute_conflicts t u in

  let changed_packages = Hashtbl.create 101 in
  let consider_package _ p =
    let nm = p.M.package in
    if not (Hashtbl.mem unchanged nm) then
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
      if not (Hashtbl.mem unchanged p) then
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
  if debug_reduction () then Format.eprintf "==> %d/%d@." !n !m;
  st.upgrade_state <-
    Some (Upgrade_common.prepare_analyze t',
          Upgrade_common.prepare_analyze u')

let reduce_repository_pair = Task.funct prepare_repository

let reduce_repositories l =
  let t = Timer.start () in
  Task.iter l
    (fun (arch, st) -> reduce_repository_pair st (Hashtbl.find unchanged arch))
    (fun () -> ());
  if debug_time () then
    Format.eprintf "Reducing repositories: %f@." (Timer.stop t)

let rec get_upgrade_state st unchanged =
  match st.upgrade_state with
    Some st ->
      st
  | None ->
      prepare_repository st unchanged;
      get_upgrade_state st unchanged

(****)

let share_packages (t, u) =
  let unchanged dist p =
    match ListTbl.find dist.M.packages_by_name p.M.package with
      []  -> false
    | [q] -> M.compare_version p.M.version q.M.version = 0
    | _   -> assert false
  in
  let common = M.new_pool () in
  M.merge2 common (fun p -> unchanged u p) t;
  let t' = M.copy common in
  M.merge2 t' (fun p -> not (unchanged u p)) t;
  let u' = common in
  M.merge2 u' (fun p -> not (unchanged t p)) u;
  assert (t'.M.size = t.M.size && u'.M.size = u.M.size);
  (t', u')
 
let init_arch arch testing_srcs testing_bugs unstable_srcs unstable_bugs () =
  let files =
    [bin_package_file (testing ()) arch;
     bin_package_file (unstable ()) arch]
  in
  let cache = bin_package_file cache_dir arch in
  let ((t, u), uid) =
    cached files cache "version 1"
      (fun () ->
        share_packages
         (load_bin_packages (testing ()) arch,
          load_bin_packages (unstable ()) arch))
  in
(*
let n1 = ref 0 in
let n2 = ref 0 in
Hashtbl.iter (fun _ p ->
  incr n1;
if ListTbl.mem t.M.packages_by_name p.M.package then begin
  let q = List.hd (ListTbl.find t.M.packages_by_name p.M.package) in
  if M.compare_version p.M.version q.M.version = 0 then incr n2
end
) u.M.packages_by_num;
Format.eprintf "====> %d/%d" !n2 !n1;
*)
  { arch = arch;
    testing = t;
    testing_srcs = testing_srcs; testing_bugs = testing_bugs;
    unstable = u;
    unstable_srcs = unstable_srcs; unstable_bugs = unstable_bugs;
    upgrade_state = None;
    uid = uid }

type cstr =
    No_change of int * (string * string) * reason
  | Associates of (string * string) * (string * string) * reason
  | All_or_none of (string * string) list * reason

let arch_constraints st (produce_excuses, compute_hints) =
  let t = st.testing_srcs in
  let u = st.unstable_srcs in
  let t' = st.testing in
  let u' = st.unstable in
  let fake_srcs = ref [] in
  let is_fake = Hashtbl.create 17 in
  let sources_with_binaries = ref [] in
  let source_has_binaries = Hashtbl.create 8192 in
  let l = ref [] in
  let no_change pkg reason = l := No_change (0, pkg, reason) :: !l in
  let no_change_deferred pkg reason = l := No_change (1, pkg, reason) :: !l in
  let associates pkg1 pkg2 reason =
    l := Associates (pkg1, pkg2, reason):: !l
  in
  let all_or_none pkgl reason = l := All_or_none (pkgl, reason) :: !l in
  let get_bugs src bugs p =
    try Hashtbl.find bugs p with Not_found -> StringSet.empty
  in
  let no_new_bugs is_new p =
    if is_new then
      StringSet.is_empty (get_bugs u st.unstable_bugs p)
    else
      StringSet.subset
        (get_bugs u st.unstable_bugs p)
        (get_bugs t st.testing_bugs p)
  in
  let arch = st.arch in
  let bin_nmus = ListTbl.create 101 in
  Hashtbl.iter
    (fun _ p ->
       let pkg = (p.M.package, arch) in
       let (nm, v) = p.M.source in
       (* Faux packages *)
       if not (Hashtbl.mem u nm) then begin
         Hashtbl.add u nm
           { M.s_name = nm; s_version = v; s_section = "" };
         fake_srcs := (nm, v) :: !fake_srcs;
         Hashtbl.add is_fake nm ();
         no_change (nm, "source") Unchanged
       end;
       let v' = (Hashtbl.find u nm).M.s_version in
       (* Do not add a binary package if its source is not
          the most up to date source file. *)
       if M.compare_version v v' <> 0 then
         no_change pkg (Not_yet_built (nm, v, v'))
       else if not (Hashtbl.mem source_has_binaries nm) then begin
         sources_with_binaries := nm :: !sources_with_binaries;
         Hashtbl.add source_has_binaries nm ()
       end;
       let source_changed =
         not (same_source_version t u (fst p.M.source)) in
       (* We only propagate binary packages with a larger version.
          Faux packages are not propagated. *)
       if no_new_bin t' u' p.M.package || Hashtbl.mem is_fake nm then
         no_change pkg Unchanged
       else begin
         (* Do not upgrade a package if it has new bugs *)
         let is_new = bin_version t' p.M.package = None in
         if not (no_new_bugs is_new p.M.package) then
           no_change_deferred pkg More_bugs;
         if source_changed then
           (* We cannot add a binary package without also adding
              its source. *)
           associates pkg (fst p.M.source, "source")
             (Source_not_propagated p.M.source)
         else
           ListTbl.add bin_nmus p.M.source pkg;

       end;
       (* If a source is propagated, all its binaries should
          be propagated as well *)
       if source_changed || produce_excuses then
         associates (fst p.M.source, "source") pkg
           (Binary_not_propagated pkg))
    u'.M.packages_by_num;
  (* All binaries packages from a same source are propagated
     atomically on any given architecture. *)
  ListTbl.iter
    (fun _ pkgs -> all_or_none pkgs (Atomic pkgs)) bin_nmus;
  Hashtbl.iter
    (fun _ p ->
       let pkg = (p.M.package, arch) in
       let (nm, v) = p.M.source in
       (* Faux packages *)
       if not (Hashtbl.mem t nm) then begin
         (* The source should be fake in unstable as well. *)
         assert (not (Hashtbl.mem u nm) || Hashtbl.mem is_fake nm);
         Hashtbl.add t nm
           { M.s_name = nm; s_version = v; s_section = "" };
         fake_srcs := (nm, v) :: !fake_srcs;
         Hashtbl.add is_fake nm ();
         no_change (nm, "source") Unchanged
       end;
       let v' = (Hashtbl.find t nm).M.s_version in
       let source_changed =
         not (same_source_version t u (fst p.M.source)) in
       (* We only propagate binary packages with a larger version.
          Faux packages are not propagated. *)
       if no_new_bin t' u' p.M.package || Hashtbl.mem is_fake nm then
         no_change pkg Unchanged
       else begin
         (* Binary packages without source of the same version can
            be removed freely when not needed anymore (these are
            binaries left for smooth update).
            However, when producing hints, we do not allow this, as
            we have no way to communicate the change to britney... *)
         if not compute_hints && M.compare_version v v' <> 0 then
           ()
         (* We cannot remove a binary without removing its source. *)
         else if source_changed then
           associates pkg (fst p.M.source, "source")
             (Source_not_propagated p.M.source)
         else
           ListTbl.add bin_nmus p.M.source pkg
       end;
       (* We cannot remove or upgrade a source package if a
          corresponding binary package still exists.
          We relax this constraint for libraries when upgrading
          a source package. *)
       if
         (source_changed || produce_excuses)
           &&
         not (allow_smooth_updates p && Hashtbl.mem u nm)
       then
         associates (fst p.M.source, "source") pkg
           (Binary_not_propagated pkg))
    t'.M.packages_by_num;
  (List.rev !l, st.uid, !sources_with_binaries, !fake_srcs)

let arch_constraints = Task.funct arch_constraints

(****)

let find_coinst_constraints st unchanged =
  let arch = st.arch in
  let (t', u') = get_upgrade_state st unchanged in
  let changes = ref [] in
  if debug_coinst () then
    Format.eprintf "==================== %s@." arch;
  let step_t = Timer.start () in
  let problems =
    Upgrade_common.find_problematic_packages
      ~check_new_packages:true t' u'
      (fun nm -> Hashtbl.mem unchanged nm)
  in
  let t = Timer.start () in
  let has_singletons =
    List.exists
      (fun (cl, _) -> StringSet.cardinal cl.Upgrade_common.pos = 1)
      problems
  in
  List.iter
    (fun ({Upgrade_common.pos = pos;  neg = neg}, s) ->
       let n = StringSet.cardinal pos in
       if not (has_singletons && n > 1) then begin
         let nm = StringSet.choose pos in
         let can_learn = n = 1 && StringSet.cardinal neg <= 1 in
         changes := (nm, arch, neg, s, can_learn) :: !changes
       end)
    problems;
  if debug_time () then begin
    Format.eprintf "  New constraints: %f@." (Timer.stop t);
    Format.eprintf "Step duration: %f@." (Timer.stop step_t)
  end;
  List.rev !changes

let find_coinst_constraints = Task.funct find_coinst_constraints

let find_all_coinst_constraints a =
  let t = Timer.start () in
  let c = Array.length a in
  let running = Array.make c false in
  let changed = Array.make c true in
  let n = ref 0 in
  let max_proc = Task.get_processor_count () in
  let scheduler = Task.scheduler () in
  let rec start c i0 i =
    if running.(i) || not changed.(i) then begin
      let i = (i + 1) mod c in
      if i <> i0 then start c i0 i
    end else begin
      changed.(i) <- false;
      running.(i) <- true;
      incr n;
      let (arch, st) = a.(i) in
      Task.async scheduler
        (find_coinst_constraints st (Hashtbl.find unchanged arch))
        (fun changes -> stop c i changes);
      if !n < max_proc then begin
        start c 0 0
      end
    end
  and stop c i changes =
    if changes <> [] then begin
      Array.fill changed 0 c true;
      List.iter
        (fun (nm, arch, neg, s, can_learn) ->
           if can_learn then learn_rule nm arch neg s;
           no_change (nm, arch) (Conflict (neg, s)))
        changes
    end;
    running.(i) <- false;
    decr n;
    start c i i
  in
  start 1 0 0;
  Task.run scheduler;
  start c 0 0;
  Task.run scheduler;
  if debug_time () then
    Format.eprintf "Solving constraints: %f@." (Timer.stop t)

(****)

let arch_change st unchanged =
  let arch = st.arch in
  let t' = st.testing in
  let u' = st.unstable in
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       let v = p.M.version in
       if not (Hashtbl.mem unchanged nm) then
         match bin_version u' nm with
           Some v' ->
             Format.eprintf
               "Upgrade binary package %s/%s from %a to %a@."
               nm arch M.print_version v M.print_version v'
         | None ->
             Format.eprintf "Remove binary package %s/%s@." nm arch)
    t'.M.packages_by_num;
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       if not (Hashtbl.mem unchanged nm) then
         if not (ListTbl.mem t'.M.packages_by_name nm) then
           Format.eprintf "Adding binary package %s/%s@." nm arch)
    u'.M.packages_by_num

let arch_change = Task.funct arch_change

(****)

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

let merge t t' f =
  let t = repr t in
  let t' = repr t' in
  if t != t' then begin
    t.state <- Value (f (get t) (get t'));
    t'.state <- Link t
  end

let elt v = { state = Value v }

end

let cluster_packages st (unchanged, clusters) =
  let clusters =
    List.map (fun (lst, id) -> (lst, (id, Union_find.elt id))) clusters
  in
  let merge (_, e1) (_, e2) = Union_find.merge e1 e2 min in
  let (t, u) =
    match st.upgrade_state with Some st -> st | None -> assert false in
  Upgrade_common.find_clusters t u
    (fun nm -> Hashtbl.mem unchanged nm) clusters merge;
  List.map (fun (_, (id, elt)) -> (id, Union_find.get elt)) clusters

let cluster_packages = Task.funct cluster_packages

type 'a easy_hint =
  { mutable h_names : 'a list;
    mutable h_pkgs : (string * string) list;
    mutable h_live : bool;
    h_id : int }

let generate_small_hints l buckets =
  let to_consider = ref [] in
  let buckets_by_id = Hashtbl.create 17 in
  let n = ref 0 in
  ListTbl.iter
    (fun (src, arch) lst ->
       let info =
         { h_names = [(src, arch)]; h_pkgs = lst; h_live = true; h_id = !n } in
       let elt = Union_find.elt info in
       Hashtbl.add buckets_by_id !n elt;
       incr n;
       to_consider := (info, elt) :: !to_consider)
    buckets;

  let merge elt elt' =
    Union_find.merge elt elt'
      (fun info info' ->
         assert (info.h_live);
         assert (info'.h_live);
         info.h_names <- info'.h_names @ info.h_names;
         info.h_pkgs <- info'.h_pkgs @ info.h_pkgs;
         info'.h_live <- false;
         info)
  in

  Task.iter l
    (fun (arch, st) ->
       let unchanged' = Hashtbl.find unchanged arch in
       let clusters = ref [] in
       List.iter
         (fun (info, elt) ->
            let l =
              List.filter (fun (_, arch') -> arch = arch') info.h_pkgs in
            if l <> [] then
              clusters :=
                (List.map fst l, (Union_find.get elt).h_id) :: !clusters)
         !to_consider;
       cluster_packages st (unchanged', !clusters))
    (fun lst ->
       List.iter
         (fun (id, id') ->
            merge (Hashtbl.find buckets_by_id id)
                  (Hashtbl.find buckets_by_id id'))
         lst);

  let lst =
    List.filter (fun info -> info.h_live) (List.map fst !to_consider) in
  let lst = List.map (fun info -> info.h_names) lst in
  let compare_elt = compare_pair compare compare in
  let rec compare_lst l1 l2 =
    match l1, l2 with
      [], [] ->
        0
    | [], _ ->
        -1
    | _, [] ->
        1
    | v1 :: r1, v2 :: r2 ->
        let c = compare_elt v1 v2 in if c = 0 then compare_lst r1 r2 else c
  in
  let lst = List.map (fun names -> List.sort compare_elt names) lst in
  let lst = List.sort compare_lst lst in
  List.stable_sort (fun l l' -> compare (List.length l) (List.length l')) lst

let collect_changes st unchanged =
  let changes = ref [] in
  let u' = st.unstable in
  let t' = st.testing in
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       if not (Hashtbl.mem unchanged nm) then begin
         let (src, v) = p.M.source in
         changes := (src, nm) :: !changes
       end)
    u'.M.packages_by_num;
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       if
         not (Hashtbl.mem unchanged nm)
           &&
         not (ListTbl.mem u'.M.packages_by_name nm)
       then begin
         let (src, v) = p.M.source in
         changes := (src, nm) :: !changes
       end)
    t'.M.packages_by_num;
  List.rev !changes

let collect_changes = Task.funct collect_changes

let generate_hints t u l =
  let hint_t = Timer.start () in
  let changes = ListTbl.create 101 in
  Task.iteri l
    (fun (arch, st) ->
       (arch, collect_changes st (Hashtbl.find unchanged arch)))
    (fun arch lst ->
       List.iter (fun (src, nm) -> ListTbl.add changes src (nm, arch)) lst);
  let buckets = ListTbl.create 101 in
  let unchanged' = Hashtbl.find unchanged "source" in
  ListTbl.iter
    (fun src l ->
       if not (Hashtbl.mem unchanged' src) then
         List.iter
           (fun info -> ListTbl.add buckets (src, "source") info)
           l
       else
         List.iter
           (fun ((_, arch) as info) ->
              ListTbl.add buckets (src, arch) info)
           l)
    changes;
  let hints = generate_small_hints l buckets in
  if debug_time () then
    Format.eprintf "Generating hints: %f@." (Timer.stop hint_t);
  let print_pkg f src arch =
    try
      let vers = (Hashtbl.find u src).M.s_version in
      if arch = "source" then begin
        (* We are propagating a source package. *)
        Format.fprintf f " %s/%a" src M.print_version vers
      end else begin
        (* We are changing some binaries. *)
        Format.fprintf f " %s/%s/%a" src arch M.print_version vers
      end
    with Not_found ->
      (* We are removing a source package. *)
      assert (arch = "source");
      let vers = (Hashtbl.find t src).M.s_version in
      Format.fprintf f " -%s/%a" src M.print_version vers
  in
  let print_hint f l =
    if !all_hints || List.length l > 1 then begin
      Format.fprintf f "easy";
      List.iter (fun (src, arch) -> print_pkg f src arch) l;
      Format.fprintf f "@."
    end
  in
  let print_hints f = List.iter (fun names -> print_hint f names) hints in
  if debug_hints () then print_hints Format.std_formatter;
  if !hint_file <> "" then begin
    let ch = open_out !hint_file in
    print_hints (Format.formatter_of_out_channel ch);
    close_out ch
  end

(****)

let heidi_buffer = Buffer.create 80

let heidi_line lines nm vers arch sect =
  Format.bprintf heidi_buffer "%s %a %s %s@."
    nm M.print_version vers arch sect;
  lines := Buffer.contents heidi_buffer :: !lines;
  Buffer.clear heidi_buffer

let heidi_arch st unchanged =
  let lines = ref [] in
  let arch = st.arch in
  let t = st.testing in
  let u = st.unstable in
  let is_preserved nm = Hashtbl.mem unchanged nm in
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       let sect = if p.M.section = "" then "faux" else p.M.section in
       if is_preserved nm then heidi_line lines nm p.M.version arch sect)
    t.M.packages_by_num;
  Hashtbl.iter
    (fun _ p ->
       let nm = p.M.package in
       let sect = if p.M.section = "" then "faux" else p.M.section in
       if not (is_preserved nm) then
         heidi_line lines nm p.M.version arch sect)
    u.M.packages_by_num;
  String.concat "" (List.sort compare !lines)

let heidi_arch = Task.funct heidi_arch

let print_heidi ch fake_src l t u =
  let heidi_t = Timer.start () in
  let lines = ref [] in
  Task.iter (List.sort (fun (arch, _) (arch', _) -> compare arch arch') l)
    (fun (arch, st) -> heidi_arch st (Hashtbl.find unchanged arch))
    (fun lines -> output_string ch lines);
  let unchanged' = Hashtbl.find unchanged "source" in
  let is_preserved nm = Hashtbl.mem unchanged' nm in
  let source_sect nm s =
    if Hashtbl.mem fake_src nm then "faux"
    else if s.M.s_section = "" then "unknown"
    else s.M.s_section
  in
  Hashtbl.iter
    (fun nm s ->
       let sect = source_sect nm s in
       if is_preserved nm then
         heidi_line lines nm s.M.s_version "source" sect)
    t;
  Hashtbl.iter
    (fun nm s ->
       let sect = source_sect nm s in
       if not (is_preserved nm) then
         heidi_line lines nm s.M.s_version "source" sect)
    u;
  List.iter (output_string ch) (List.sort compare !lines);
  if debug_time () then
    Format.eprintf "Writing Heidi file: %f@." (Timer.stop heidi_t)

let f () =
  Util.enable_messages false;
  let load_t = Timer.start () in
  let (testing_bugs, unstable_bugs) = read_bugs () in
  let files =
    [src_package_file (testing ()); src_package_file (unstable ())] in
  let cache = Filename.concat cache_dir "Sources" in
  let ((t, u), _) =
    cached files cache "version 2" (fun () ->
      (load_src_packages (testing ()), load_src_packages (unstable ())))
  in
  if debug_time () then
    Format.eprintf "  Loading shared data: %f@." (Timer.stop load_t);
  let l =
    List.map
      (fun arch ->
         (arch, Task.spawn (init_arch arch t testing_bugs u unstable_bugs)))
      !archs
  in
  let (dates, urgencies, hints) = read_extra_info () in

  if debug_time () then Format.eprintf "Loading: %f@." (Timer.stop load_t);

  let init_t = Timer.start () in
  init_changes ();
  let compute_hints = debug_hints () || !hint_file <> "" in
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
  let deferred_constraints = ref [] in
  let produce_excuses = !excuse_file <> "" in
  let no_change_deferred pkg reason =
    if produce_excuses then
      deferred_constraints := (pkg, reason) :: !deferred_constraints
    else
      no_change pkg reason
  in
  let perform_deferred () =
    List.iter
      (fun (pkg, reason) -> no_change pkg reason) !deferred_constraints;
    deferred_constraints := []
  in
  let arch_results =
    List.map
      (fun (_, t) -> arch_constraints t (produce_excuses, compute_hints)) l
  in
  Hashtbl.iter
    (fun nm s ->
       let v = s.M.s_version in
       (* We only propagate source packages with a larger version *)
       if no_new_source t u nm then
         no_change (nm, "source") Unchanged
       else if is_blocked nm then
         no_change_deferred (nm, "source") Blocked
       else begin
         (* Do not propagate a source package if not old enough *)
         let v' = source_version t nm in
         let (cur_ag, req_ag) = compute_ages nm v v' in
         if cur_ag < req_ag then
           no_change_deferred (nm, "source") (Too_young (cur_ag, req_ag));
         (* Do not propagate a source package if it has new bugs *)
         let is_new = v' = None in
         if
           not (no_new_bugs is_new nm && no_new_bugs is_new ("src:" ^ nm))
         then
           no_change_deferred (nm, "source") More_bugs
       end)
    u;
  let source_has_binaries = Hashtbl.create 8192 in
  let is_fake = Hashtbl.create 17 in
  let uids =
    List.map
      (fun r ->
         let (l, uid, sources_with_binaries, fake_srcs) = Task.wait r in
         List.iter
           (fun nm ->
              if not (Hashtbl.mem source_has_binaries nm) then
                Hashtbl.add source_has_binaries nm ())
           sources_with_binaries;
         List.iter
           (fun (nm, v) ->
              if not (Hashtbl.mem is_fake nm) then begin
                Hashtbl.add is_fake nm ();
                Hashtbl.add t nm
                  { M.s_name = nm; s_version = v; s_section = "" };
              end)
           fake_srcs;
         List.iter
           (fun c ->
              match c with
                No_change (0, pkg, reason) ->
                  no_change pkg reason
              | No_change (_, pkg, reason) ->
                  no_change_deferred pkg reason
              | Associates (pkg1, pkg2, reason) ->
                  associates pkg1 pkg2 reason
              | All_or_none (pkgl, reason) ->
                  all_or_none pkgl reason)
           l;
         uid)
      arch_results
  in
  Hashtbl.iter
    (fun nm s ->
       if not (Hashtbl.mem source_has_binaries nm) then
         no_change (nm, "source") No_binary)
    u;
  load_rules uids;
  if debug_time () then
    Format.eprintf "Initial constraints: %f@." (Timer.stop init_t);

  reduce_repositories l;

  find_all_coinst_constraints (Array.of_list l);
  if !deferred_constraints <> [] then begin
    perform_deferred ();
    find_all_coinst_constraints (Array.of_list l)
  end;

  save_rules uids;

  if debug_outcome () then begin
    let unchanged_src = Hashtbl.find unchanged "source" in
    Hashtbl.iter
      (fun nm s ->
         if not (Hashtbl.mem unchanged_src nm) then
           try
             let s' = Hashtbl.find u nm in
             Format.eprintf "Upgrade source package %s from %a to %a@." nm
               M.print_version s.M.s_version M.print_version s'.M.s_version
           with Not_found ->
             Format.eprintf "Remove source package %s@." nm)
      t;
    Hashtbl.iter
      (fun nm v ->
         if not (Hashtbl.mem t nm || Hashtbl.mem unchanged_src nm) then
           Format.eprintf "Adding source package %s@." nm)
      u;

    List.iter
      (fun (arch, st) ->
         Task.wait (arch_change st (Hashtbl.find unchanged arch)))
      l
  end;

  if compute_hints then generate_hints t u l;

  if !heidi_file <> "" then begin
    let ch = open_out !heidi_file in
    print_heidi ch is_fake l t u;
    close_out ch
  end;

  if !excuse_file <> "" then output_reasons l !excuse_file;

  List.iter (fun (_, t) -> Task.kill t) l

(****)

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
  archs := (try Hashtbl.find options "ARCHITECTURES" with Not_found -> !archs);
  smooth_updates :=
    (try
       Hashtbl.find options "SMOOTH_UPDATES"
     with Not_found ->
       !smooth_updates)

let _ =
let spec =
  Arg.align
  ["--input",
   Arg.String (fun d -> dir := d),
   "DIR Select directory containing britney data";
   "-c",
   Arg.String read_conf,
   "FILE Read britney config FILE";
   "--hints",
   Arg.String (fun f -> hint_file := f),
   "FILE Output hints to FILE";
   "--all-hints",
   Arg.Unit (fun () -> all_hints := true),
   " Show all hints (including single package ones)";
   "--heidi",
   Arg.String (fun f -> heidi_file := f),
   "FILE Output Heidi results to FILE";
   "--excuses",
   Arg.String (fun f -> excuse_file := f),
   "FILE Output excuses to FILE";
   "--offset",
   Arg.Int (fun n -> offset := n),
   "N Move N days into the future";
   "--no-cache",
   Arg.Unit (fun () -> cache_disabled := true),
   " Disable on-disk caching";
   "--proc",
   Arg.Int Task.set_processor_count,
   "N Provide number of processors (use 1 to disable concurrency)";
   "--debug",
   Arg.String Debug.set,
   "NAME Activate debug option NAME";
   "--control-files",
   Arg.Unit (fun () -> ()),
   " Currently ignored";
   "--auto-hinter",
   Arg.Unit (fun () -> ()),
   " Currently ignored";
   "-v",
   Arg.Unit (fun () -> ()),
   " Currently ignored";
   "--compatible",
   Arg.Unit (fun () -> ()),
   " Currently ignored"]
in
Arg.parse spec (fun p -> ())
  ("Usage: " ^ Sys.argv.(0) ^ " OPTIONS\n\
    Computes which packages can migrate from sid to testing.\n\
    Takes as input either a britney data directory (option -input)\n\
    or a britney config file (option -c).\n\
    \n\
    Options:");
  f ()

(*
let _ = Format.eprintf ">>>> %f@." (Unix.times ()).Unix.tms_utime
*)

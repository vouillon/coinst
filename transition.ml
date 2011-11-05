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

Printexc.record_backtrace true;;

(*
PRIORITIES
==> save learnt clauses
==> graphs for reporting co-installability issues
==> explain what it takes to migrate a package
==> find a way to indicate what corresponds to each new
    co-installability issue
    ==> if I upgrade, these packages will be broken
        if I don't, these other packages will be broken (?)
==> how do we ignore co-installability issues?
==> allow package removal
==> indicate which packages would be propagated by britney but not by
    this tool


  Generate explanations
  ==> link to build logs / merge packages
  ==> link to http://packages.qa.debian.org (for source files)
  ==> link to bugs
  ==> show source and binary versions
  ==> three step: no age/bug constraints, bugs added, all
  ==> show packages that only wait for age, for bugs

  Find what it takes to install a package
  ==> iterative: relax the problem until we can install the package
  ==> with clause learning
  ==> we could save learnt rules...

  - parallelise the program: one process per architecture

  - associate a nice graph to each co-installability issue found
*)

let dir = ref (Filename.concat (Sys.getenv "HOME") "debian-dists/britney")
let archs = ref ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let smooth_updates = ref ["libs"; "oldlibs"]
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
let excuse_file = ref ""
let offset = ref 0
let small_hints = ref false

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

let cached files cache magic f =
  let magic = Format.sprintf "%s\n%s\n\n" magic (String.concat "\n" files) in
  let ch = try Some (open_in cache) with Sys_error _ -> None in
  let should_compute =
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
    let tmp = cache ^ ".tmp" in
    Util.make_directories tmp;
    let ch = open_out tmp in
    output_string ch magic;
    Marshal.to_channel ch res [];
    close_out ch;
    Sys.rename tmp cache;
    res
  end else begin
    match ch with
      Some ch ->
        let res = Marshal.from_channel ch in
        close_in ch;
        res
    | None ->
        assert false
  end

module StringSet = Upgrade_common.StringSet

let _ =
Gc.set { (Gc.get ())
         with Gc.space_overhead = 200; max_overhead = 1000000;
              major_heap_increment = 20 * 1024 * 1024 }

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
  let cache = Filename.concat (Sys.getenv "HOME") ".coinst/Urgencies" in
  cached [file] cache "version 1"
    (fun () -> read_package_info file urgency_delay)

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
       let file = Filename.concat dir f in
       if Sys.is_directory file || f = "README" || f = "index.html" || f.[0] = '.' then
         ()
       else
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
               if debug_read_hints () then
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
       if Sys.is_directory file then
         ()
       else
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
       if Sys.is_directory file then
         ()
       else
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
  | No_binary
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
  | Binary_not_propagated ((bin, v), arch) ->
      Format.fprintf f "binary package %s (%a / %s) cannot be propagated"
        bin M.print_version v arch
  | No_binary ->
      Format.fprintf f "no associated binary package"
  | Atomic l ->
      Format.fprintf f "binary packages";
      List.iter
        (fun ((src, v), arch) ->
           Format.fprintf f " %s (%a / %s)" src M.print_version v arch)
        l;
      Format.fprintf f " cannot be propagated all at once"

let unchanged = ListTbl.create 101

let propagation_rules = Hashtbl.create 101

let rec no_change pkg reason =
  let ((nm, version), arch) = pkg in
  let already = ListTbl.mem unchanged (nm, arch) in
  ListTbl.add unchanged (nm, arch) reason;
  if not already then begin
    if reason <> Unchanged && verbose () then
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
  let c = compare a1 b1 in
  if c = 0 then compare a2 b2 else c

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

let rec interesting_reason r =
  match r with
    Unchanged ->
      false
  | Binary_not_propagated ((bin, _), arch) ->
      List.exists interesting_reason (ListTbl.find unchanged (bin, arch))
  | Source_not_propagated _ ->
      false
  | Atomic _ ->
      false
  | _ ->
      true

let output_reasons l filename =
  let ch = open_out filename in
  let f = Format.formatter_of_out_channel ch in

  let sources = ref [] in
  ListTbl.iter
    (fun (nm, arch) reasons ->
       if arch = "source" && List.exists interesting_reason reasons then
         sources := (nm, reasons) :: !sources)
    unchanged;

  let print_binary f (nm, (arch : string)) =
    let (t, u) = List.assoc arch l in
    let source =
      match ListTbl.find u.M.packages_by_name nm with
        p :: _ ->
          fst p.M.source
      | [] ->
          match ListTbl.find t.M.packages_by_name nm with
            p :: _ -> fst p.M.source
          | []     -> assert false
    in
    if not (List.mem_assoc source !sources) then begin
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
              | Binary_not_propagated ((bin, v), arch) ->
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
                    (ListTbl.find unchanged bin))
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
                    (ListTbl.find unchanged bin))
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
                  (ListTbl.find unchanged bin))
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
               (ListTbl.find unchanged (nm', arch));
             Format.fprintf f "</ul>@.")
           binaries;
         Format.fprintf f "</ul>@.")
      (List.sort (fun (nm1, _) (nm2, _) -> compare nm1 nm2) !sources);
    Format.fprintf f "</ul>@."
  end;
  close_out ch

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

let reduce_repository_pair (arch, (t, u)) =
  let conflicts = compute_conflicts t u in

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
  if debug_reduction () then Format.eprintf "==> %d/%d@." !n !m;
  (arch, (t', u'))

let reduce_repositories l =
  let t = Timer.start () in
  let l = List.map reduce_repository_pair l in
  if debug_time () then
    Format.eprintf "Reducing repositories: %f@." (Timer.stop t);
  l

(****)

type 'a easy_hint =
  { mutable h_names : 'a list;
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

let generate_small_hints l buckets =
  let to_consider = ref [] in
  ListTbl.iter
    (fun (src, arch) lst ->
       let info =
         { h_names = [(src, (arch, lst))]; h_pkgs = List.map fst lst;
           h_live = true }
       in
       let elt = Union_find.elt info in
       to_consider := (info, elt) :: !to_consider)
    buckets;

  let merge elt elt' =
    if Union_find.repr elt != Union_find.repr elt' then begin
      let info = Union_find.get elt in
      let info' = Union_find.get elt' in
      assert (info.h_live);
      assert (info'.h_live);
      Union_find.merge elt elt';
      info.h_names <- info'.h_names @ info.h_names;
      info.h_pkgs <- info'.h_pkgs @ info.h_pkgs;
      info'.h_live <- false
    end
  in

  List.iter
    (fun (arch, t, u) ->
       let clusters = ref [] in
       List.iter
         (fun (info, elt) ->
            let l = List.filter (fun (_, arch') -> arch = arch') info.h_pkgs in
            if l <> [] then
              clusters := (List.map fst l, elt) :: !clusters)
         !to_consider;
       Upgrade_common.find_clusters t u
         (fun nm -> ListTbl.mem unchanged (nm, arch)) !clusters merge)
    l;

  let lst =
    List.filter (fun info -> info.h_live) (List.map fst !to_consider) in
  let lst = List.map (fun info -> info.h_names) lst in
  let compare_elt = compare_pair compare (compare_pair compare compare) in
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
  let lst =
    List.stable_sort
      (fun l l' -> compare (List.length l) (List.length l')) lst
  in
  lst

let generate_hints t u l l' =
  let changes = ListTbl.create 101 in
  List.iter
    (fun (arch, (t', u')) ->
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if not (ListTbl.mem unchanged (nm, arch)) then begin
              let (src, v) = p.M.source in
              ListTbl.add changes src ((nm, arch), `Propagate)
            end)
         u'.M.packages_by_num;
       Hashtbl.iter
         (fun _ p ->
            let nm = p.M.package in
            if
              not (ListTbl.mem unchanged (nm, arch))
                &&
              not (ListTbl.mem u'.M.packages_by_name nm)
            then begin
              let (src, v) = p.M.source in
              let explicit =
                allow_smooth_updates p ||
                M.compare_version v (Hashtbl.find t src).M.s_version <> 0
              in
              ListTbl.add changes src
                ((nm, arch), `Remove (p.M.version, explicit))
            end)
         t'.M.packages_by_num)
    l;
  let buckets = ListTbl.create 101 in
  ListTbl.iter
    (fun src l ->
       if not (ListTbl.mem unchanged (src, "source")) then
         List.iter
           (fun info -> ListTbl.add buckets (src, "source") info)
           l
       else
         List.iter
           (fun (((_, arch), _) as info) ->
              ListTbl.add buckets (src, arch) info)
           l)
    changes;
  let hints =
    if !small_hints then
      generate_small_hints l' buckets
    else
      []
  in
  let print_pkg f src arch lst =
    try
      let vers = (Hashtbl.find u src).M.s_version in
      if arch = "source" then begin
        (* We are propagating a source package. *)
        Format.fprintf f " %s/%a" src M.print_version vers;
        (* Explicitly remove unneeded packages left over due to smooth
           upgrade. *)
        List.iter
          (fun ((nm, arch), action) ->
             match action with
               `Propagate ->
                 ()
             | `Remove (vers, explicit) ->
                 if explicit then
                   Format.fprintf f " -%s/%s/%a" nm arch M.print_version vers)
          lst
      end else begin
        (* We are changing some binaries. *)
        if List.exists (fun (_, action) -> action = `Propagate) lst then
          Format.fprintf f " %s/%s/%a" src arch M.print_version vers;
        List.iter
          (fun ((nm, arch), action) ->
             match action with
               `Propagate ->
                 ()
             | `Remove (vers, explicit) ->
                 assert explicit;
                 Format.fprintf f " -%s/%s/%a" nm arch M.print_version vers)
          lst
      end
    with Not_found ->
      (* We are removing a source package. *)
      assert (arch = "source");
      let vers = (Hashtbl.find t src).M.s_version in
      Format.fprintf f " -%s/%a" src M.print_version vers
  in
  let print_hint f l =
    Format.fprintf f "easy";
    List.iter (fun (src, (arch, lst)) -> print_pkg f src arch lst) l;
    Format.fprintf f "@."
  in
  let print_hints f =
    if not !small_hints then begin
      Format.fprintf f "easy";
      ListTbl.iter (fun (src, arch) lst -> print_pkg f src arch lst) buckets;
      Format.fprintf f "@."
    end else
      List.iter (fun names -> print_hint f names) hints
  in
  if debug_hints () then print_hints Format.std_formatter;
  if !hint_file <> "-" then begin
    let ch = open_out !hint_file in
    print_hints (Format.formatter_of_out_channel ch);
    close_out ch
  end

(****)

let f () =
  let load_t = Timer.start () in
  let (dates, urgencies, testing_bugs, unstable_bugs, hints) =
    read_extra_info () in
  let files =
    List.flatten
      (List.map
         (fun arch ->
            bin_package_files (testing ()) arch @
            bin_package_files (unstable ()) arch)
         !archs)
  in
  let cache = Filename.concat (Sys.getenv "HOME") ".coinst/Packages" in
  let l =
    cached files cache "version 2" (fun () ->
    List.map
      (fun arch ->
         (arch,
          (load_bin_packages (testing ()) arch,
           load_bin_packages (unstable ()) arch)))
      !archs)
  in
  let files =
    src_package_files (testing ()) @ src_package_files (unstable ()) in
  let cache = Filename.concat (Sys.getenv "HOME") ".coinst/Sources" in
  let (t, u) =
    cached files cache "version 2" (fun () ->
      (load_src_packages (testing ()), load_src_packages (unstable ())))
  in
  if debug_time () then Format.eprintf "Loading: %f@." (Timer.stop load_t);

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
  Hashtbl.iter
    (fun nm s ->
       let v = s.M.s_version in
       (* We only propagate source packages with a larger version *)
       if no_new_source t u nm then
         no_change ((nm, v), "source") Unchanged
       else if is_blocked nm then
         no_change_deferred ((nm, v), "source") Blocked
       else begin
         (* Do not propagate a source package if not old enough *)
         let v' = source_version t nm in
         let (cur_ag, req_ag) = compute_ages nm v v' in
         if cur_ag < req_ag then
           no_change_deferred ((nm, v), "source") (Too_young (cur_ag, req_ag));
         (* Do not propagate a source package if it has new bugs *)
         let is_new = v' = None in
         if
           not (no_new_bugs is_new nm && no_new_bugs is_new ("src:" ^ nm))
         then
           no_change_deferred ((nm, v), "source") More_bugs
       end)
    u;
  let fake_src = Hashtbl.create 17 in
  let sources_with_binaries = Hashtbl.create 16384 in
  List.iter
    (fun (arch, (t', u')) ->
       let bin_nmus = ListTbl.create 101 in
       Hashtbl.iter
         (fun _ p ->
            let pkg = ((p.M.package, p.M.version), arch) in
            let (nm, v) = p.M.source in
            (* Faux packages *)
            if not (Hashtbl.mem u nm) then begin
              Hashtbl.add u nm
                { M.s_name = nm; s_version = v; s_section = "" };
              Hashtbl.add fake_src nm ();
              no_change ((nm, v), "source") Unchanged
            end;
            let v' = (Hashtbl.find u nm).M.s_version in
            (* Do not add a binary package if its source is not
               the most up to date source file. *)
            if M.compare_version v v' <> 0 then
              no_change pkg (Not_yet_built (nm, v, v'))
            else
              Hashtbl.replace sources_with_binaries nm ();
            let source_changed =
              not (same_source_version t u (fst p.M.source)) in
            (* We only propagate binary packages with a larger version.
               Faux packages are not propagated. *)
            if no_new_bin t' u' p.M.package || Hashtbl.mem fake_src nm then
              no_change pkg Unchanged
            else begin
              (* Do not upgrade a package if it has new bugs *)
              let is_new = bin_version t' p.M.package = None in
              if not (no_new_bugs is_new p.M.package) then
                no_change_deferred pkg More_bugs;
              if source_changed then
                (* We cannot add a binary package without also adding
                   its source. *)
                associates pkg (p.M.source, "source")
                  (Source_not_propagated p.M.source)
              else
                ListTbl.add bin_nmus p.M.source pkg;

            end;
            (* If a source is propagated, all its binaries should
               be propagated as well *)
            if source_changed || produce_excuses then
              associates (p.M.source, "source") pkg
                (Binary_not_propagated pkg))
         u'.M.packages_by_num;
       (* All binaries packages from a same source are propagated
          atomically on any given architecture. *)
       ListTbl.iter
         (fun _ pkgs -> all_or_none pkgs (Atomic pkgs)) bin_nmus;
       Hashtbl.iter
         (fun _ p ->
            let pkg = ((p.M.package, p.M.version), arch) in
            let (nm, v) = p.M.source in
            (* Faux packages *)
            if not (Hashtbl.mem t nm) then begin
              (* The source should be fake in unstable as well. *)
              assert (not (Hashtbl.mem u nm) || Hashtbl.mem fake_src nm);
              Hashtbl.add t nm
                { M.s_name = nm; s_version = v; s_section = "" };
              Hashtbl.add fake_src nm ();
              no_change ((nm, v), "source") Unchanged
            end;
            let v' = (Hashtbl.find t nm).M.s_version in
            let source_changed =
              not (same_source_version t u (fst p.M.source)) in
            (* Faux packages are not propagated. *)
            if Hashtbl.mem fake_src nm then
              no_change pkg Unchanged
            (* Binary packages without source of the same version can
               be removed freely when not needed anymore (these are
               binaries left for smooth update) *)
            else if M.compare_version v v' = 0 then begin
              (* We cannot remove a binary without removing its source *)
              if source_changed then
                associates pkg (p.M.source, "source")
                  (Source_not_propagated p.M.source)
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
              associates (p.M.source, "source") pkg
                (Binary_not_propagated pkg))
         t'.M.packages_by_num)
    l;

  Hashtbl.iter
    (fun nm s ->
       if not (Hashtbl.mem sources_with_binaries nm) then
         let v = s.M.s_version in
         no_change ((nm, v), "source") No_binary)
    u;


  if debug_time () then
    Format.eprintf "Initial constraints: %f@." (Timer.stop init_t);

  let l0 = l in
  let l = reduce_repositories l in

  let l' =
    List.map
      (fun (arch, (t', u')) ->
         (arch,
          Upgrade_common.prepare_analyze t',
          Upgrade_common.prepare_analyze u'))
      l
  in

  let find_coinst_constraints () =
    while
      let changed = ref false in
      List.iter
        (fun (arch, t', u') ->
           if debug_coinst () then
             Format.eprintf "==================== %s@." arch;
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
                        ListTbl.find
                          u'.Upgrade_common.dist.M.packages_by_name nm
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
             if debug_time () then begin
               Format.eprintf "  New constraints: %f@." (Timer.stop t);
               Format.eprintf "Step duration: %f@." (Timer.stop step_t)
             end;
             if !arch_changed then changed := true;
             !arch_changed
           do () done)
        l';
      !changed
    do () done
  in

  find_coinst_constraints ();
  if !deferred_constraints <> [] then begin
    perform_deferred ();
    find_coinst_constraints ()
  end;

  if debug_outcome () then begin
    Hashtbl.iter
      (fun nm s ->
         if not (ListTbl.mem unchanged (nm, "source")) then
           try
             let s' = Hashtbl.find u nm in
             Format.eprintf "Upgrade source package %s from %a to %a@." nm
               M.print_version s.M.s_version M.print_version s'.M.s_version
           with Not_found ->
             Format.eprintf "Remove source package %s@." nm)
      t;
    Hashtbl.iter
      (fun nm v ->
         if not (Hashtbl.mem t nm || ListTbl.mem unchanged (nm, "source")) then
           Format.eprintf "Adding source package %s@." nm)
      u;
    List.iter
      (fun (arch, (t', u')) ->
         Hashtbl.iter
           (fun _ p ->
              let nm = p.M.package in
              let v = p.M.version in
              if not (ListTbl.mem unchanged (nm, arch)) then
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
              if not (ListTbl.mem unchanged (nm, arch)) then
                if not (ListTbl.mem t'.M.packages_by_name nm) then
                  Format.eprintf "Adding binary package %s/%s@." nm arch)
           u'.M.packages_by_num)
      l
  end;

  generate_hints t u l l';

  let print_heidi ch =
    let lines = ref [] in
    let add_line nm vers arch sect =
      let b = Buffer.create 80 in
      Format.bprintf b "%s %a %s %s@." nm M.print_version vers arch sect;
      lines := Buffer.contents b :: !lines
    in
    let output_lines ch =
      List.iter (output_string ch) (List.sort compare !lines); lines := []
    in
    List.iter
      (fun (arch, (t, u)) ->
         let is_preserved nm = ListTbl.mem unchanged (nm, arch) in
         Hashtbl.iter
           (fun _ p ->
              let nm = p.M.package in
              let sect = if p.M.section = "" then "faux" else p.M.section in
              if is_preserved nm then add_line nm p.M.version arch sect)
           t.M.packages_by_num;
         Hashtbl.iter
           (fun _ p ->
              let nm = p.M.package in
              let sect = if p.M.section = "" then "faux" else p.M.section in
              if not (is_preserved nm) then
                add_line nm p.M.version arch sect)
           u.M.packages_by_num;
         output_lines ch)
      (List.sort (fun (arch, _) (arch', _) -> compare arch arch') l0);
    let is_preserved nm = ListTbl.mem unchanged (nm, "source") in
    let source_sect nm s =
      if Hashtbl.mem fake_src nm then "faux"
      else if s.M.s_section = "" then "unknown"
      else s.M.s_section
    in
    Hashtbl.iter
      (fun nm s ->
         let sect = source_sect nm s in
         if is_preserved nm then add_line nm s.M.s_version "source" sect)
      t;
    Hashtbl.iter
      (fun nm s ->
         let sect = source_sect nm s in
         if not (is_preserved nm) then add_line nm s.M.s_version "source" sect)
      u;
    output_lines ch; flush ch
  in
  if !heidi_file <> "" then begin
    let ch = open_out !heidi_file in
    print_heidi ch;
    close_out ch
  end;

  if !excuse_file <> "" then output_reasons l0 !excuse_file

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
   "--debug",
   Arg.String Debug.set,
   "NAME      Activate debug option NAME";
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
try
  f ()
with _ ->
  Printexc.print_backtrace stdout

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
- incremental (re)computations
- repeat the remove commands from the input to the hint ouput?
- what should we do about arch: all packages?
  ==> ignore them, in a by need fashion (if we find a conflict involving
      one of them, add the package to the 'can break' list
- cache reverse dependencies: then, either we have a lot of work to do and
  we can afford to compute them, or we can get them rapidly
- better report issues preventing migration:
  => show source package version numbers
  => example: empathy
- allow breaking single packages (--force-break option?)
- improve '--migrate option': multiple files, bin_nmus
- could the 'migrate' option automatically generate removal hints?
  (seems difficult, as there can be many possible choices...)
- parse more options from britney config file (in particular, hint files)
- make Deb_lib more abstract...
- SVG graphs: use CSS styles
- incremental recomputation of repositories and flattened repositories

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

EXPLANATIONS
  ==> summaries; in particular, show packages that only wait for age,
      for bugs

LATER
==> user interaction
    - interactive mode
==> performance
    - for migration, is it possible to focus on a small part of
      the repositories?
    - urgency information is huge; can we reduce it?
      (filter, depending on source informations...)
    - reducing size with installability:
      ===> flatten a superposition of testing and sid
      ===> needs to be very conservative!!
      packages with no deps after flattening do not need deps before that
===> robustness
    - find_coinst_constraints: check whether we have a larger set
      of unconstrained packages and automatically recompute the set
      of packages to consider in that case
*)


(**** Configuration settings ****)

let archs =
  ref ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips";
       "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64";
       (*"armhf"; "s390x"*)]
let smooth_updates = ref ["libs"; "oldlibs"]

let default_dir = Filename.concat (Sys.getenv "HOME") "debian-dists/britney"
let dir = ref default_dir

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

let bug_url n = "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=" ^ n
let pts_url nm = Format.sprintf "http://packages.qa.debian.org/%s" nm
let build_log_url nm arch =
  Format.sprintf
    "https://buildd.debian.org/status/logs.php?arch=%s&pkg=%s" arch nm

let cache_dir = Filename.concat (Sys.getenv "HOME") ".coinst"

let urgency_delay u =
  match u with
    "low"       -> 10
  | "medium"    -> 5
  | "high"      -> 2
  | "critical"  -> 0
  | "emergency" -> 0
  | _           -> assert false

let default_urgency = urgency_delay "low"

let update_data = ref false
let hint_file = ref ""
let heidi_file = ref ""
let excuse_file = ref ""
let offset = ref 0
let all_hints = ref false
let to_migrate = ref None
let to_remove = ref []
let check_coinstallability = ref true
let equivocal = ref false
let svg = ref false
let broken_sets = Upgrade_common.empty_break_set ()

(**** Debug options ****)

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
let debug_migration =
  Debug.make "migration" "Debug migration option" ["normal"]
let debug_gc =
  Debug.make "gc" "Output gc stats" []
let debug_remove = Debug.make "remove" "Debug removal hints" ["normal"]

(**** Useful modules from Util ****)

module StringSet = Util.StringSet
module IntSet = Util.IntSet
module Timer = Util.Timer
module ListTbl = Util.ListTbl
module StringTbl = Util.StringTbl
module BitVect = Util.BitVect
module Union_find = Util.Union_find

let (>>) v f = f v

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
let _ =
Printexc.record_backtrace true;
Gc.set { (Gc.get ())
         with Gc.space_overhead = 200; max_overhead = 1000000;
              major_heap_increment = 5 * 1024 * 1024 }

(**** Parsing of input files ****)

let whitespaces = Str.regexp "[ \t]+"
let comma = Str.regexp ","
let slash = Str.regexp "/"

let now = truncate ((Unix.time () /. 3600. -. 15.) /. 24.)

let read_package_info file f =
  let ch = open_in file in
  let h = StringTbl.create 32768 in
  begin try
    while true do
      let l = input_line ch in
      match Str.split whitespaces l with
        [name; version; info] ->
          let version = Deb_lib.parse_version version in
          StringTbl.add h name (version, f info)
      | [] ->
          ()
      | _ ->
          assert false
    done;
  with End_of_file -> () end;
  close_in ch;
  h

let read_dates file =
  let cache = Filename.concat cache_dir "Dates" in
  fst (Cache.cached [file] cache "version 1"
         (fun () -> read_package_info file int_of_string))

let read_urgencies file =
  let cache = Filename.concat cache_dir "Urgencies" in
  fst (Cache.cached [file] cache "version 1"
         (fun () -> read_package_info file urgency_delay))

let read_bugs file =
  let ch = open_in file in
  let h = StringTbl.create 4096 in
  begin try
    while true do
      let l = input_line ch in
      match Str.split whitespaces l with
        [name; bugs] ->
          StringTbl.add h name
            (List.fold_right StringSet.add
               (Str.split comma bugs) StringSet.empty)
      | _ ->
          assert false
    done;
  with End_of_file -> () end;
  close_in ch;
  h

type hint =
  { h_block : string StringTbl.t;
    h_block_udeb : string StringTbl.t;
    mutable h_block_all : string option;
    h_unblock : Deb_lib.version StringTbl.t;
    h_unblock_udeb : Deb_lib.version StringTbl.t;
    h_urgent : Deb_lib.version StringTbl.t;
    h_remove : Deb_lib.version StringTbl.t;
    h_age_days : (Deb_lib.version option * int) StringTbl.t }

let debug_read_hints = Debug.make "read_hints" "Show input hints." ["normal"]

let process_unblock_request h l =
  List.iter
    (fun p ->
       match Str.split slash p with
         [nm; v] ->
           let v = Deb_lib.parse_version v in
           begin try
             let v' = StringTbl.find h nm in
             if Deb_lib.compare_version v' v < 0 then raise Not_found
           with Not_found ->
             StringTbl.replace h nm v
           end
       | _ ->
           ())
    l

exception Ignored_hint

let hint_files () =
  let hint_re = Str.regexp "^HINTS_\\(.*\\)$" in
  Hashtbl.fold
    (fun key _ l ->
       if Str.string_match hint_re key 0 then
         String.lowercase (Str.matched_group 1 key) :: l
       else
         l)
    options []
  >> List.sort compare

let read_hints dir =
  let hints =
    { h_block = StringTbl.create 16;
      h_block_udeb = StringTbl.create 16;
      h_block_all = None;
      h_unblock = StringTbl.create 16;
      h_unblock_udeb = StringTbl.create 16;
      h_urgent = StringTbl.create 16;
      h_remove = StringTbl.create 16;
      h_age_days = StringTbl.create 16 }
  in
  if debug_read_hints () then
    Format.eprintf "Reading hints:@.";
  let files =
    List.filter
      (fun who ->
         Sys.file_exists (Filename.concat dir who)
           ||
         (Format.eprintf "Hint file '%s' does not exists.@." who; false))
      (hint_files ())
  in
  List.iter
    (fun who ->
       let ch = open_in (Filename.concat dir who) in
       begin try
         while true do
           let l = input_line ch in
           let l = Str.split whitespaces l in
           try
             begin match l with
             | "block" :: l ->
                 List.iter
                   (fun p -> StringTbl.replace hints.h_block p who) l
             | "block-udeb" :: l ->
                 List.iter
                   (fun p -> StringTbl.replace hints.h_block_udeb p who) l
             | "block-all" :: l ->
                 if List.mem "source" l then hints.h_block_all <- Some who
             | "unblock" :: l ->
                 process_unblock_request hints.h_unblock l
             | "unblock-udeb" :: l ->
                 process_unblock_request hints.h_unblock_udeb l
             | "urgent" :: l ->
                 List.iter
                   (fun p ->
                      match Str.split slash p with
                        [nm; v] -> StringTbl.replace hints.h_urgent
                                     nm (Deb_lib.parse_version v)
                      | _       -> ())
                   l
             | "remove" :: l ->
                 List.iter
                   (fun p ->
                      match Str.split slash p with
                        [nm; v] -> StringTbl.replace hints.h_remove
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
                          StringTbl.replace hints.h_age_days nm (v, n)
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

(**** Conversion from dot to svg ****)

let send_to_dot_process (oc, _) s = output_string oc s; flush oc
let send_to_dot_process = Task.funct send_to_dot_process

let shutdown_dot_process (oc, pid) () =
  close_out oc; ignore (Unix.waitpid [] pid)
let shutdown_dot_process = Task.funct shutdown_dot_process

let create_dot_process () =
  let (out_read, out_write) = Unix.pipe () in
  flush_all ();
  let helper =
    Task.spawn
      (fun () ->
         Unix.close out_read;
         let (in_read, in_write) = Unix.pipe () in
         match Unix.fork () with
           0 ->
             Unix.close in_write;
             Unix.dup2 in_read Unix.stdin; Unix.dup2 out_write Unix.stdout;
             Unix.close in_read; Unix.close out_write;
             Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; "dot" |]
         | pid ->
             Unix.close in_read;
             (Unix.out_channel_of_descr in_write, pid))
  in
  Unix.close out_write;
  (helper, Unix.in_channel_of_descr out_read)

let dot_process = lazy (create_dot_process ())

let dot_to_svg s =
  let (t, ic) = Lazy.force dot_process in
  let send = send_to_dot_process t s in
  let (_, g) = Dot_graph.of_channel ic in
  ignore (Task.wait send);
  let (bbox, scene) = Dot_render.f g in
  let l = Scene.get scene in
  let b = Buffer.create 200 in
  Scene_svg.format (Format.formatter_of_buffer b) (bbox, l);
  Buffer.contents b

(**** Parsing of Debian control files ****)

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
  let dist = M.new_src_pool () in
  assert (not (Sys.is_directory file));
  let ch = File.open_in file in
  M.parse_src_packages dist ch;
  close_in ch;
  M.src_only_latest dist

(**** Possible reasons for a package not to be upgraded ****)

type reason =
  | Unchanged
    (* Source *)
  | Blocked of (string * string)
  | Too_young of int * int
  | Binary_not_added | Binary_not_removed
  | No_binary
  (* Both *)
  | More_bugs of StringSet.t
  (* Binaries *)
  | Conflict of IntSet.t * IntSet.t * StringSet.t * Upgrade_common.reason list
  | Not_yet_built of string * M.version * M.version
  | Source_not_propagated
  | Atomic

module L = Layout
let (&) = L.(&)

let print_pkg_ref (nm, t, u) =
  match t, u with
    true,  true  -> L.code (L.s nm)
  | true,  false -> L.code (L.s nm) & L.s " (testing)"
  | false, true  -> L.code (L.s nm) & L.s " (sid)"
  | false, false -> assert false

let print_cstr r =
  match r with
    Upgrade_common.R_depends (pkg, dep, pkgs) ->
      print_pkg_ref pkg & L.s " depends on " &
      L.code (L.format M.print_package_dependency [dep]) &
      L.s " {" & L.seq ", " print_pkg_ref pkgs & L.s "}"
  | Upgrade_common.R_conflict (pkg, confl, pkg') ->
      print_pkg_ref pkg & L.s " conflicts with " &
      L.code
        (L.format M.print_package_dependency (List.map (fun c -> [c]) confl)) &
      L.s " {" & print_pkg_ref pkg' & L.s "}"

let print_explanation conflict expl =
  let conflict_graph () =
    let b = Buffer.create 200 in
    Upgrade_common.output_conflict_graph (Format.formatter_of_buffer b)
      conflict expl;
    dot_to_svg (Buffer.contents b)
  in
  L.ul ~prefix:" - " (L.list (fun r -> L.li (print_cstr r)) expl) &
  (if !svg then L.raw_html conflict_graph else L.emp)

let print_binaries conj print_binary s =
  let l =
    IntSet.elements s
    >> List.map print_binary
    >> List.sort (Util.compare_pair compare compare)
    >> List.map snd
  in
  match l with
    []     -> L.emp
  | [p]    -> p
  | [p; q] -> p & L.s " " & L.s conj & L.s " " & q
  | _      -> L.seq ", " (fun x -> x)
                (l
                 >> List.rev
                 >> (fun l ->
                       match l with
                         [] | [_]           -> l
                       | p :: (_ :: _ as r) -> (L.s conj & L.s " " & p) :: r)
                 >> List.rev)

let print_reason capitalize print_binary print_source lits reason =
  let c = if capitalize then String.capitalize else String.uncapitalize in
  match reason with
    Unchanged ->
      L.s (c "No update")
  | Blocked (kind, who) ->
      L.s (c "Left unchanged due to ") & L.s kind &
      L.s " request by " & L.s who & L.s "."
  | Too_young (cur_ag, req_ag) ->
      L.s (c "Only ") & L.i cur_ag & L.s " days old; must be " & L.i req_ag &
      L.s " days old to go in."
  | More_bugs s ->
      L.s (c "Has new bugs: ") &
      L.seq ", " (fun s -> L.anchor (bug_url s) (L.s "#" & L.s s))
        (StringSet.elements s) &
      L.s "."
  | Conflict (s, s', conflict, explanation) ->
      begin match IntSet.cardinal s with
        0 ->
          L.s (c "A dependency would not be satisfied")
      | 1 ->
          L.s (c "Needs binary package ") &
          snd (print_binary false (IntSet.choose s))
      | _ ->
          L.s (c "Needs one of the binary packages ") &
          print_binaries "or" (print_binary false) s
      end
        &
      begin
        if IntSet.cardinal s' > 1 || not (IntSet.mem lits.(0) s') then begin
          if IntSet.cardinal s' = 1 then begin
            L.s " (would break package " &
            snd (print_binary false (IntSet.choose s')) & L.s ")"
          end else begin
            L.s " (would break co-installability of packages " &
            print_binaries "and" (print_binary false) s' & L.s ")"
          end
        end else
          L.emp
      end
        &
      L.s ":" & print_explanation conflict explanation
  | Not_yet_built (src, v1, v2) ->
      L.s (c "Not yet rebuilt (source ") &
      L.s src & L.s " version " & L.format M.print_version v1 &
      L.s " rather than " & L.format M.print_version v2 & L.s ")."
  | Source_not_propagated ->
      L.s (c "Source package ") & print_source lits.(1) &
      L.s " cannot migrate."
  | Atomic ->
      L.s (c "Binary package ") & snd (print_binary false lits.(1)) &
      L.s " cannot migrate."
  | Binary_not_added ->
      L.s (c "Binary package ") & snd (print_binary true lits.(1)) &
      L.s " cannot migrate."
  | Binary_not_removed ->
      L.s (c "Binary package ") & snd (print_binary true lits.(1)) &
      L.s " cannot be removed."
  | No_binary ->
      L.s (c "No associated binary package.")

let print_reason' get_name_arch lits reason =
  let print_binary verbose id =
    let (nm, arch) = get_name_arch id in
    (nm, if verbose then L.s nm & L.s "/" & L.s arch else L.s nm)
  in
  let print_source id = let (nm, _) = get_name_arch id in L.s nm in
  print_reason false print_binary print_source lits reason

(**** Constraint solver ****)

module HornSolver = Horn.F (struct type t = reason type reason = t end)

(**** Caching of non-coinstallability issues ****)

let learnt_rules = ref []

let learn_rule r neg s s' expl =
  learnt_rules := (r, neg, s, s', expl) :: !learnt_rules

let coinst_rules = ref []

let switch_to_installability solver =
  assert !check_coinstallability;
  List.iter (fun r -> HornSolver.retract_rule solver r) !coinst_rules;
  check_coinstallability := false

let load_rules solver uids =
  let cache = Filename.concat cache_dir "Rules" in
  let uids = String.concat "\n" uids in
  let (rules, _) =
    Cache.cached [] cache ("version 6\n" ^ uids) (fun () -> []) in
  List.iter
    (fun (r, neg, s, s', expl) ->
       let n = IntSet.cardinal s in
       if
         (!check_coinstallability || n = 1)
           &&
         not (Upgrade_common.is_ignored_set broken_sets s')
       then begin
         let r = HornSolver.add_rule solver r (Conflict (neg, s, s', expl)) in
         if n > 1 then coinst_rules := r :: !coinst_rules
       end)
    rules;
  learnt_rules := List.rev rules

let save_rules uids =
  let cache = Filename.concat cache_dir "Rules" in
  let uids = String.concat "\n" uids in
  ignore (Cache.cached ~force:true [] cache ("version 6\n" ^ uids)
            (fun () -> List.rev !learnt_rules))

(**** Global state for per-architecture processes ****)

type st =
  { arch : string;
    testing : M.pool; unstable : M.pool;
    testing_srcs : M.s_pool; unstable_srcs : M.s_pool;
    unstable_bugs : StringSet.t StringTbl.t;
    testing_bugs : StringSet.t StringTbl.t;
    mutable outdated_binaries : M.p list;
    first_bin_id : int;
    id_of_bin : HornSolver.var StringTbl.t; bin_of_id : string array;
    id_of_source : HornSolver.var StringTbl.t;
    mutable upgrade_state :
      (Upgrade_common.state * Upgrade_common.state) option;
    uid : string }

(**** Misc. useful functions ****)

let source_version src nm =
  try
    Some (StringTbl.find src.M.s_packages nm).M.s_version
  with Not_found ->
    None
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
  match M.find_packages_by_name dist nm with
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

let compute_ages dates urgencies hints nm uv tv =
  let d =
    try
      let (v, d) = StringTbl.find dates nm in
      if M.compare_version uv v = 0 then d else now
    with Not_found ->
      now
  in
  let u =
    if
      try
        M.compare_version uv (StringTbl.find hints.h_urgent nm) = 0
      with Not_found ->
        false
    then
      0
    else
      try
        let (v, d) = StringTbl.find hints.h_age_days nm in
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
                (StringTbl.find_all urgencies nm)
            in
            List.fold_left (fun u (_, u') -> min u u') default_urgency l
  in
  (now + !offset - d, u)

(**** Writing of an excuse file ****)

let rec interesting_reason solver (lits, reason) =
  match reason with
    Unchanged ->
      false
  | Binary_not_added | Binary_not_removed ->
      List.exists (interesting_reason solver)
        (HornSolver.direct_reasons solver lits.(1))
  | Source_not_propagated ->
      false
  | Atomic ->
      false
  | _ ->
      true

let binary_names st (offset, l) =
  List.map
    (fun id ->
      let nm = st.bin_of_id.(id - offset) in
      let p =
        match M.find_packages_by_name st.unstable nm with
          p :: _ -> p
        | []     -> match M.find_packages_by_name st.testing nm with
                      p :: _ -> p
                    | []     -> List.find (fun p -> p.M.package = nm)
                                  st.outdated_binaries
      in
      (id, nm, fst p.M.source))
    l
let binary_names = Task.funct binary_names

let output_reasons
      l dates urgencies hints solver source_of_id id_offsets filename t u =
  let reason_t = Timer.start () in

  let blocked_source = StringTbl.create 1024 in
  let sources = ref [] in
  let binaries = ref IntSet.empty in
  Array.iteri
    (fun id nm ->
       let reasons = HornSolver.direct_reasons solver id in
       if List.exists (interesting_reason solver) reasons then begin
         sources := (nm, (id, reasons)) :: !sources;
         StringTbl.add blocked_source nm ();
         List.iter
           (fun (lits, reason) ->
              match reason with
                Binary_not_added | Binary_not_removed ->
                  let id = lits.(1) in
                  binaries := IntSet.add id !binaries;
                  List.iter
                    (fun (lits, reason) ->
                       match reason with
                         Conflict (s, s', _, _) ->
                           binaries :=
                             IntSet.union (IntSet.union s s') !binaries
                       | _ ->
                           ())
                    (HornSolver.direct_reasons solver id)
              | _ ->
                  ())
           reasons

       end)
    source_of_id;

  let name_of_binary = Hashtbl.create 1024 in
  Task.iteri l
    (fun (arch, st) ->
       let (first, offset, len) = StringTbl.find id_offsets arch in
       let pos = first + offset in
       let l =
         IntSet.elements
           (IntSet.filter (fun id -> id >= pos && id < pos + len) !binaries)
       in
       (arch, binary_names st (pos, l)))
    (fun arch l ->
       List.iter
         (fun (id, nm, src) -> Hashtbl.add name_of_binary id (nm, arch, src))
         l);

  let print_binary _ id =
    let (nm, arch, source) = Hashtbl.find name_of_binary id in
    let txt =
      if source = nm then
        L.code (L.s nm)
      else
        (L.code (L.s nm) & L.s " (from " & L.code (L.s source) & L.s ")")
    in
    if not (StringTbl.mem blocked_source source) then (nm, txt) else
    (nm, L.anchor ("#" ^ source) txt)
  in
  let print_source id = assert false in

  let lst =
    L.dl (L.list
      (fun (nm, (id, reasons)) ->
         let about_bin (_, r) =
           match r with
             Binary_not_added | Binary_not_removed -> true
           | _                                     -> false
         in
         let src_reasons =
           begin match StringTbl.find_all u.M.s_packages nm with
             []  -> L.emp
           | [p] ->
               if same_source_version t u nm then L.emp else
               let (cur_ag, req_ag) =
                 compute_ages dates urgencies hints
                      nm p.M.s_version (source_version t nm)
               in
               if cur_ag < req_ag then L.emp else
               L.li (L.s "Package is " & L.i cur_ag &
                     L.s " days old (needed " & L.i req_ag & L.s " days).")
           | _   -> assert false
           end
             &
           L.list
             (fun r ->
                if interesting_reason solver r && not (about_bin r) then
                  L.li (print_reason true print_binary print_source
                          (fst r) (snd r))
                else
                  L.emp)
             reasons
         in
         let binaries =
           reasons
           >> List.filter
                (fun r -> interesting_reason solver r && about_bin r)
           >> List.map (fun (lits, r) -> (lits.(1), r))
           >> List.sort (Util.compare_pair compare compare)
           >> Util.group compare
           >> List.map
                (fun (id, l) ->
                   let (name, arch, _) = Hashtbl.find name_of_binary id in
                   let is_removal =
                     List.for_all (fun r -> r = Binary_not_removed) l in
                   (name, (arch, (id, is_removal))))
           >> List.sort
                (Util.compare_pair compare (Util.compare_pair compare compare))
         in
         let not_yet_built =
           binaries
           >>
           List.filter
             (fun (_, (_, (id, _))) ->
                List.exists
                  (fun (_, r) ->
                     match r with Not_yet_built _ -> true | _ -> false)
                  (HornSolver.direct_reasons solver id))
           >> List.map (fun (name, (arch, _)) -> (name, arch))
           >> List.sort (Util.compare_pair compare compare)
           >> Util.group compare
           >> List.map (fun (name, l) -> (l, name))
           >> List.sort (Util.compare_pair (Util.compare_list compare) compare)
           >> Util.group (Util.compare_list compare)
         in
         let build_reasons =
           L.list
             (fun (al, bl) ->
                L.li (L.s (if List.length bl = 1 then "Binary package "
                           else "Binary packages ") &
                      L.seq ", " (fun nm -> L.code (L.s nm)) bl &
                      L.s " not yet rebuilt on " &
                      L.seq ", "
                        (fun arch ->
                           L.anchor (build_log_url nm arch) (L.s arch)) al &
                      L.s "."))
             not_yet_built
         in
         let with_new_bugs =
           Util.group compare
             (List.flatten
                (List.map
                   (fun (nm, (_, (id, _))) ->
                      List.flatten
                        (List.map
                           (fun (_, reason) ->
                              match reason with
                                More_bugs s -> [(nm, s)]
                              | _           -> [])
                           (HornSolver.direct_reasons solver id)))
                   binaries))
         in
         let bug_reasons =
           L.list
             (fun (nm', s) ->
                let s = List.hd s in (* Same bugs on all archs. *)
                if nm' <> nm then
                  L.li (L.s "Binary package " & L.code (L.s nm') &
                        L.s " has new bugs: " &
                        L.seq ", "
                          (fun s -> L.anchor (bug_url s) (L.s "#" & L.s s))
                          (StringSet.elements s) &
                        L.s ".")
                else
                  L.emp)
           with_new_bugs
         in
         let compare_conflict r r' =
           match r, r' with
             Conflict (s1, s1', _, e1), Conflict (s2, s2', _, e2) ->
               compare e1 e2
           | _ ->
               assert false
         in
         let compare_reason =
           Util.compare_pair (fun x y -> 0) compare_conflict in
         let binaries =
           binaries >> Util.group compare >>
           List.map
             (fun (nm, l) ->
                l >>
                List.map
                  (fun (arch, (id, is_removal)) ->
                     (HornSolver.direct_reasons solver id
                        >>
                      List.filter
                        (fun r ->
                           interesting_reason solver r &&
                           match snd r with
                             Not_yet_built _ | More_bugs _ -> false
                           | Conflict _                    -> true
                           | _                             -> assert false)
                         >>
                      List.sort compare_reason,
                      (arch, is_removal))) >>
                List.sort (Util.compare_pair
                             (Util.compare_list compare_reason) compare) >>
                Util.group (Util.compare_list compare_reason) >>
                List.map (fun (archs, l) -> (nm, (archs, l)))) >>
           List.flatten
         in
         let bin_reasons =
           L.list
             (fun (nm, (reasons, archs_and_removals)) ->
                if reasons =  [] then L.emp else
                let is_removal = List.for_all snd archs_and_removals in
                let archs = List.map fst archs_and_removals in
                let reasons =
                  L.list
                    (fun (lits, r) ->
                       L.li (print_reason true print_binary print_source
                               lits r))
                    reasons
                in
                L.li (L.s "Binary package " & L.code (L.s nm) &
                      L.s " cannot be " &
                      L.s (if is_removal then "removed" else "propagated") &
                      L.s " (" & L.seq ", " L.s archs & L.s "):" &
                      L.ul reasons))
             binaries
         in
         let version dist nm =
           match StringTbl.find_all dist.M.s_packages nm with
             [p] -> L.format M.print_version p.M.s_version
           | []  -> L.s "-"
           | _   -> assert false
         in
         let versions nm =
           if
             List.exists (fun (_, r) -> r = Unchanged)
               (HornSolver.direct_reasons solver id)
           then
             version t nm
           else
             (version t nm & L.s " to " & version u nm)
         in
         L.dli ~id:nm (L.anchor (pts_url nm) (L.code (L.s nm)) &
                       L.s " (" & versions nm & L.s ")")
           (L.ul (src_reasons & build_reasons & bug_reasons & bin_reasons)))
      (List.sort (fun (nm1, _) (nm2, _) -> compare nm1 nm2) !sources))
  in

  let ch = if filename = "-" then stdout else open_out filename in
  L.print (new L.html_printer ch "Explanations") lst;
  if filename <> "-" then close_out ch;
  if debug_time () then
    Format.eprintf "Writing excuse file: %f@." (Timer.stop reason_t)

(**** Per arch. information on which packages are not propagated ****)

let extract_unchanged_bin solver id_offsets arch unch =
  let (first, offset, len) = StringTbl.find id_offsets arch in
  BitVect.sub unch (first + offset) len

let is_unchanged st unch nm =
  BitVect.test unch (StringTbl.find st.id_of_bin nm - st.first_bin_id)

(**** Prepare repositories before looking for (co-)installability issues ****)

let compute_reverse_dependencies st d id_tbl =
  let rdeps = Array.create (Array.length st.bin_of_id) [] in
  let add_rdep src_id dep =
    List.iter
      (fun cstr ->
         List.iter
           (fun q ->
              let target_id =
                PTbl.get id_tbl (Package.of_index q.M.num) in
              let i = target_id - st.first_bin_id in
              rdeps.(i) <- src_id :: rdeps.(i))
           (M.find_provided_packages d (fst cstr)))
      dep
  in
  M.iter_packages d
    (fun p ->
       let src_id = PTbl.get id_tbl (Package.of_index p.M.num) in
       List.iter (fun d -> add_rdep src_id d) p.M.depends;
       List.iter (fun d -> add_rdep src_id d) p.M.pre_depends);
  rdeps

let compute_reverse_dependencies st d id_tbl =
  let rdep_t = Timer.start () in
  let rdeps = compute_reverse_dependencies st d id_tbl in
(*
  let cache = Filename.concat cache_dir ("Rev_" ^ st.arch) in
  let (rdeps, _) =
    Cache.cached [] cache ("version 1\n" ^ st.uid)
      (fun () -> compute_reverse_dependencies st d id_tbl)
  in
*)
  if debug_time () then
    Format.eprintf "  Reversing dependencies: %f@." (Timer.stop rdep_t);
  rdeps

let reduce_for_installability st unchanged =
  let t = st.testing in
  let u = st.unstable in
  let d = M.new_pool () in
  M.merge d (fun _ -> true) t;
  M.merge d (fun p -> true) u;
  let id_tbl = PTbl.create d 0 in
  M.iter_packages d
    (fun p ->
       PTbl.set id_tbl
         (Package.of_index p.M.num) (StringTbl.find st.id_of_bin p.M.package));

  let rdeps = compute_reverse_dependencies st d id_tbl in

  let predecessors = StringTbl.create 1024 in
  let rec add_preds nm =
    if not (StringTbl.mem predecessors nm) then begin
      StringTbl.add predecessors nm ();
      let id = StringTbl.find st.id_of_bin nm in
      List.iter (fun id -> add_preds st.bin_of_id.(id - st.first_bin_id))
        rdeps.(id - st.first_bin_id)
    end
  in
  Array.iter
    (fun nm -> if not (is_unchanged st unchanged nm) then add_preds nm)
    st.bin_of_id;

  let pkgs = StringTbl.create 1024 in
  let rec add_package nm =
    if not (StringTbl.mem pkgs nm) then begin
      StringTbl.add pkgs nm ();
      List.iter follow_deps (M.find_packages_by_name d nm);
    end
  and follow_deps p =
    follow_deps_2 p.M.depends; follow_deps_2 p.M.pre_depends
  and follow_deps_2 deps =
    List.iter
      (fun l ->
         List.iter
           (fun (nm, _) ->
              List.iter
                (fun q -> add_package q.M.package)
                (M.find_provided_packages d nm))
           l)
      deps
  in
  StringTbl.iter (fun nm _ -> add_package nm) predecessors;
  pkgs

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
    M.iter_packages d1
      (fun p ->
         add_conflicts d2 p p.M.conflicts;
         add_conflicts d2 p p.M.breaks)
  in
  compute_package_conflicts t t; compute_package_conflicts t u;
  compute_package_conflicts u t; compute_package_conflicts u u;
  conflicts

let reduce_for_coinstallability st unchanged =
  let t = st.testing in
  let u = st.unstable in

  let conflicts = compute_conflicts t u in

  let changed_packages = StringTbl.create 101 in
  let consider_package p =
    let nm = p.M.package in
    if not (is_unchanged st unchanged nm) then
      StringTbl.replace changed_packages nm ()
  in
  M.iter_packages t consider_package;
  M.iter_packages u consider_package;

  let pkgs = StringTbl.create 1024 in
  let rec add_package p =
    if not (StringTbl.mem pkgs p) then begin
      StringTbl.add pkgs p ();
      List.iter add_package (ListTbl.find conflicts p);
      List.iter follow_deps (M.find_packages_by_name t p);
      List.iter follow_deps (M.find_packages_by_name u p)
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
                (M.find_provided_packages d nm))
           l)
      deps
  in

  (* Changed packages should be kept. *)
  StringTbl.iter (fun p _ -> add_package p) changed_packages;

  (* Packages unchanged but with stronger dependencies, or that may
     depend on a package for which we ignore some co-installability
     issues, should be kept as well. *)
  let break_candidates = Upgrade_common.ignored_set_domain broken_sets in
  let stronger_deps l =
    (* Check whether there is a package that satisfies the dependency,
       that might not satisfy the dependency anymore. *)
    List.exists
      (fun d ->
         List.exists
           (fun cstr ->
              List.exists
                (fun p ->
                   StringSet.mem p.M.package break_candidates
                     ||
                   (* If the package is left unchanged, the dependency
                      will remain satisfied *)
                   (StringTbl.mem changed_packages p.M.package &&
                    (* Otherwise, we check whether a replacement exists,
                       that still satisfies the dependency *)
                    List.for_all
                      (fun cstr' ->
                         List.for_all
                           (fun p' -> p.M.package <> p'.M.package)
                           (M.resolve_package_dep_raw u cstr'))
                      d))
                (M.resolve_package_dep_raw t cstr)
                   ||
              (not (StringSet.is_empty break_candidates)
                 &&
               List.exists
                 (fun p -> StringSet.mem p.M.package break_candidates)
                 (M.resolve_package_dep_raw u cstr)))
           d)
      l
  in
  (* Changed packages are already all in [pkgs], thus we only have to
     look in testing. *)
  M.iter_packages t
    (fun p ->
       if not (StringTbl.mem pkgs p.M.package) then
         if stronger_deps p.M.depends || stronger_deps p.M.pre_depends then
           add_package p.M.package);
  pkgs

let prepare_repository st unchanged check_coinstallability =
  let t = st.testing in
  let u = st.unstable in

  let red_t = Timer.start () in

  let pkgs =
    if check_coinstallability then
      reduce_for_coinstallability st unchanged
    else
      reduce_for_installability st unchanged
  in

  let n = ref 0 in
  let m = ref 0 in
  let filter p =
    incr m;
    let nm = p.M.package in
    let keep = StringTbl.mem pkgs nm in
    if keep then incr n;
    keep
  in
  let t' = M.new_pool () in
  M.merge t' filter t;
  let u' = M.new_pool () in
  M.merge u' filter u;
  if debug_reduction () then Format.eprintf "==> %d/%d@." !n !m;
  if debug_time () then
    Format.eprintf "  Reducing repository sizes: %f@." (Timer.stop red_t);
  st.upgrade_state <-
    Some (Upgrade_common.prepare_analyze t', Upgrade_common.prepare_analyze u')

let rec get_upgrade_state st unchanged check_coinstallability =
  match st.upgrade_state with
    Some state -> state
  | None       -> prepare_repository st unchanged check_coinstallability;
                  get_upgrade_state st unchanged check_coinstallability

let clear_upgrade_state = Task.funct (fun st () -> st.upgrade_state <- None)

let clear_upgrade_states l =
  Task.iter l (fun (arch, st) -> clear_upgrade_state st ()) (fun () -> ())

(**** Loading ****)

let compute_bin_ids first_id t u =
  let id = ref first_id in
  let id_of_bin = StringTbl.create 16384 in
  let bin_of_id = ref [] in
  let insert nm =
    StringTbl.add id_of_bin nm !id; bin_of_id := nm :: !bin_of_id; incr id
  in
  M.iter_packages_by_name t (fun nm _ -> insert nm);
  M.iter_packages_by_name u
    (fun nm _ -> if not (StringTbl.mem id_of_bin nm) then insert nm);
  let bin_of_id = Array.of_list (List.rev !bin_of_id) in
  (id_of_bin, bin_of_id)

let compute_source_ids t u =
  let id = ref 0 in
  let id_of_source = StringTbl.create 16384 in
  let source_of_id = ref [] in
  let insert nm =
    StringTbl.add id_of_source nm !id; source_of_id := nm :: !source_of_id;
    incr id
  in
  StringTbl.iter (fun nm _ -> insert nm) t.M.s_packages;
  StringTbl.iter
    (fun nm _ -> if not (StringTbl.mem id_of_source nm) then insert nm)
    u.M.s_packages;
  let source_of_id = Array.of_list (List.rev !source_of_id) in
  (id_of_source, source_of_id)

let share_packages (t, u) =
  let unchanged dist p =
    match M.find_packages_by_name dist p.M.package with
      []  -> false
    | [q] -> M.compare_version p.M.version q.M.version = 0
    | _   -> assert false
  in
  let common = M.new_pool () in
  M.merge common (fun p -> unchanged u p) t;
  let t' = M.copy common in
  M.merge t' (fun p -> not (unchanged u p)) t;
  let u' = common in
  M.merge u' (fun p -> not (unchanged t p)) u;
  assert (M.pool_size t' = M.pool_size t && M.pool_size u' = M.pool_size u);
  (t', u')

let load_arch arch
      testing_srcs testing_bugs unstable_srcs unstable_bugs
      id_of_source first_id () =
  let files =
    [bin_package_file (testing ()) arch;
     bin_package_file (unstable ()) arch]
  in
  let cache = bin_package_file cache_dir arch in
  let ((t, u), uid) =
    Cache.cached files cache "version 2"
      (fun () ->
        share_packages
         (load_bin_packages (testing ()) arch,
          load_bin_packages (unstable ()) arch))
  in
  let (id_of_bin, bin_of_id) = compute_bin_ids first_id t u in
  { arch = arch;
    testing = t;
    testing_srcs = testing_srcs; testing_bugs = testing_bugs;
    unstable = u;
    unstable_srcs = unstable_srcs; unstable_bugs = unstable_bugs;
    outdated_binaries = [];
    first_bin_id = first_id; id_of_bin = id_of_bin; bin_of_id = bin_of_id;
    id_of_source = id_of_source;
    upgrade_state = None; uid = uid }

let load_all_files () =
  let load_t = Timer.start () in
  let (testing_bugs, unstable_bugs) = read_bugs () in
  let files =
    [src_package_file (testing ()); src_package_file (unstable ())] in
  let cache = Filename.concat cache_dir "Sources" in
  let ((t, u), src_uid) =
    Cache.cached files cache "version 3" (fun () ->
      (load_src_packages (testing ()), load_src_packages (unstable ())))
  in
  let (id_of_source, source_of_id) = compute_source_ids t u in
  if debug_time () then
    Format.eprintf "  Loading shared data: %f@." (Timer.stop load_t);
  let l =
    List.map
      (fun arch ->
         (arch,
          Task.spawn
            (load_arch arch t testing_bugs u unstable_bugs
               id_of_source (Array.length source_of_id))))
      !archs
  in
  let (dates, urgencies, hints) = read_extra_info () in

  if debug_time () then Format.eprintf "Loading: %f@." (Timer.stop load_t);

  (dates, urgencies, hints, t, u, testing_bugs, unstable_bugs, l,
   id_of_source, source_of_id, src_uid)

(**** Constraint computation ****)

type cstr =
    Assume of HornSolver.var * reason
  | Implies of HornSolver.var * HornSolver.var * reason
  | All_or_none of HornSolver.var list * reason

let compute_hints () = debug_hints () || !hint_file <> ""

let string_uid s =
  s >> Digest.string >> Digest.to_hex >> fun s -> String.sub s 0 16

let remove_sources central remove_hints t u =
  let l = ref [] in
  StringTbl.iter
    (fun nm v ->
       match source_version t nm with
         Some v' when M.compare_version v v' = 0 ->
           if central && debug_remove () && StringTbl.mem u.M.s_packages nm then
             Format.eprintf "Trying to remove source package %s@." nm;
           StringTbl.remove u.M.s_packages nm;
           l := nm :: !l
         | _ ->
           ())
    remove_hints;
 !l >> List.sort compare
    >> fun l -> Marshal.to_string l [] >> string_uid
    


let arch_constraints
      st (produce_excuses, fucked_arch, break_arch, remove_hints) =
  let t = st.testing_srcs in
  let u = st.unstable_srcs in
  let t' = st.testing in
  let u' = st.unstable in

  let removed_pkgs = ref [] in
  M.iter_packages u'
    (fun p ->
       let (nm, _) = p.M.source in
       try
         let v = StringTbl.find remove_hints nm in
         let v' = (StringTbl.find t.M.s_packages nm).M.s_version in
         if M.compare_version v v' = 0 then begin
           if debug_remove () then
             Format.eprintf
               "Trying to remove binary package %s/%s (source: %s)@."
               p.M.package st.arch nm;
           removed_pkgs := p :: !removed_pkgs
         end
       with Not_found ->
         ());
  List.iter (fun p -> M.remove_package u' p) !removed_pkgs;
  ignore (remove_sources false remove_hints t u);

  let fake_srcs = ref [] in
  let is_fake = StringTbl.create 17 in
  let sources_with_binaries = ref [] in
  let source_has_binaries = StringTbl.create 8192 in
  let l = ref [] in
  let assume id reason = l := Assume (id, reason) :: !l in
  let implies id1 id2 reason = l := Implies (id1, id2, reason):: !l
  in
  let all_or_none pkgl reason = l :=
    All_or_none (Util.sort_and_uniq compare pkgl, reason) :: !l in
  let get_bugs src bugs p =
    try StringTbl.find bugs p with Not_found -> StringSet.empty
  in
  let no_new_bugs is_new p =
    if is_new then
      StringSet.is_empty (get_bugs u st.unstable_bugs p)
    else
      StringSet.subset
        (get_bugs u st.unstable_bugs p)
        (get_bugs t st.testing_bugs p)
  in
  let new_bugs is_new p =
    if is_new then
      get_bugs u st.unstable_bugs p
    else
      StringSet.diff
        (get_bugs u st.unstable_bugs p)
        (get_bugs t st.testing_bugs p)
  in
  let bin_nmus = ListTbl.create 101 in
  let source_id p = StringTbl.find st.id_of_source (fst p.M.source) in
  let bin_id p = StringTbl.find st.id_of_bin p.M.package in
  let bin_id_count = Array.length st.bin_of_id in
  let last_id = ref (st.first_bin_id + bin_id_count) in
  M.iter_packages u'
    (fun p ->
       let id = bin_id p in
       let (nm, v) = p.M.source in
       (* Faux packages *)
       if not (StringTbl.mem u.M.s_packages nm) then begin
         StringTbl.add u.M.s_packages nm
           { M.s_name = nm; s_version = v; s_section = "" };
         fake_srcs := (nm, v) :: !fake_srcs;
         StringTbl.add is_fake nm ();
         StringTbl.add st.id_of_source nm !last_id;
         incr last_id;
         assume (source_id p) Unchanged
       end;
       let v' = (StringTbl.find u.M.s_packages nm).M.s_version in
       let source_changed = not (same_source_version t u nm) in
       (* Do not add a binary package if its source is not
          the most up to date source file. *)
       let outdated = M.compare_version v v' <> 0 in
       if outdated then begin
         st.outdated_binaries <- p :: st.outdated_binaries;
         assume id (Not_yet_built (nm, v, v'))
       end else begin
         if not (StringTbl.mem source_has_binaries nm) then begin
           sources_with_binaries := nm :: !sources_with_binaries;
           StringTbl.add source_has_binaries nm ()
         end;
         (* We only propagate binary packages with a larger version.
            Faux packages are not propagated. *)
         if no_new_bin t' u' p.M.package || StringTbl.mem is_fake nm then
           assume id Unchanged
         else begin
           (* Do not upgrade a package if it has new bugs *)
           let is_new = bin_version t' p.M.package = None in
           if not (no_new_bugs is_new p.M.package) then
             assume id (More_bugs (new_bugs is_new p.M.package));
           if source_changed then
             (* We cannot add a binary package without also adding
                its source. *)
             implies (source_id p) id Source_not_propagated
           else
             ListTbl.add bin_nmus nm id
         end
       end;
       (* If a source is propagated, all its binaries should
          be propagated as well *)
       if
         (source_changed || produce_excuses)
           &&
         not (outdated && fucked_arch)
           &&
         not (not outdated && break_arch)
       then
         implies id (source_id p)
           (if outdated then Binary_not_removed else Binary_not_added));
  (* Remove not up to date binaries from sid. The idea is that removing
     the 'Not_yet_build' constraint then makes it possible to test whether
     these packages can be removed without breaking anything. To allow smooth
     updates, libraries in sid are rather replaced by their counterpart in
     testing. This way, the 'Binary_not_propagated' constraint just above
     can always be satisfied when the 'Not_yet_build' constraint is removed. *)
  let is_outdated = StringTbl.create 400 in
  List.iter
    (fun p ->
       StringTbl.add is_outdated p.M.package ();
       if
         allow_smooth_updates p &&
         M.has_package_of_name t' p.M.package
       then
         M.replace_package u' p
           (List.hd (M.find_packages_by_name t' p.M.package))
       else
         M.remove_package u' p)
    st.outdated_binaries;
  M.iter_packages t'
    (fun p ->
       let id = bin_id p in
       let (nm, v) = p.M.source in
       (* Faux packages *)
       if not (StringTbl.mem t.M.s_packages nm) then begin
         (* The source should be fake in unstable as well. *)
         assert
           (not (StringTbl.mem u.M.s_packages nm) || StringTbl.mem is_fake nm);
         StringTbl.add t.M.s_packages nm
           { M.s_name = nm; s_version = v; s_section = "" };
         fake_srcs := (nm, v) :: !fake_srcs;
         StringTbl.add is_fake nm ();
         StringTbl.add st.id_of_source nm !last_id;
         incr last_id;
         assume (source_id p) Unchanged
       end;
       let v' = (StringTbl.find t.M.s_packages nm).M.s_version in
       let source_changed =
         not (same_source_version t u nm) in
       (* We only propagate binary packages with a larger version.
          Faux packages are not propagated. Outdated packages are
          never consider to be unchanged, so that we can test smooth
          upgrades. *)
       if
         not (StringTbl.mem is_outdated p.M.package)
           &&
         (no_new_bin t' u' p.M.package || StringTbl.mem is_fake nm)
       then
         assume id Unchanged
       else begin
         (* Binary packages without source of the same version can
            be removed freely when not needed anymore (these are
            binaries left for smooth update).
            However, when producing hints, we do not allow this, as
            we have no way to communicate the change to britney... *)
         if not (compute_hints ()) && M.compare_version v v' <> 0 then
           ()
         (* We cannot remove a binary without removing its source. *)
         else if source_changed then
           implies (source_id p) id Source_not_propagated
         else
           ListTbl.add bin_nmus nm id
       end;
       (* We cannot remove or upgrade a source package if a
          corresponding binary package still exists.
          We relax this constraint for libraries when upgrading
          a source package. *)
       if
         (source_changed || produce_excuses)
           &&
         not (allow_smooth_updates p && StringTbl.mem u.M.s_packages nm)
           &&
         not break_arch
       then
         implies id (source_id p) Binary_not_removed);
  (* All binaries packages from a same source are propagated
     atomically on any given architecture. *)
  ListTbl.iter
    (fun _ pkgs -> all_or_none pkgs Atomic) bin_nmus;
  (* Clear faked packages (needed when using a single processor). *)
  List.iter
    (fun (nm, _) ->
       StringTbl.remove t.M.s_packages nm;
       StringTbl.remove u.M.s_packages nm;
       StringTbl.remove st.id_of_source nm)
    !fake_srcs;
  (List.rev !l, st.uid, !sources_with_binaries, !fake_srcs, bin_id_count,
   st.bin_of_id)

let arch_constraints = Task.funct arch_constraints

let initial_constraints
      (dates, urgencies, hints, t, u,
       testing_bugs, unstable_bugs, l, id_of_source, source_of_id, src_uid) =
  let init_t = Timer.start () in

  List.iter
    (fun p ->
       let l = Str.split slash p in
       try
         match l with
           [nm; v] -> StringTbl.replace hints.h_remove nm
                        (Deb_lib.parse_version v)
         | [nm]    -> StringTbl.replace hints.h_remove nm
                        (StringTbl.find t.M.s_packages nm).M.s_version
         | _       -> ()
       with Not_found ->
         Format.eprintf "No source package %s.@." (List.hd l);
         exit 1)
    !to_remove;
  let rem_uid = remove_sources true hints.h_remove t u in

  let name_of_id =
    ref [("source", 0, Array.length source_of_id, source_of_id)] in
  let get_name_arch id =
    let (arch, start, len, tbl) =
      List.find
        (fun (arch, start, len, tbl) ->
           id >= start && id < start + len)
        !name_of_id
    in
    (tbl.(id - start), arch)
  in
  let print_package f id =
    let (name, arch) = get_name_arch id in
    if arch = "source" then
      Format.fprintf f "%s" name
    else
      Format.fprintf f "%s/%s" name arch
  in
  let signal_assign r reason =
    if reason <> Unchanged && verbose () then begin
      let id = r.(0) in
      let (nm, arch) = get_name_arch id in
      L.print (new L.format_printer Format.err_formatter)
        (L.s "Skipping " & L.i id & L.s " - " &
         L.s nm & L.s "/" & L.s arch & L.s ": " &
         print_reason' get_name_arch r reason)
    end
  in
  let solver =
    HornSolver.initialize ~signal_assign (Array.length source_of_id) in
  HornSolver.set_var_printer solver print_package;
  let get_bugs src bugs p =
    try StringTbl.find bugs p with Not_found -> StringSet.empty
  in
  let no_new_bugs is_new p =
    if is_new then
      StringSet.is_empty (get_bugs u unstable_bugs p)
    else
      StringSet.subset (get_bugs u unstable_bugs p) (get_bugs t testing_bugs p)
  in
  let new_bugs is_new p =
    if is_new then
      get_bugs u unstable_bugs p
    else
      StringSet.diff (get_bugs u unstable_bugs p) (get_bugs t testing_bugs p)
  in
  let is_unblocked h nm v =
    try M.compare_version (StringTbl.find h nm) v = 0 with Not_found -> false
  in
  let is_blocked nm v =
    ((hints.h_block_all <> None || StringTbl.mem hints.h_block nm) &&
     not (is_unblocked hints.h_unblock nm v))
       ||
    (StringTbl.mem hints.h_block_udeb nm &&
     not (is_unblocked hints.h_unblock_udeb nm v))
  in
  let blocked_reason nm v =
    let unblocked = is_unblocked hints.h_unblock nm v in
    match
      if unblocked then None else
      try
        Some (StringTbl.find hints.h_block nm)
      with Not_found ->
        hints.h_block_all
    with
      Some who -> ("block", who)
    | None     -> ("block-udeb", StringTbl.find hints.h_block_udeb nm)
  in
  let block_constraints = ref [] in
  let age_constraints = ref [] in
  let bug_constraints = ref [] in
  let outdated_constraints = ref [] in
  let produce_excuses = !excuse_file <> "" in
  let implies id1 id2 reason =
    ignore (HornSolver.add_rule solver [|id2; id1|] reason) in
  let assume_deferred lst id reason =
    if produce_excuses then
      lst := (id, reason) :: !lst
    else
      HornSolver.assume solver id reason
  in
  let perform_deferred lst =
    if !lst <> [] then begin
      List.iter (fun (id, reason) -> HornSolver.assume solver id reason)
        !lst;
      lst := [];
      true
    end else
      false
  in
  let deferred_constraints =
    List.map
      (fun lst () -> perform_deferred lst)
      [bug_constraints; outdated_constraints;
       age_constraints; block_constraints]
  in
  let all_or_none ids reason =
    match ids with
      [] ->
        ()
    | id :: rem ->
        List.iter
          (fun id' -> implies id id' reason; implies id' id reason) rem
  in
  let arch_results =
    List.map
      (fun (arch, t) ->
         let fucked_arch =
           List.mem arch
             (try Hashtbl.find options "FUCKED_ARCHES" with Not_found -> [])
         in
         let break_arch =
           List.mem arch
             (try Hashtbl.find options "BREAK_ARCHES" with Not_found -> [])
         in
         (arch,
          arch_constraints t
            (produce_excuses, fucked_arch, break_arch, hints.h_remove)))
      l
  in
  StringTbl.iter
    (fun nm s ->
       let v = s.M.s_version in
       let id = StringTbl.find id_of_source nm in
       (* We only propagate source packages with a larger version *)
       if no_new_source t u nm then
         ignore (HornSolver.add_rule solver [|id|] Unchanged)
       else begin
         (* Do not propagate a source package requested to be blocked *)
         if is_blocked nm v then
           assume_deferred block_constraints id (Blocked (blocked_reason nm v));
         (* Do not propagate a source package if not old enough *)
         let v' = source_version t nm in
         let (cur_ag, req_ag) = compute_ages dates urgencies hints nm v v' in
         if cur_ag < req_ag then
           assume_deferred age_constraints id (Too_young (cur_ag, req_ag));
         (* Do not propagate a source package if it has new bugs *)
         let is_new = v' = None in
         if
           not (no_new_bugs is_new nm && no_new_bugs is_new ("src:" ^ nm))
         then
           assume_deferred bug_constraints id
             (More_bugs (StringSet.union
                           (new_bugs is_new nm)
                           (new_bugs is_new ("src:" ^ nm))))
       end)
    u.M.s_packages;
  StringTbl.iter
    (fun nm s ->
       if not (StringTbl.mem u.M.s_packages nm) then
         try
           let who = StringTbl.find hints.h_block ("-" ^ nm) in
           let id =
             try StringTbl.find id_of_source nm with Not_found -> assert false
           in
           assume_deferred block_constraints id (Blocked ("blocked", who))
         with Not_found ->
           ())
    t.M.s_packages;
  let source_has_binaries = StringTbl.create 8192 in
  let is_fake = StringTbl.create 17 in
  let first_bin_id = Array.length source_of_id in
  let id_offset = ref 0 in
  let id_offsets = StringTbl.create 17 in
  let smooth_uid =
    string_uid (String.concat " " (List.sort compare !smooth_updates)) in
  let uids =
    smooth_uid ::
    rem_uid ::
    src_uid ::
    List.map
      (fun (arch, r) ->
         let (constraints, uid, sources_with_binaries,
              fake_srcs, bin_id_count, bin_of_id) =
           Task.wait r in
(*XXXXX Use ids? *)
         List.iter
           (fun nm ->
              if not (StringTbl.mem source_has_binaries nm) then
                StringTbl.add source_has_binaries nm ())
           sources_with_binaries;
         let last_bin_id = first_bin_id + bin_id_count in
         let last_id = ref (last_bin_id + !id_offset) in
         let id_of_fake = Hashtbl.create 101 in
         let offset id =
           if id < first_bin_id then
             id
           else if id < last_bin_id then
             id + !id_offset
           else
             Hashtbl.find id_of_fake id
         in
         StringTbl.add id_offsets arch (first_bin_id, !id_offset, bin_id_count);
         name_of_id :=
           (arch, first_bin_id + !id_offset, bin_id_count, bin_of_id) ::
           !name_of_id;
         let cur_id = ref last_bin_id in
         let fake_lst = ref [] in
         List.iter
           (fun (nm, v) ->
              if not (StringTbl.mem is_fake nm) then begin
                fake_lst := nm :: !fake_lst;
                StringTbl.add is_fake nm !last_id;
                StringTbl.add id_of_source nm !last_id;
                incr last_id;
                StringTbl.add t.M.s_packages nm
                  { M.s_name = nm; s_version = v; s_section = "" };
              end;
              Hashtbl.add id_of_fake !cur_id (StringTbl.find is_fake nm);
              incr cur_id)
           fake_srcs;
         if !fake_lst <> [] then begin
           let start = last_bin_id + !id_offset in
           let tbl = Array.of_list (List.rev !fake_lst) in
           assert (Array.length tbl = !last_id - start);
           name_of_id :=
             ("source", start, !last_id - start, tbl) :: !name_of_id
         end;
         HornSolver.extend solver !last_id;
         List.iter
           (fun c ->
              match c with
                Assume (id, reason) ->
                  begin match reason with
                    Unchanged ->
                      ignore (HornSolver.add_rule solver [|offset id|] reason)
                  | Too_young _ ->
                      assume_deferred age_constraints (offset id) reason
                  | More_bugs _ ->
                      assume_deferred bug_constraints (offset id) reason
                  | Not_yet_built _ ->
                      assume_deferred outdated_constraints (offset id) reason
                  | Blocked _ | Binary_not_added | Binary_not_removed
                  | No_binary | Conflict _ | Source_not_propagated | Atomic ->
                      assert false
                  end
              | Implies (id1, id2, reason) ->
                  implies (offset id1) (offset id2) reason
              | All_or_none (ids, reason) ->
                  all_or_none (List.map offset ids) reason)
           constraints;
         id_offset := !last_id - first_bin_id;
         uid)
      arch_results
  in
  StringTbl.iter
    (fun nm s ->
       if not (StringTbl.mem source_has_binaries nm) then
         ignore
           (HornSolver.add_rule solver
              [|StringTbl.find id_of_source nm|] No_binary))
    u.M.s_packages;
  load_rules solver uids;
  if debug_time () then
    Format.eprintf "Initial constraints: %f@." (Timer.stop init_t);

  (uids, solver, deferred_constraints, is_fake, id_offsets, get_name_arch)

(**** Find constraints due to co-installability issues ****)

let find_coinst_constraints st (unchanged, check_coinstallability) =
  Gc.set { (Gc.get ()) with Gc.space_overhead = 80 };
  if debug_gc () then begin
    Gc.full_major (); Gc.print_stat stderr; flush stderr
  end;
  let arch = st.arch in
  let (t', u') = get_upgrade_state st unchanged check_coinstallability in
  if debug_coinst () then
    Format.eprintf "==================== %s@." arch;
  let step_t = Timer.start () in
  let problems =
    if check_coinstallability then
      Upgrade_common.find_problematic_packages
        ~check_new_packages:true broken_sets t' u'
        (fun nm -> is_unchanged st unchanged nm)
    else
      Upgrade_common.find_non_inst_packages
        t' u' (fun nm -> is_unchanged st unchanged nm)
  in
  let t = Timer.start () in
  let is_singleton pos =
    StringSet.cardinal pos = 1
      ||
    let source nm =
      match M.find_packages_by_name st.testing nm with
        [p] when not (allow_smooth_updates p) -> Some p.M.source
      | _                                     -> None
    in
    let eq s1 s2 =
      match s1, s2 with
        Some (nm1, v1), Some (nm2, v2) ->
          nm1 = nm2 && M.compare_version v1 v2 = 0
      | _ ->
          false
    in
    let src = source (StringSet.choose pos) in
    StringSet.for_all (fun nm -> eq (source nm) src) pos
  in
  let has_singletons =
    List.exists (fun (cl, _, _) -> is_singleton cl.Upgrade_common.pos) problems
  in
  let changes = ref [] in
  List.iter
    (fun ({Upgrade_common.pos = pos;  neg = neg}, s, expl) ->
       let singleton = is_singleton pos in
       if singleton || not has_singletons then begin

(*
let arch_all dist p = match M.find_packages_by_name dist p with [] -> true | [p] -> p.M.architecture = "all" | _ -> assert false in
StringSet.iter
 (fun nm -> if arch_all st.testing nm && arch_all st.unstable nm then Format.eprintf "IGN %s/%s@." nm arch) s;
*)    

         let to_ids s =
           StringSet.fold
             (fun nm s -> IntSet.add (StringTbl.find st.id_of_bin nm) s)
             s IntSet.empty
         in
         let neg = to_ids neg in
         let s' = to_ids s in
         let id = StringTbl.find st.id_of_bin (StringSet.choose pos) in
         let r = Array.of_list (id :: IntSet.elements neg) in
         let can_learn = singleton in
         changes := (r, neg, s', s, expl, can_learn) :: !changes
       end)
    problems;
  if debug_time () then begin
    Format.eprintf "  New constraints: %f@." (Timer.stop t);
    Format.eprintf "Step duration: %f@." (Timer.stop step_t)
  end;
  List.rev !changes

let find_coinst_constraints = Task.funct find_coinst_constraints

let find_all_coinst_constraints solver id_offsets l =
  let t = Timer.start () in
  let a = Array.of_list l in
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
        (find_coinst_constraints st
           (extract_unchanged_bin
              solver id_offsets arch (HornSolver.assignment solver),
            !check_coinstallability))
        (fun changes -> stop c i changes);
      if !n < max_proc then begin
        start c 0 0
      end
    end
  and stop c i changes =
    if changes <> [] then begin
      Array.fill changed 0 c true;
      let (_, offset, _) = StringTbl.find id_offsets (fst a.(i)) in
      List.iter
        (fun (r, neg, s, s', expl, can_learn) ->
           let r = Array.map (fun id -> id + offset) r in
           let offset_set ids =
             IntSet.fold (fun id s -> IntSet.add (id + offset) s)
               ids IntSet.empty
           in
           let neg = offset_set neg in
           let s = offset_set s in
           let r' =
             HornSolver.add_rule solver r (Conflict (neg, s, s', expl)) in
           if IntSet.cardinal s > 1 then coinst_rules := r' :: !coinst_rules;
           if can_learn then learn_rule r neg s s' expl)
        changes
    end;
    running.(i) <- false;
    decr n;
    start c i i
  in
(*
  start 1 0 0;
  Task.run scheduler;
*)
  start c 0 0;
  Task.run scheduler;
  if debug_time () then
    Format.eprintf "Solving constraints: %f@." (Timer.stop t)

(**** Output all changes (for debugging) ****)

let output_arch_changes st unchanged =
  let arch = st.arch in
  let t' = st.testing in
  let u' = st.unstable in
  M.iter_packages t'
    (fun p ->
       let nm = p.M.package in
       let v = p.M.version in
       if not (is_unchanged st unchanged nm) then
         match bin_version u' nm with
           Some v' ->
             Format.eprintf
               "Upgrade binary package %s/%s from %a to %a@."
               nm arch M.print_version v M.print_version v'
         | None ->
             Format.eprintf "Remove binary package %s/%s@." nm arch);
  M.iter_packages u'
    (fun p ->
       let nm = p.M.package in
       if not (is_unchanged st unchanged nm) then
         if not (M.has_package_of_name t' nm) then
           Format.eprintf "Adding binary package %s/%s@." nm arch)

let output_arch_changes = Task.funct output_arch_changes

let output_outcome solver id_of_source id_offsets t u l unchanged =
  let is_unchanged src =
    BitVect.test unchanged (StringTbl.find id_of_source src) in
  StringTbl.iter
    (fun nm s ->
       if not (is_unchanged nm) then
         try
           let s' = StringTbl.find u.M.s_packages nm in
           Format.eprintf "Upgrade source package %s from %a to %a@." nm
             M.print_version s.M.s_version M.print_version s'.M.s_version
         with Not_found ->
           Format.eprintf "Remove source package %s@." nm)
    t.M.s_packages;
  StringTbl.iter
    (fun nm v ->
       if
         not (StringTbl.mem t.M.s_packages nm || is_unchanged nm)
       then
         Format.eprintf "Adding source package %s@." nm)
    u.M.s_packages;

  List.iter
    (fun (arch, st) ->
       Task.wait (output_arch_changes st
                    (extract_unchanged_bin solver id_offsets arch unchanged)))
    l

(**** Hint output ****)

let cluster_packages st (unchanged, clusters, check_coinstallability) =
  let clusters =
    List.map (fun (lst, id) -> (lst, (id, Union_find.elt id))) clusters
  in
  let merge (_, e1) (_, e2) = Union_find.merge e1 e2 min in
  let (t, u) = get_upgrade_state st unchanged check_coinstallability in
  Upgrade_common.find_clusters t u
    (fun nm -> is_unchanged st unchanged nm) clusters merge;
  List.map (fun (_, (id, elt)) -> (id, Union_find.get elt)) clusters

let cluster_packages = Task.funct cluster_packages

type 'a easy_hint =
  { mutable h_names : 'a list;
    mutable h_pkgs : (string * string) list;
    mutable h_live : bool;
    h_id : int }

let generate_small_hints solver id_offsets l buckets subset_opt =
  let to_consider = ref [] in
  let buckets_by_id = Hashtbl.create 17 in
  let n = ref 0 in
  ListTbl.iter
    (fun (src, arch) lst ->
       let info =
         { h_names = [((src, arch), List.for_all snd lst)];
           h_pkgs = List.map fst lst; h_live = true; h_id = !n }
       in
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
       let unchanged =
         extract_unchanged_bin
           solver id_offsets arch (HornSolver.assignment solver)
       in
       let clusters = ref [] in
       List.iter
         (fun (info, elt) ->
            let l =
              List.filter (fun (_, arch') -> arch = arch') info.h_pkgs in
            if l <> [] then
              clusters :=
                (List.map (fun (nm, _) -> nm) l, (Union_find.get elt).h_id)
                :: !clusters)
         !to_consider;
       cluster_packages st (unchanged, !clusters, !check_coinstallability))
    (fun lst ->
       List.iter
         (fun (id, id') ->
            merge (Hashtbl.find buckets_by_id id)
                  (Hashtbl.find buckets_by_id id'))
         lst);

  let compare_elt = Util.compare_pair compare compare in
  !to_consider
  >> List.map fst
  >> List.filter (fun info -> info.h_live)
  >> List.map
       (fun info ->
          info.h_names
          >> List.filter (fun (_, hide) -> not hide)
          >> List.map fst
          >> List.sort compare_elt)
  >> List.filter (fun l -> l <> [])
  >> List.sort (Util.compare_list compare_elt)
  >> List.stable_sort (fun l l' -> compare (List.length l) (List.length l'))

let collect_changes st (unchanged, subset) =
  let changes = ref [] in
  let u' = st.unstable in
  let t' = st.testing in
  M.iter_packages u'
    (fun p ->
       let nm = p.M.package in
       if not (is_unchanged st unchanged nm) then begin
         let (src, v) = p.M.source in
         changes := (src, nm, is_unchanged st subset nm) :: !changes
       end);
  M.iter_packages t'
    (fun p ->
       let nm = p.M.package in
       if
         not (is_unchanged st unchanged nm)
           &&
         not (M.has_package_of_name u' nm)
       then begin
         let (src, v) = p.M.source in
         changes := (src, nm, is_unchanged st subset nm) :: !changes
       end);
  List.rev !changes

let collect_changes = Task.funct collect_changes

let generate_hints
      solver id_of_source id_offsets t u l extra_lines pkg_opt subset_opt =
  let hint_t = Timer.start () in
  let unchanged = HornSolver.assignment solver in
  let subset = match subset_opt with Some s -> s | None -> unchanged in
  let changes = ListTbl.create 101 in
  Task.iteri l
    (fun (arch, st) ->
       (arch,
        collect_changes st
          (extract_unchanged_bin solver id_offsets arch unchanged,
           extract_unchanged_bin solver id_offsets arch subset)))
    (fun arch lst ->
       List.iter
         (fun (src, nm, hide) -> ListTbl.add changes src ((nm, arch), hide))
         lst);
  let buckets = ListTbl.create 101 in
  let is_unchanged src =
    BitVect.test unchanged (StringTbl.find id_of_source src) in
  ListTbl.iter
    (fun src l ->
       if not (is_unchanged src) then
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
    generate_small_hints solver id_offsets l buckets subset_opt in
  if debug_time () then
    Format.eprintf "Generating hints: %f@." (Timer.stop hint_t);
  let print_pkg f src arch =
    try
      let vers = (StringTbl.find u.M.s_packages src).M.s_version in
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
      let vers = (StringTbl.find t.M.s_packages src).M.s_version in
      Format.fprintf f " -%s/%a" src M.print_version vers
  in
  let print_hint f l =
    let should_show =
      (!all_hints || List.length l > 1 || subset_opt <> None)
          &&
      match pkg_opt with
        Some pkg -> List.mem_assoc pkg l
      | None     -> true
    in
    if should_show then begin
      Format.fprintf f "easy";
      List.iter (fun (src, arch) -> print_pkg f src arch) l;
      Format.fprintf f "@."
    end
  in
  let print_hints f =
    List.iter (fun l -> Format.fprintf f "%s@." l) extra_lines;
    List.iter (fun names -> print_hint f names) hints
  in
  if debug_hints () || !hint_file = "-" then print_hints Format.std_formatter;
  if !hint_file <> "" && !hint_file <> "-" then begin
    let ch = open_out !hint_file in
    print_hints (Format.formatter_of_out_channel ch);
    close_out ch
  end

(**** Heidi file output ****)

let heidi_buffer = Buffer.create 80

let heidi_line lines nm vers arch sect =
  Format.bprintf heidi_buffer "%s %a %s %s@."
    nm M.print_version vers arch sect;
  lines := Buffer.contents heidi_buffer :: !lines;
  Buffer.clear heidi_buffer

let heidi_arch st unchanged =
  let lines = ref [] in
  let t = st.testing in
  let u = st.unstable in
  M.iter_packages t
    (fun p ->
       let nm = p.M.package in
       let sect = if p.M.section = "" then "faux" else p.M.section in
       if is_unchanged st unchanged nm then
         heidi_line lines nm p.M.version p.M.architecture sect);
  M.iter_packages u
    (fun p ->
       let nm = p.M.package in
       let sect = if p.M.section = "" then "faux" else p.M.section in
       if not (is_unchanged st unchanged nm) then
         heidi_line lines nm p.M.version p.M.architecture sect);
  String.concat "" (List.sort compare !lines)

let heidi_arch = Task.funct heidi_arch

let print_heidi solver id_of_source id_offsets fake_src l t u =
  let ch = if !heidi_file = "-" then stdout else open_out !heidi_file in
  let heidi_t = Timer.start () in
  Task.iter_ordered
    (List.sort (fun (arch, _) (arch', _) -> compare arch arch') l)
    (fun (arch, st) ->
       heidi_arch st (extract_unchanged_bin
                        solver id_offsets arch (HornSolver.assignment solver)))
    (fun lines -> output_string ch lines);
  let unchanged = HornSolver.assignment solver in
  let is_unchanged src =
    BitVect.test unchanged (StringTbl.find id_of_source src) in
  let source_sect nm s =
    if StringTbl.mem fake_src nm then "faux"
    else if s.M.s_section = "" then "unknown"
    else s.M.s_section
  in
  let lines = ref [] in
  StringTbl.iter
    (fun nm s ->
       let sect = source_sect nm s in
       if is_unchanged nm then
         heidi_line lines nm s.M.s_version "source" sect)
    t.M.s_packages;
  StringTbl.iter
    (fun nm s ->
       let sect = source_sect nm s in
       if not (is_unchanged nm) then
         heidi_line lines nm s.M.s_version "source" sect)
    u.M.s_packages;
  List.iter (output_string ch) (List.sort compare !lines);
  if !heidi_file <> "-" then close_out ch;
  if debug_time () then
    Format.eprintf "Writing Heidi file: %f@." (Timer.stop heidi_t)

(**** Migration analyze ****)

let rec collect_reasons solver get_name_arch print_package id =
  match HornSolver.reason solver id with
    None ->
      let l = HornSolver.assumptions solver id in
      L.s "Package " & L.format print_package id & L.s ": " &
      print_reason' get_name_arch [|id|] (List.hd (List.rev l))
  | Some (l, r) ->
      let cur =
        L.s "Package " & L.format print_package id & L.s ": " &
        print_reason' get_name_arch l r
      in
      let len = Array.length l in
      let rem sep =
        L.list
          (fun id ->
             sep (collect_reasons solver get_name_arch print_package id))
          (Array.to_list (Array.sub l 1 (len - 1)))
      in
      if len <= 2 then
        (cur & rem (fun x -> L.p & x))
      else
        (cur & L.ul (rem L.li))

let rec collect_assumptions solver id =
  match HornSolver.reason solver id with
    None ->
      IntSet.singleton id
  | Some (l, _) ->
      let s = ref IntSet.empty in
      for i = 1 to Array.length l - 1 do
        s := IntSet.union (collect_assumptions solver l.(i)) !s
      done;
      !s

let analyze_migration
      uids solver id_of_source id_offsets t u l get_name_arch nm =
  let id =
    try
      StringTbl.find id_of_source nm
    with Not_found ->
      Format.eprintf "Unknown package %s@." nm;
      -1
  in
  let assign = HornSolver.assignment solver in
  if debug_migration () then
    Format.eprintf "%s (%d) : %b@." nm id (BitVect.test assign id);
  let lst = ref [] in
  let print_package f id =
    let (name, arch) = get_name_arch id in
    if arch = "source" then
      Format.fprintf f "%s" name
    else
      Format.fprintf f "%s/%s" name arch
  in
  let output_hints () =
    let source_bugs = StringTbl.create 17 in
    List.iter
      (fun (p, reason) ->
         match reason with
           More_bugs s ->
             let (src, arch) = get_name_arch p in
             if arch = "source" then StringTbl.add source_bugs src s
         | _ ->
           ())
      !lst;
    let lst =
      let b = Buffer.create 80 in
      List.map
        (fun (p, reason) ->
           Buffer.clear b;
           begin match reason with
             Not_yet_built (nm, _, _) ->
               Format.bprintf b "# remove outdated binary package %a"
                 print_package p
           | Blocked (kind, _) ->
               let (src, _) = get_name_arch p in
               let vers = (StringTbl.find u.M.s_packages src).M.s_version in
               Format.bprintf b "un%s %s/%a" kind src M.print_version vers
           | Too_young (cur_ag, _) ->
               let (src, _) = get_name_arch p in
               let vers = (StringTbl.find u.M.s_packages src).M.s_version in
               Format.bprintf b "age-days %d %s/%a"
                 cur_ag src M.print_version vers
           | More_bugs s ->
               let print_bugs =
                 Util.print_list (fun f s -> Format.fprintf f "#%s" s) ", "
               in
               let (nm, arch) = get_name_arch p in
               if arch = "source" then begin
                 let vers = (StringTbl.find u.M.s_packages nm).M.s_version in
                 Format.bprintf b "# source package %s/%a: fix bugs %a"
                   nm M.print_version vers print_bugs (StringSet.elements s)
               end else begin
                 let s =
                   try
                     StringSet.diff s (StringTbl.find source_bugs nm)
                   with Not_found ->
                     StringTbl.add source_bugs nm s;
                     s
                 in
                 if not (StringSet.is_empty s) then begin
                   Format.bprintf b "# binary package %s: fix bugs %a"
                     nm print_bugs (StringSet.elements s)
                 end
               end
           | Conflict _ | Atomic | Source_not_propagated | No_binary
           | Binary_not_added | Binary_not_removed | Unchanged ->
               assert false
           end;
           Buffer.contents b)
        (List.rev !lst)
    in
    let lst = List.filter (fun s -> s <> "") lst in
    if !hint_file = "" then hint_file := "-";
    generate_hints solver id_of_source id_offsets t u l lst (Some nm) None
  in
  let rec migrate () =
    if BitVect.test assign id then begin
      let s = collect_assumptions solver id in
      if IntSet.is_empty s then begin
        L.print (new L.format_printer Format.std_formatter)
          (L.s "Package " & L.s nm & L.s " cannot migrate:" & L.p &
           L.ul ~prefix:"  "
             (L.li (collect_reasons solver get_name_arch print_package id)))
      end else begin
        if debug_migration () then
          L.print (new L.format_printer Format.err_formatter)
            (collect_reasons solver get_name_arch print_package id);
        let p = IntSet.choose s in
        let ass = HornSolver.assumptions solver p in
        lst := List.rev_append (List.map (fun reason -> (p, reason)) ass) !lst;
        if debug_migration () then begin
          L.print (new L.format_printer Format.err_formatter)
            (L.s "Need the following:" &
             L.ul
               (L.list
                  (fun r ->
                     L.li (L.format print_package p & L.s ": " &
                           print_reason' get_name_arch [|p|] r))
                  ass))
        end;
        HornSolver.retract_assumptions solver p;
        migrate ()
      end
    end else begin
      (* We need to check whether there are additional constraints to
         consider.  First clear the state: we may have removed some
         constraints, so we may have to consider a larger set of
         packages. *)
      clear_upgrade_states l;
      find_all_coinst_constraints solver id_offsets l;
      if BitVect.test assign id then
        migrate ()
      else begin
        if !lst = [] then
          Format.printf "The package %s can already migrate.@." nm
        else
          Format.printf "Successful:@.";
        output_hints ()
      end
    end
  in
  if id > 0 then begin
    migrate ();
    save_rules uids
  end

(**** Main part of the program ****)

let print_equivocal_packages uids solver id_of_source id_offsets t u l =
  assert !check_coinstallability;
  find_all_coinst_constraints solver id_offsets l;
  let coinst_unchanged = BitVect.copy (HornSolver.assignment solver) in
  switch_to_installability solver;
  clear_upgrade_states l;
  find_all_coinst_constraints solver id_offsets l;
  save_rules uids;
  let inst_unchanged = HornSolver.assignment solver in
  assert (BitVect.implies inst_unchanged coinst_unchanged);
  let equivocal_pkgs =
    BitVect.(lor) (BitVect.lnot coinst_unchanged) inst_unchanged in
  if debug_outcome () then
    output_outcome solver id_of_source id_offsets t u l equivocal_pkgs;
  if !hint_file = "" then hint_file := "-";
  generate_hints solver id_of_source id_offsets t u l
    ["# equivocal packages:"] None (Some equivocal_pkgs)

let f () =
  Util.enable_messages false;
  let (dates, urgencies, hints, t, u, testing_bugs, unstable_bugs, l,
       id_of_source, source_of_id, src_uid) as info =
    load_all_files () in

  if !equivocal then check_coinstallability := true;

  let (uids, solver, deferred_constraints, is_fake, id_offsets, get_name_arch) =
    initial_constraints info in

  if !equivocal then
    print_equivocal_packages uids solver id_of_source id_offsets t u l
  else begin match !to_migrate with
    Some p ->
      analyze_migration
        uids solver id_of_source id_offsets t u l get_name_arch p
  | None ->
      find_all_coinst_constraints solver id_offsets l;
      List.iter
        (fun f ->
           if f () then
             find_all_coinst_constraints solver id_offsets l)
        deferred_constraints;
      save_rules uids;

      if debug_outcome () then
        output_outcome solver id_of_source id_offsets t u l
          (HornSolver.assignment solver);

      if compute_hints () then
        generate_hints solver id_of_source id_offsets t u l [] None None;

      if !heidi_file <> "" then
        print_heidi solver id_of_source id_offsets is_fake l t u;

      if !excuse_file <> "" then
        output_reasons l dates urgencies hints
          solver source_of_id id_offsets !excuse_file t u
  end;

  List.iter (fun (_, t) -> Task.kill t) l

(**** Parsing of configuration settings ****)

let read_conf f =
  let ch = open_in f in
  begin try
    while true do
      let l = input_line ch in
      let l = Str.split whitespaces l in
      match l with
        []                      -> ()
      | s :: _ when s.[0] = '#' -> ()
      | k :: "=" :: l           -> Hashtbl.replace options k l
      | _                       -> assert false
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

let comma_re = Str.regexp "[ \t]*,[ \t]*"

let _ =
let spec =
  Arg.align
  ["--update",
   Arg.Unit (fun () -> update_data := true),
   " Update data";
   "--input",
   Arg.String (fun d -> dir := d),
   "DIR Select directory containing britney data";
   "--arches",
   Arg.String (fun a -> archs := Str.split comma_re (Util.trim a)),
   "LST Comma-separated list of arches to consider (default to all)";
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
   "--svg",
   Arg.Unit (fun () -> svg := true),
   " Include conflict graphs (in SVG) in excuse output";
   "--migrate",
   Arg.String (fun p -> to_migrate := Some p),
   "PKG Explain what it takes to migrate PKG";
   "--equivocal",
   Arg.Unit (fun () -> equivocal := true),
   " List packages whose behavior depends on the migration policy";
   "--offset",
   Arg.Int (fun n -> offset := n),
   "N Move N days into the future";
   "--inst",
   Arg.Unit (fun () -> check_coinstallability := false),
   " Check for single package installability only";
   "--remove",
   Arg.String (fun p -> to_remove := p :: !to_remove),
   "PKG Attempt to remove the source package PKG";
   "--break",
   Arg.String (Upgrade_common.allow_broken_sets broken_sets),
   "SETS Allows sets of packages to be broken by the migration";
   "-c",
   Arg.String read_conf,
   "FILE Read britney config FILE";
   "--config",
   Arg.String read_conf,
   "FILE Read britney config FILE";
   "--no-cache",
   Arg.Unit (fun () -> Cache.set_disabled true),
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
let msg =
  "Usage: " ^ Sys.argv.(0) ^ " OPTIONS\n\
   Computes which packages can migrate from sid to testing.\n\
   Takes as input either a britney data directory (option -input)\n\
   or a britney config file (option -c).\n\
   \n\
   Options:"
in
Arg.parse spec (fun p -> ()) msg;
if
  !dir = default_dir &&
  not (try Sys.is_directory !dir with Sys_error _ -> false)
then begin
  Arg.usage spec msg;
  Format.eprintf
    "@.Please use '--input' or '-c' option to indicate \
       the location of britney data.@.";
  exit 1
end;
let opts =
  [!to_migrate <> None, "--migrate";
   !excuse_file <> "", "--excuse";
   !equivocal, "--equivocal";
   !update_data, "--update"]
in
begin match List.filter (fun (b, _) -> b) opts with
  (_, o1) :: (_, o2) :: _ ->
    Format.eprintf "Incompatible options %s and %s.@." o1 o2;
    exit 1
| _ ->
    ()
end;
if
  !heidi_file = "" && !hint_file = "" && !excuse_file = "" &&
  !to_migrate = None && not !equivocal && not !update_data
then
  Format.eprintf "Warning: no output option has been provided.@.";
if !update_data then begin
  Update_data.f
    (testing ()) (unstable ()) !archs
    (Filename.concat (unstable ()) "Hints") (hint_files ())
end else
  f ()

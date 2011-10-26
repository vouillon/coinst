(*
XXXX List broken new packages!

- Does it make sense to consider new packages as previously
  installable, and report issues for them in a uniform way?

- Be smarter when deciding not to upgrade a package
  when there are disjunctions

- Bug when reporting an empty set of issues

- Do not collapse updated, non-updated, and new packages
- Iterate until there is no problematic packages
- Print equivalence classes

- We might miss some configurations (because we generate only
  non-minimal supersets)
- Il doit être possible d'aller beaucoup plus vite en ne considérant
  que des paquets intéressants (en particulier, au-dessus de nouvelles
  dependences)...
- Reflattening is quite slow...
*)

let (file1,file2) =
("snapshots/updates/stable", "snapshots/updates/testing")
(*
("snapshots/updates/oldstable", "snapshots/updates/stable")
("/tmp/last_month", "/tmp/new")
*)

(****)

let _ =
Gc.set {(Gc.get ())
        with Gc.space_overhead = 300; Gc.max_overhead = 1000000}

let debug = false

module M = Deb_lib
module Repository = Upgrade_common.Repository
open Repository
module Quotient = Quotient.F (Repository)
module Graph = Graph.F (Repository)

module PSetSet = Upgrade_common.PSetSet
module PSetMap = Map.Make (PSet)

(****)

let output_conflicts filename dist2 results =
  let in_conflict p p' =
    p <> p' && PSetSet.exists (fun s -> PSet.mem p s && PSet.mem p' s) results
  in
  let involved = PSet.elements (PSetSet.fold PSet.union results PSet.empty) in
  let partition =
    List.fold_left
      (fun l p ->
         List.fold_left
           (fun l s ->
              let (s1, s2) = List.partition (fun p' -> in_conflict p p') s in
              let l = if s1 = [] then l else s1 :: l in
              let l = if s2 = [] then l else s2 :: l in
              l)
           [] l)
      [involved] involved
  in
  let classes = Hashtbl.create 101 in
  let repr = Hashtbl.create 101 in
  List.iter
    (fun s ->
       let p = List.hd s in
       Hashtbl.add classes p s;
       List.iter (fun q -> Hashtbl.add repr q p) s)
    partition;
  let results =
    PSetSet.filter (fun s -> PSet.for_all (fun p -> Hashtbl.find repr p = p) s)
      results
  in
  (*
  List.iter
    (fun s ->
       Format.printf "Set ";
       List.iter (fun p -> Format.printf " %a" (Package.print_name dist2) p) s;
       Format.printf "@.")
    partition
  *)
  PSetSet.iter
    (fun s ->
       let start = ref true in
       PSet.iter
         (fun p ->
            if not !start then Format.printf ", ";
            start := false;
            let l = Hashtbl.find classes p in
            let start = ref true in
            List.iter
              (fun p ->
                 if not !start then Format.printf " | ";
                 start := false;
                 Format.printf "%a" (Package.print_name dist2) p)
              l)
         s;
      Format.printf "@.")
    results;

  let ch = open_out filename in
  let f = Format.formatter_of_out_channel ch in
  Format.fprintf f "digraph G {@.";
  Format.fprintf f "rankdir=LR;@.";
  (*Format.fprintf f "overlap=false;@.";*)
  (*Format.fprintf f "ratio=1.4;@.margin=5;@.ranksep=3;@.";*)
  Format.fprintf f "node[fontsize=8];@.";
  Format.fprintf f "node[margin=0,0];@.";
  Format.fprintf f "node[height=0.2];@.";
  Format.fprintf f "node [style=rounded];@.";

  let confl_style = ",color=red" in
  let confl_clique_style = ",color=red,fontcolor=red" in
  let n = ref 0 in
  PSetSet.iter
    (fun s ->
       if PSet.cardinal s = 2 then begin
         let i = PSet.choose s in
         let j = PSet.choose (PSet.remove i s) in
         Format.fprintf f "%d -> %d [dir=none%s];@."
           (Package.index i) (Package.index j) confl_style
       end else begin
         incr n;
         Format.fprintf f
           "confl%d [label=\" \",shape=circle%s];@."
           !n confl_clique_style;
         PSet.iter
           (fun i ->
              Format.fprintf f
                "%d -> confl%d [dir=none%s];@."
                (Package.index i) !n confl_style)
           s
       end)
    results;

  List.iter
    (fun i ->
       if Hashtbl.find repr i = i then begin
         let print_name f i =
           let l = List.length (Hashtbl.find classes i) in
           if l = 1 then Package.print_name dist2 f i else
           Format.fprintf f "%a (x %d)" (Package.print_name dist2) i l
         in
         Format.fprintf f
           "%d [label=\"%a\",style=\"filled\",\
                fillcolor=\"0.0,%f,1.0\"];@."
           (Package.index i) print_name i 0.
       end)
    involved;

  Format.fprintf f "}@.";
  close_out ch

(****)

module StringSet = Upgrade_common.StringSet

module F = struct

  let overlaps s s' = StringSet.exists (fun nm -> StringSet.mem nm s) s'

  let _true = (StringSet.empty, [])

  let is_true (s, l) = StringSet.is_empty s && l = []

  let conj1 (s, l) s' =
    if StringSet.cardinal s' = 1 then begin
      let nm = StringSet.choose s' in
      if StringSet.mem nm s then
        (s, l)
      else
        (StringSet.add nm s,
         List.filter (fun s -> not (StringSet.mem nm s)) l)
    end else begin
      if
        overlaps s s'
          ||
        List.exists (fun s -> StringSet.subset s s') l
      then
        (s, l)
      else
        (s, s' :: List.filter (fun s -> not (StringSet.subset s' s)) l)
    end

  let conj (s1, l1) (s2, l2) =
    let l1 = List.filter (fun s -> not (overlaps s2 s)) l1 in
    let l2 = List.filter (fun s -> not (overlaps s1 s)) l2 in
    let l1 =
      List.filter
        (fun s1 -> not (List.exists (fun s2 -> StringSet.subset s2 s1) l2)) l1
    in
    let l2 =
      List.filter
        (fun s2 -> not (List.exists (fun s1 -> StringSet.subset s1 s2) l1)) l2
    in
    (StringSet.union s1 s2, l1 @ l2)

  let print ch (s, l) =
    if is_true (s, l) then Format.fprintf ch "TRUE" else begin
      Util.print_list (fun ch nm -> Format.fprintf ch "%s" nm) ", " ch
        (StringSet.elements s);
      if not (StringSet.is_empty s) && l <> [] then Format.fprintf ch ", ";
      Util.print_list
        (fun ch s ->
           Util.print_list (fun ch nm -> Format.fprintf ch "%s" nm) " | " ch
             (StringSet.elements s))
        ", " ch l
    end

end

(****)

let read_data ignored_packages ic =
  let dist = M.new_pool () in
  M.parse_packages dist ignored_packages ic;
  M.only_latest dist

let f file1 file2 output_dir =
let dist1 = read_data [] (File.open_in file1) in
let dist2 = read_data [] (File.open_in file2) in

let dist1_state = Upgrade_common.prepare_analyze dist1 in
let (deps1, deps2, pred, st2,
     results, all_pkgs, all_conflicts, dep_src, graphs, _) =
  Upgrade_common.analyze dist1_state dist2
in

(****)

Format.eprintf "Outputting results...@.";

Util.make_directories (Filename.concat output_dir "index.html");
let ch = open_out (Filename.concat output_dir "index.html") in
let f = Format.formatter_of_out_channel ch in

(****)

Format.fprintf f "<h1>Upgrade issues</h1>@.";
Format.fprintf f "<h2>Graph of new conflicts</h2>@.";
output_conflicts (Filename.concat output_dir "conflicts.dot") dist2 results;
let basename = Filename.concat output_dir "conflicts" in
ignore
  (Sys.command
     (Format.sprintf "dot %s.dot -Tpng -o %s.png" basename basename));
Format.fprintf f "<p><img src=\"conflicts.png\" alt=\"conflicts\"/></p>@.";
(*
ignore
  (Sys.command
     (Format.sprintf "dot %s.dot -Tsvg -o %s.svg" basename basename));
Format.fprintf f
  "<p><object data=\"conflicts.svg\" type=\"image/svg+xml\"></object></p>@.";
*)

(****)
let t = Unix.gettimeofday () in
Format.printf "Preparing explanations...@.";

let involved = PSet.elements all_pkgs in
let partition =
  List.fold_left
    (fun l p ->
       List.fold_left
         (fun l s ->
            let (s1, s2) = List.partition (fun p' -> Conflict.check all_conflicts p p') s in
            let l = if s1 = [] then l else s1 :: l in
            let l = if s2 = [] then l else s2 :: l in
            l)
         [] l)
    [involved] involved
in
let partition =
  List.fold_left
    (fun l p ->
       List.fold_left
         (fun l s ->
            let (s1, s2) =
              List.partition (fun p' -> PSet.mem p' (PTbl.get dep_src p)) s in
            let l = if s1 = [] then l else s1 :: l in
            let l = if s2 = [] then l else s2 :: l in
            l)
         [] l)
    partition involved
in
let partition =
  List.fold_left
    (fun l p ->
       List.fold_left
         (fun l s ->
            let (s1, s2) =
              List.partition (fun p' -> PSet.mem p (PTbl.get dep_src p')) s in
            let l = if s1 = [] then l else s1 :: l in
            let l = if s2 = [] then l else s2 :: l in
            l)
         [] l)
    partition involved
in
let classes = Hashtbl.create 101 in
let repr = Hashtbl.create 101 in
List.iter
  (fun s ->
     let p = List.hd s in
     Hashtbl.add classes p s;
     List.iter (fun q -> Hashtbl.add repr q p) s)
  partition;
let prob_pkgs = ref PSetMap.empty in
let graphs =
  List.filter
    (fun {Upgrade_common.i_issue = s; i_clause  = {Upgrade_common.pos = pos}} ->
       let s' =
         PSet.fold (fun p s' -> PSet.add (Hashtbl.find repr p) s') s PSet.empty
       in
       prob_pkgs :=
         PSetMap.add s'
           (F.conj1
              (try PSetMap.find s' !prob_pkgs with Not_found -> F._true)
              pos)
           !prob_pkgs;
       PSet.for_all
         (fun p -> try Hashtbl.find repr p = p with Not_found -> false) s)
    graphs
in
Format.printf "Preparing explanations... %fs@." (Unix.gettimeofday () -. t);

let t = Unix.gettimeofday () in
Format.printf "Generating explanations...@.";
Format.fprintf f "<h2>Explanations of conflicts</h2>@.";
List.iter
  (fun { Upgrade_common.i_issue = s;
         Upgrade_common.i_graph =
           { Upgrade_common.g_nodes = pkgs;
             g_deps = deps; g_confl = confl }} ->
(*Task.async (fun () ->*)
     let quotient = Quotient.from_partition dist2 pkgs partition in
     let deps = Quotient.dependencies quotient deps in
     let confl = Quotient.conflicts quotient confl in
     let nm =
       String.concat ","
         (List.map (fun p -> M.package_name dist2 (Package.index p))
            (PSet.elements s))
     in
     let basename = Filename.concat output_dir nm in
     let edge_color p2 _ d2 =
       let i1 = PTbl.get pred p2 in
       let is_new =
         i1 <> -1 &&
         let d1 =
           Disj.fold
             (fun p2 d2 ->
                let i = PTbl.get pred p2 in
                if i = -1 then d2 else
                Disj.disj (Disj.lit (Package.of_index i)) d2)
             d2 Disj._false
         in
         not (Formula.implies1 (PTbl.get deps1 (Package.of_index i1)) d1)
       in
       if is_new then
         Some "violet"
       else
         Some "blue"
     in
     let package_weight p =
       let i = PTbl.get pred p in
       if i = -1 then 2. else begin
         let name dist p =
           let b = Buffer.create 16 in
           Format.fprintf (Format.formatter_of_buffer b) "%a@?"
             (Package.print dist) p;
           Buffer.contents b
         in
         if name dist2 p = name dist1 (Package.of_index i) then 1. else 10.
       end
     in
     Graph.output (basename ^ ".dot")
       ~options:["rankdir=LR;"; "node[fontsize=8];"; "node[margin=\"0.05,0\"];"; "node[height=0.2];"]
       ~edge_color
       ~package_weight
       ~package_emph:(fun p -> PSet.mem p s)
       ~mark_all:true quotient deps confl;
     ignore
       (Sys.command
          (Format.sprintf "dot %s.dot -Tpng -o %s.png" basename basename));
     Format.fprintf f "<p><img src=\"%s.png\" alt=\"%s\"/></p>@." nm nm;
(*
     ignore
       (Sys.command
          (Format.sprintf "dot %s.dot -Tsvg -o %s.svg" basename basename));
     Format.fprintf f
       "<p><object data=\"%s.svg\" type=\"image/svg+xml\"></object></p>@." nm;
*)
     let ppkgs = PSetMap.find s !prob_pkgs in
     if not (F.is_true ppkgs) then begin
       Format.fprintf f "<p><b>Problematic packages:</b> %a</p>@."
         F.print ppkgs
     end)
  (List.sort
     (fun {Upgrade_common.i_issue = s} {Upgrade_common.i_issue = s'} ->
        - compare (PSet.cardinal s) (PSet.cardinal s'))
     graphs);

let ppkgs =
  PSetMap.fold (fun _ s s' -> F.conj s s') !prob_pkgs F._true
in
Format.fprintf f "<p><b>Full list of problematic packages:</b> %a</p>@."
  F.print ppkgs;

close_out ch;
Format.printf "Generating explanations... %fs@." (Unix.gettimeofday () -. t)

(****)

let _ =
let output_dir = ref "/tmp/upgrade" in
let l = ref [] in
let spec =
  Arg.align
  ["-o",
   Arg.String (fun d -> output_dir := d),
   "DIR       Write output to directory DIR"]
in
Arg.parse spec (fun f -> l := f :: !l)
  ("Usage: " ^ Sys.argv.(0) ^ " OPTIONS FILE1 FILE2\n\
    Takes two Debian binary package control files as input and computes\n\
    a core set of packages that were co-installable but are not anymore\n\
    after upgrade.\n\
    \n\
    Options:");
let (file1, file2) =
  match List.rev !l with
    [] -> (file1, file2)
  | [file1; file2] -> (file1, file2)
  | _ ->
    Format.eprintf
      "Exactly two Debian binary package control files \
       should be provided as input.@.";
    exit 1
in
f file1 file2 !output_dir


(*
libjpeg8-dev "replaces" libjpeg62-dev, so why does the tools do not
propose me to install it instead?

========================

jerome@keithp:~/Mancoosi$ LC_ALL=C sudo apt-get dist-upgrade
Reading package lists... Done
Building dependency tree       
Reading state information... Done
Calculating upgrade... Done
The following packages will be REMOVED:
  libhdf4-dev
The following packages will be upgraded:
  libhdf4-0
1 upgraded, 0 newly installed, 1 to remove and 0 not upgraded.
Need to get 353 kB of archives.
After this operation, 2227 kB disk space will be freed.
Do you want to continue [Y/n]? 

========================

jerome@keithp:~/Mancoosi$ LC_ALL=C sudo aptitude dist-upgrade
The following NEW packages will be installed:
  libjpeg8-dev{ab} 
The following packages will be upgraded:
  libhdf4-0 libhdf4-dev 
2 packages upgraded, 1 newly installed, 0 to remove and 0 not upgraded.
Need to get 1141 kB of archives. After unpacking 652 kB will be used.
The following packages have unmet dependencies:
  libjpeg8-dev: Conflicts: libjpeg62-dev but 6b1-2 is installed.
The following actions will resolve these dependencies:

     Remove the following packages:                       
1)     libhdf4-dev                                        

     Keep the following packages at their current version:
2)     libjpeg8-dev [Not Installed]                       

Accept this solution? [Y/n/q/?] 
*)

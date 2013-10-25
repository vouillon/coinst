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

let _ =
Gc.set {(Gc.get ())
        with Gc.space_overhead = 300; Gc.max_overhead = 1000000}

module M = Deb_lib
module Repository = Upgrade_common.Repository
open Repository
module Quotient = Quotient.F (Repository)
module Graph = Graph.F (Repository)

module PSetSet = Upgrade_common.PSetSet
module PSetMap = Map.Make (PSet)

module L = Layout
let (&) = L.(&)

(****)

let whitespaces = Str.regexp "[ \t]+"

let load_popcon file =
  let tbl = Hashtbl.create 4096 in
  let ch = File.open_in file in
  begin try
    while true do
      let l = input_line ch in
      if l <> "" && l.[0] = '-' then raise End_of_file;
      if l <> "" && l.[0] <> '#' then begin
        let l = Str.split whitespaces l in
        match l with
          _ :: name :: inst :: _ ->
            Hashtbl.add tbl name (int_of_string inst)
        | _ ->
            assert false
      end
    done
  with End_of_file -> () end;
  close_in ch;
  tbl

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

let formatted_dot_to_svg f =
  let ic = open_in f in
  let lst = ref [] in
  let lb = Lexing.from_channel ic in
  begin try
    while true do
      let (_, g) = Dot_graph.from_lexbuf lb in
      let (bbox, scene) = Dot_render.f g in
      let l = Scene.get scene in
      let b = Buffer.create 200 in
      Scene_svg.format (Format.formatter_of_buffer b) (bbox, l);
      lst := Buffer.contents b :: !lst
    done
  with End_of_file -> () end;
  close_in ic;
  List.rev !lst

(****)

let output_conflicts filename dist2 results =
  let in_conflict p p' =
    p <> p' && PSetSet.exists (fun s -> PSet.mem p s && PSet.mem p' s) results
  in
  let involved = PSet.elements (PSetSet.fold PSet.union results PSet.empty) in
  let partition =
    if involved = [] then [] else
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
  Format.fprintf f "node[margin=\"0,0\"];@.";
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

module StringSet = Util.StringSet

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

  let print print_package ch (s, l) =
    if is_true (s, l) then Format.fprintf ch "TRUE" else begin
      Util.print_list print_package ", " ch
        (StringSet.elements s);
      if not (StringSet.is_empty s) && l <> [] then Format.fprintf ch ", ";
      Util.print_list
        (fun ch s ->
           Util.print_list print_package " | " ch (StringSet.elements s))
        ", " ch l
    end

  let format dist format_package (s, l) =
    let format_package nm =
      match M.find_packages_by_name dist (M.id_of_name nm) with
        [p] -> format_package (Package.of_index p.M.num)
      | _   -> L.s nm
    in
    if is_true (s, l) then
      L.s "TRUE"
    else begin
      L.seq ", " format_package (StringSet.elements s) &
      (if not (StringSet.is_empty s) && l <> [] then L.s ", " else L.emp) &
      L.seq ", "
        (fun s -> L.seq " | " format_package (StringSet.elements s))
        l
    end
end

(****)

module IntSet = Util.IntSet

let read_data file =
  let ch = File.open_in file in
  let dist = M.new_pool () in
  M.parse_packages dist [] ch;
  close_in ch;
  M.only_latest dist

type t =
  M.pool * (Repository.Package.t -> L.outside_anchor L.phrasing L.t) *
  Upgrade_common.issue list * PSet.t PTbl.t *
  (string, int) Hashtbl.t * (StringSet.t * StringSet.t list) PSetMap.t

let compute broken_sets ?popcon_file dist1 dist2 format_package =
  let popcon =
    match popcon_file with
      Some file -> load_popcon file
    | None      -> Hashtbl.create 1
  in

  let dist1_state = Upgrade_common.prepare_analyze dist1 in
  let dist2_state = Upgrade_common.prepare_analyze dist2 in
  let (pred, all_pkgs, all_conflicts, dep_src, graphs, _) =
    Upgrade_common.analyze broken_sets dist1_state dist2 in

  (* Compute not new conjunctive dependencies *)
  let cdeps1 =
    Upgrade_common.conj_dependencies dist1 dist1_state.Upgrade_common.deps in
  let cdeps2 =
    Upgrade_common.reversed_conj_dependencies
      dist2 dist2_state.Upgrade_common.deps
  in
  let cdeps =
    PTbl.mapi
      (fun p2 rev ->
         let p1 = Package.of_index (PTbl.get pred p2) in
           PSet.filter
             (fun p' ->
                let i' = PTbl.get pred p' in
                i' <> -1 &&
                match PTbl.get cdeps1 (Package.of_index i') with
                  None   -> true
                | Some s -> PSet.mem p1 s)
             rev)
       cdeps2
  in

  (* Filter out some of the non co-installable sets *)
  let last_id = ref (-1) in
  let package_to_graph = PTbl.create dist2 IntSet.empty in
  let id_to_graph = Hashtbl.create 4096 in
  let register_graph p g =
    PTbl.set package_to_graph p (IntSet.add g (PTbl.get package_to_graph p))
  in
  List.iter
    (fun g ->
       incr last_id;
       let id = !last_id in
       let s = g.Upgrade_common.i_issue in
       PSet.iter
         (fun p ->
            PSet.iter (fun p' -> register_graph p' id) (PTbl.get cdeps p))
         s;
       Hashtbl.add id_to_graph id (g, ref true))
    graphs;
  let graphs = ref [] in
  Hashtbl.iter
    (fun id (g, active) ->
       if !active then begin
         let s = g.Upgrade_common.i_issue in
         let p = PSet.choose s in
         let grs =
           PSet.fold
             (fun p gr -> IntSet.inter (PTbl.get package_to_graph p) gr)
             s (PTbl.get package_to_graph p)
         in
         let inactivate =
           IntSet.exists
             (fun id' ->
                id <> id' &&
                let (g', active') = Hashtbl.find id_to_graph id' in
                !active' &&
                PSet.for_all
                  (fun p ->
                     PSet.exists (fun p' -> PSet.mem p' s) (PTbl.get cdeps p))
                  g'.Upgrade_common.i_issue)
             grs
         in
         if inactivate then active := false else graphs := g :: !graphs
       end)
    id_to_graph;
  let prob_pkgs = ref PSetMap.empty in
  List.iter
    (fun {Upgrade_common.i_issue = s;
          i_problem =
            {Upgrade_common.p_clause  = {Upgrade_common.pos = pos}}} ->
       prob_pkgs :=
         PSetMap.add s
           (F.conj1
              (try PSetMap.find s !prob_pkgs with Not_found -> F._true)
              pos)
           !prob_pkgs)
    !graphs;
  (dist2, format_package, !graphs, cdeps, popcon, !prob_pkgs)

let conflict_graph (dist2, _, graphs, _, _, _) =
  let tmpname = Filename.temp_file "conflicts" ".dot" in
  let results =
    List.fold_left (fun res gr -> PSetSet.add gr.Upgrade_common.i_issue res)
      PSetSet.empty graphs
  in
  output_conflicts tmpname dist2 results;
  let tmpname' = Filename.temp_file "conflicts" ".dot" in
  ignore
    (Sys.command
       (Format.sprintf "ccomps -x %s | dot -o > %s" tmpname tmpname'));
(*
  ignore (Sys.command (Format.sprintf "dot %s -o /tmp/full.dot" tmpname));
*)
  let figs = formatted_dot_to_svg tmpname' in
  Sys.remove tmpname; Sys.remove tmpname';
  L.section
    (L.heading (L.s "Graph of new conflicts") &
     L.list (fun s -> L.p & L.raw_html (fun _ -> s)) figs)

let format_package dist p = L.s (M.package_name dist (Package.index p))

let explanations (dist2, format_package, graphs, cdeps, popcon, prob_pkgs) =
  let popcon_weight s =
    PSet.fold
      (fun p2 w ->
         let rev = PSet.add p2 (PTbl.get cdeps p2) in
         min w (PSet.fold
                  (fun p' w' ->
                     max (try
                            Hashtbl.find popcon
                              (M.package_name dist2 (Package.index p')) 
                          with Not_found ->
                            0)
                       w')
                  rev 0))
      s max_int
  in
  let format_issue { Upgrade_common.i_issue = s; i_problem = problem } =
    let l = PSet.elements s in
    begin match l with
      [p] ->
        L.dt (L.s "Package " & format_package p &
              L.s " can no longer be installed")
    | _ ->
        L.dt (L.s "Packages " &
              L.seq2 ", " " and " format_package l &
              L.s " can no longer be installed together")
    end
      &
    L.dd
      (let fig =
         let b = Buffer.create 200 in
         Upgrade_common.output_conflict_graph (Format.formatter_of_buffer b)
           problem;
         dot_to_svg (Buffer.contents b)
       in
       L.p & L.raw_html (fun () -> fig)
         &
       let w = popcon_weight s in
       begin if w > 0 then(* Format.fprintf f "<p><b>Weight:</b> %d</p>@." w;*)
         L.p & L.span ~clss:"title" (L.s "Estimated popularity (popcon):") &
         L.s " " & L.s (string_of_int w)
       else
         L.emp
       end
         &
       begin if
         PSet.for_all (fun p -> PSet.cardinal (PTbl.get cdeps p) <= 1) s
       then
         L.emp
       else
         L.p & L.span ~clss:"title" (L.s "Other impacted packages:") &
         L.ul
           (PSet.fold
              (fun p2 doc ->
                 let rev = PTbl.get cdeps p2 in
                 if PSet.cardinal rev > 1 then begin
                   doc &
                   L.li
                     (L.span ~clss:"title"
                        (L.s "packages depending strongly on " &
                         format_package p2 & L.s ":") & L.s " "
                        &
                      L.seq ", " format_package
                        (PSet.elements (PSet.remove p2 rev)))
                 end else
                   doc)
              s L.emp)
       end
         &
       let ppkgs = PSetMap.find s prob_pkgs in
       if not (F.is_true ppkgs) then begin
  (*
         Format.fprintf f "<p><b>Problematic packages:</b> %a</p>@."
           F.print ppkgs
  *)
         L.p & L.span ~clss:"title" (L.s "Problematic packages:") & L.s " " &
         F.format dist2 format_package ppkgs
       end else
         L.emp)
  in
  let compare_graphs =
    if Hashtbl.length popcon = 0 then
      fun {Upgrade_common.i_issue = s} {Upgrade_common.i_issue = s'} ->
        compare (PSet.cardinal s') (PSet.cardinal s)
    else
      fun {Upgrade_common.i_issue = s} {Upgrade_common.i_issue = s'} ->
        compare (popcon_weight s') (popcon_weight s)
  in
  L.dl ~clss:"coinst-issue"
    (L.list format_issue (List.sort compare_graphs graphs))

let has_issues (_, _, graphs, _, _, _) = graphs <> []

let f broken_sets ?popcon_file dist1 dist2 output_file =
  let (_, _, _, _, _, prob_pkgs) as state =
    compute broken_sets ?popcon_file dist1 dist2 (format_package dist2) in

  Format.eprintf "Outputting results...@.";

(*
  let graph =
    L.section
      (L.heading (L.s "Graph of new conflicts") & conflict_graph state) in
*)

  Format.printf "Generating explanations...@.";
  let t = Unix.gettimeofday () in
  let explanations =
(*
    L.heading (L.s "Explanations of conflicts") &
*)
    explanations state &
    let ppkgs =
      PSetMap.fold (fun _ s s' -> F.conj s s') prob_pkgs F._true in
    L.p & L.span ~clss:"title" (L.s "Full list of problematic packages:") &
    L.s " " & F.format dist2 (format_package dist2) ppkgs;
  in
  Format.printf "Generating explanations... %fs@." (Unix.gettimeofday () -. t);

  let ch = open_out output_file in
  let style = "\
  dt { font-weight:bold; font-size: large; }\n\
  span.title { font-weight:bold; }\n\
  svg { display: block; margin:auto; }\n\
  @media print {\n  \
    svg { max-width:100%; }\n  \
    svg { max-height:23cm; }\n  \
    body { font-size:10px; }\n\
  }\n\
  footer {\n  \
    border-top: 2px solid #000;\n  \
    border-bottom: 0;\n  \
    font-size: small;\n  \
    margin-top: 2em;\n\
  }\n"
  in
  let pr = new Layout.html_printer ch ~style "Upgrade issues" in
  Layout.print pr
    (L.heading (L.s "Upgrade issues") & (*graph & L.section*) explanations &
     L.footer (L.s "Page generated by " &
               L.anchor "http://coinst.irill.org/upgrades/"
                 (L.s "coinst-upgrades") &
               L.s (" on " ^ Util.date () ^ ".")));
  close_out ch;

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

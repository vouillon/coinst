
let colored = true

module F (R : Repository.S) = struct

open R

module Quotient = Quotient.F(R)

module Conflicts = Conflicts.F (R)

let output
      ?package_weight
      ?(edge_color = fun _ _ _ -> Some "blue")
      file ?(mark_all = false) ?(roots = [])
      quotient deps confl =
  let package_weight =
    match package_weight with
      Some f -> f
    | None   -> fun p -> float (Quotient.class_size quotient p)
  in

  let confl_style = if colored then ",color=red" else ",style=dashed" in
  let confl_clique_style =
    if colored then ",color=red,fontcolor=red" else "" in
  let dep_style col = if colored then Format.sprintf "color=%s" col else "" in
  let disj_dep_style col =
    if colored then Format.sprintf "fontcolor=%s,color=%s" col col else "" in

  (* Mark the packages to be included in the graph *)
  let marks = Hashtbl.create 101 in
  let marked i = Hashtbl.mem marks i in
  let has_dependencies p =
    let dep = PTbl.get deps p in
    not (Formula.implies Formula._true dep ||
         Formula.implies (Formula.lit p) dep)
  in
  let rec mark p =
    if not (marked p) then begin
      Hashtbl.add marks p ();
      PSet.iter mark (Conflict.of_package confl p)
    end
  in
  if mark_all then
    Quotient.iter (fun p -> Hashtbl.add marks p ()) quotient
  else if roots = [] then
    Quotient.iter
      (fun p ->
         if has_dependencies p then begin
           mark p;
           Formula.iter (PTbl.get deps p) (fun d -> Disj.iter d mark)
         end)
      quotient
  else (*XXX Find the right algorithm...
         Work on transitive closure of dependencies
         Mark all conflicts; marks all packages at the other side of
         these conflicts and all the alternative in the dependency.
         Proceed recursively...

         Backward mode:
         mark source package and all edges but the one considered

         A package is not relevant if installing it or not has no
         impact on the considered package
       *)
    List.iter mark roots;

  let ch = open_out file in
  let f = Format.formatter_of_out_channel ch in
  Format.fprintf f "digraph G {@.";
  Format.fprintf f "rankdir=LR;@.";
  Format.fprintf f "ratio=1.4;@.margin=5;@.ranksep=3;@.";
  Format.fprintf f "node [style=rounded];@.";

  let confl_n = ref 0 in
  Conflict.iter confl
    (fun p q ->
       if not (marked p) then begin
         assert (not (marked q));
         Conflict.remove confl p q
       end);
  let l = Conflicts.f quotient confl in
  List.iter
    (fun cset ->
           match PSet.elements cset with
             [i; j] ->
                Format.fprintf f "%d -> %d [dir=none%s];@."
                  (Package.index i) (Package.index j) confl_style
           | l ->
                incr confl_n;
                let n = !confl_n in
                Format.fprintf f
                  "confl%d [label=\"#\",shape=circle%s];@."
                  n confl_clique_style;
                List.iter
                  (fun i ->
                     Format.fprintf f
                       "%d -> confl%d [dir=none%s];@."
                       (Package.index i) n confl_style)
                  l)
    l;


  let dep_tbl = Hashtbl.create 101 in
  let dep_n = ref 0 in
  let add_dep i dep d =
    let s = Disj.to_lits d in
    match edge_color i dep d with
      None ->
        ()
    | Some col ->
        match PSet.cardinal s with
          0 ->
            incr dep_n;
            let n = !dep_n in
            Format.fprintf f
              "dep%d \
               [label=\"MISSING DEP\",shape=box,fontcolor=red,%s];@."
              n (dep_style col);
            Format.fprintf f "%d -> dep%d [%s];@."
              (Package.index i) n (dep_style col)
        | 1 ->
            if PSet.choose s <> i then
              Format.fprintf f "%d -> %d [minlen=2, weight=2, %s];@."
                (Package.index i) (Package.index (PSet.choose s))
                (dep_style col)
        | _ ->
            let n =
              try
                Hashtbl.find dep_tbl s
              with Not_found ->
                incr dep_n;
                let n = !dep_n in
                Hashtbl.add dep_tbl s n;
(*
                Format.fprintf f "dep%d [label=\"DEP\",shape=box,color=%s];@."
                  n col;
*)
                Format.fprintf f "dep%d [label=\"∨\",shape=circle,%s];@."
                  n (disj_dep_style col);
(*
                Format.fprintf f "dep%d [label=\"or\",shape=circle,%s];@."
                  n (disj_dep_style col);
*)
                PSet.iter
                  (fun j ->
                     Format.fprintf f "dep%d -> %d [%s];@."
                       n (Package.index j) (dep_style col))
                  s;
                n
            in
            Format.fprintf f "%d -> dep%d [%s];@."
              (Package.index i) n (dep_style col)
  in
  Quotient.iter
    (fun i ->
       let dep = PTbl.get deps i in
       if marked i then begin
         let n = package_weight i in
         Format.fprintf f
           "%d [label=\"%a\",style=\"filled\",\
            fillcolor=\"0.0,%f,1.0\"];@."
           (Package.index i) (Quotient.print_class quotient) i
           (min 1. (log n /. log 1000.));
         Formula.iter dep (fun s -> add_dep i dep s)
       end)
    quotient;

  Format.fprintf f "}@.";
  close_out ch

end

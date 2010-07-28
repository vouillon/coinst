
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
  Format.fprintf f "ratio=1.4;@.margin=5;@.";

(*
  let confl_n = ref 0 in
  let cpairs = Hashtbl.create 101 in
  let visited i j = Hashtbl.mem cpairs (min i j, max i j) in
  let visit i j = Hashtbl.replace cpairs (min i j, max i j) () in
  Quotient.iter
    (fun i ->
       let s = Conflict.of_package confl i in
       if marked i then begin
         assert (PSet.for_all (fun j -> marked j) s);
         while
           let c = PSet.filter (fun j -> not (visited i j)) s in
           not (PSet.is_empty c)
         do
           let c = PSet.filter (fun j -> not (visited i j)) s in
           let cset =
             PSet.fold
               (fun j cset ->
                  if
                    PSet.for_all
                      (fun i -> Conflict.check confl i j && not (visited i j)) cset
                  then
                    PSet.add j cset
                  else
                    cset)
               c (PSet.add i PSet.empty)
           in
           PSet.iter (fun i -> PSet.iter (fun j -> visit i j) cset) cset;
           match PSet.elements cset with
             [i; j] ->
                Format.fprintf f "%d -> %d [dir=none,color=red];@."
                  (Package.index i) (Package.index j)
           | l ->
                incr confl_n;
                let n = !confl_n in
                Format.fprintf f
                  "confl%d [label=\"#\",shape=box,color=red,fontcolor=red];@."
                  n;
                List.iter
                  (fun i ->
                     Format.fprintf f
                       "%d -> confl%d [dir=none,color=red];@."
                       (Package.index i) n)
                  l
         done
      end)
    quotient;
*)
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
                Format.fprintf f "%d -> %d [dir=none,color=red];@."
                  (Package.index i) (Package.index j)
           | l ->
                incr confl_n;
                let n = !confl_n in
                Format.fprintf f
                  "confl%d [label=\"#\",shape=box,color=red,fontcolor=red];@."
                  n;
                List.iter
                  (fun i ->
                     Format.fprintf f
                       "%d -> confl%d [dir=none,color=red];@."
                       (Package.index i) n)
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
               [label=\"MISSING DEP\",shape=box,fontcolor=red,color=%s];@."
              n col;
            Format.fprintf f "%d -> dep%d [color=%s];@."
              (Package.index i) n col
        | 1 ->
            if PSet.choose s <> i then
              Format.fprintf f "%d -> %d [minlen=2, weight=2, color=%s];@."
                (Package.index i) (Package.index (PSet.choose s)) col
        | _ ->
            let n =
              try
                Hashtbl.find dep_tbl s
              with Not_found ->
                incr dep_n;
                let n = !dep_n in
                Hashtbl.add dep_tbl s n;
                Format.fprintf f "dep%d [label=\"DEP\",shape=box,color=%s];@."
                  n col;
                PSet.iter
                  (fun j ->
                     Format.fprintf f "dep%d -> %d [color=%s];@."
                       n (Package.index j) col)
                  s;
                n
            in
            Format.fprintf f "%d -> dep%d [color=%s];@."
              (Package.index i) n col
  in
  Quotient.iter
    (fun i ->
       let dep = PTbl.get deps i in
       if marked i then begin
         let n = package_weight i in
         Format.fprintf f
           "%d [label=\"%a\",style=filled,fillcolor=\"0.0,%f,1.0\"];@."
           (Package.index i) (Quotient.print_class quotient) i
           (min 1. (log n /. log 1000.));
         Formula.iter dep (fun s -> add_dep i dep s)
       end)
    quotient;

  Format.fprintf f "}@.";
  close_out ch

end

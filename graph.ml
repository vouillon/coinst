
module F (R : Repository.S) = struct

  open R

let print_class dist ch (i, n) =
  if n = 1 then
    Format.fprintf ch "%a" (Package.print_name dist) i
  else
    Format.fprintf ch "%a (x %d)" (Package.print_name dist) i n

let output file dist mark_all confl classes =
  (* Mark the packages to be included in the graph *)
  let marks = Hashtbl.create 101 in
  let marked i = Hashtbl.mem marks i in
  let rec mark i =
    if not (marked i) then begin
      Hashtbl.add marks i ();
      let (_, _, confl) = Hashtbl.find classes i in
      List.iter (fun s -> PSet.iter mark s) confl
    end
  in
  Hashtbl.iter
    (fun i (_, deps, _) ->
       if mark_all then
         mark i
       else if
         not (Formula.implies Formula._true deps ||
              Formula.implies1 deps (Disj.lit i))
       then begin
         mark i; Formula.iter deps (fun d -> Disj.iter d mark)
       end)
    classes;

  let ch = open_out file in
  let f = Format.formatter_of_out_channel ch in
  Format.fprintf f "digraph G {@.";
  Format.fprintf f "rankdir=LR;@.";
  Format.fprintf f "ratio=1.4;@.margin=5;@.";

  let confl_n = ref 0 in
  let cpairs = Hashtbl.create 101 in
  let visited i j = Hashtbl.mem cpairs (min i j, max i j) in
  let visit i j = Hashtbl.replace cpairs (min i j, max i j) () in
  Conflict.iter_on_packages confl
    (fun i s ->
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
      end);

  let dep_tbl = Hashtbl.create 101 in
  let dep_n = ref 0 in
  let add_dep i s =
    let s = Disj.to_lits s in
    match PSet.cardinal s with
      0 ->
        incr dep_n;
        let n = !dep_n in
        Format.fprintf f
          "dep%d [label=\"MISSING DEP\",shape=box,fontcolor=red,color=blue];@."
          n;
        Format.fprintf f "%d -> dep%d [color=blue];@." (Package.index i) n
    | 1 ->
        if PSet.choose s <> i then
          Format.fprintf f "%d -> %d [color=blue];@."
            (Package.index i) (Package.index (PSet.choose s))
    | _ ->
        let n =
          try
            Hashtbl.find dep_tbl s
          with Not_found ->
            incr dep_n;
            let n = !dep_n in
            Hashtbl.add dep_tbl s n;
            Format.fprintf f "dep%d [label=\"DEP\",shape=box,color=blue];@." n;
            PSet.iter
              (fun j ->
                 Format.fprintf f "dep%d -> %d [color=blue];@."
                   n (Package.index j))
              s;
            n
        in
        Format.fprintf f "%d -> dep%d [color=blue];@." (Package.index i) n
  in
  Hashtbl.iter
    (fun i (l, deps, _) ->
       if marked i then begin
         let n = List.length l in
         Format.fprintf f
           "%d [label=\"%a\",style=filled,fillcolor=\"0.0,%f,1.0\"];@."
           (Package.index i) (print_class dist) (i, n)
           (min 1. (log (float n) /. log 1000.));
         Formula.iter deps (fun s -> add_dep i s)
       end)
    classes;

  Format.fprintf f "}@.";
  close_out ch

end

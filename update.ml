
let debug = false

module M = Deb_lib
module Repository = Repository.F(M)
open Repository
module Quotient = Quotient.F (Repository)
module Graph = Graph.F (Repository)

(****)

let get_list' h n =
  try
    Hashtbl.find h n
  with Not_found ->
    let r = ref [] in
    Hashtbl.add h n r;
    r

let add_to_list h n p =
  let l = get_list' h n in
  l := p :: !l

let get_list h n = try !(Hashtbl.find h n) with Not_found -> []

(****)

let read_data ignored_packages ic =
  let dist = M.new_pool () in
  M.parse_packages dist ignored_packages ic;
  let confl = Conflict.create dist in
  let c = M.compute_conflicts dist in
  Array.iteri
    (fun p1 l ->
       List.iter
         (fun p2 ->
            Conflict.add confl
              (Package.of_index p1) (Package.of_index p2))
         l)
    c;

  let deps =
    let d = M.compute_deps dist in
    PTbl.init dist
      (fun p ->
         Formula.conjl
           (List.map (fun l' -> Formula.lit_disj (Package.of_index_list l'))
           d.(Package.index p)))
  in
  (dist, deps, confl)

(****)

let generate_rules quotient deps confl =
  let dist = Quotient.pool quotient in
  let st =
    M.Solver.initialize_problem
      ~print_var:(M.print_pack dist) (M.pool_size dist) in
  Conflict.iter confl
    (fun p1 p2 ->
       let p1 = M.Solver.lit_of_var (Package.index p1) false in
       let p2 = M.Solver.lit_of_var (Package.index p2) false in
       M.Solver.add_rule st [|p1; p2|] []);
  Quotient.iter
    (fun p ->
       let f = PTbl.get deps p in
       Formula.iter f
         (fun d ->
            let l = Disj.to_lits d in
            if not (PSet.mem p l) then begin
              let l = List.map (fun p -> Package.index p) (PSet.elements l) in
              M.Solver.add_rule st
                (Array.of_list
                   (M.Solver.lit_of_var (Package.index p) false ::
                    List.map (fun p -> M.Solver.lit_of_var p true) l))
                [];
              match l with
                [] | [_] ->
                  ()
              | _ ->
                  M.Solver.associate_vars st
                    (M.Solver.lit_of_var (Package.index p) true) l
            end))
    quotient;
  st

(****)

let simplify_formula confl f =
  Formula.filter
    (fun d ->
       Disj.for_all
         (fun p ->
            Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d)
    f

let rec flatten_deps tbl dist deps conflicts visited l =
  Formula.fold
    (fun d (l, r) ->
       let (l', r') =
         Disj.fold
           (fun i (l, r) ->
              let (l', r') = flatten_dep tbl dist deps conflicts visited i in
              (Formula.disj l' l, PSet.union r r')) d (Formula._false, r)
       in
       (Formula.conj l' l, r'))
    l (Formula._true, PSet.empty)

and flatten_dep tbl dist deps conflicts visited i =
  try
    (Hashtbl.find tbl i, PSet.empty)
  with Not_found ->
    let res =
      if List.mem i visited then
        (Formula._true, PSet.singleton i)
      else begin
        let (l, r) =
          flatten_deps tbl dist deps conflicts (i :: visited) (PTbl.get deps i)
        in
        let l = simplify_formula conflicts l in
(*???
        let l = filter_conflicts conflicts i l in
*)
        let r = PSet.remove i r in
        if Conflict.has conflicts i then
          (Formula.conj (Formula.lit i) l, r)
        else
          (l, r)
      end
    in
    (* Only cache the result if it is unconditionally true *)
    if PSet.is_empty (snd res) then Hashtbl.add tbl i (fst res);
    res

let flatten_dependencies dist deps confl =
  let tbl = Hashtbl.create 17 in
  PTbl.init dist (fun p -> fst (flatten_dep tbl dist deps confl [] p))

(****)

let remove_redundant_conflicts dist deps confl =
  let conj_deps p =
    let f = PTbl.get deps p in
    Formula.fold
      (fun d s -> match Disj.to_lit d with Some p -> PSet.add p s | None -> s)
      f PSet.empty
  in
  Conflict.iter confl
    (fun p1 p2 ->
       let d1 = conj_deps p1 in
       let d2 = conj_deps p2 in
       if
         PSet.exists
           (fun q1 ->
              PSet.exists
                (fun q2 ->
                   (p1 <> q1 || p2 <> q2) &&
                   (p1 <> q2 || p2 <> q1) &&
                   Conflict.check confl q1 q2)
                d2)
           d1
       then begin
(*
         Format.eprintf "%a ## %a@."
           (Package.print_name dist) p1 (Package.print_name dist) p2;
*)
         Conflict.remove confl p1 p2
       end);
  let try_remove_conflict p1 p2 =
    let f1 = PTbl.get deps p1 in
    let d2 = conj_deps p2 in
    if
      Formula.exists
        (fun d1 ->
           Disj.for_all
             (fun q1 ->
                PSet.exists
                  (fun q2 ->
                     (p1 <> q1 || p2 <> q2) &&
                     (p1 <> q2 || p2 <> q1) &&
                     Conflict.check confl q1 q2)
                  d2)
             d1)
        f1
    then begin
(*
      Format.eprintf "%a ## %a@."
        (Package.print_name dist) p1 (Package.print_name dist) p2;
*)
      Conflict.remove confl p1 p2
    end
  in
  Conflict.iter confl try_remove_conflict;
  Conflict.iter confl (fun p1 p2 -> try_remove_conflict p2 p1);
  (* We may now be able to remove some dependencies *)
  PTbl.map (simplify_formula confl) deps

(****)

let (file1,file2) =
("snapshots/updates/stable", "snapshots/updates/testing")
(*
("snapshots/updates/oldstable", "snapshots/updates/stable")
("/tmp/last_month", "/tmp/new")
*)

let flatten dist deps confl =
  let deps = flatten_dependencies dist deps confl in
  let deps = remove_redundant_conflicts dist deps confl in
  let deps = flatten_dependencies dist deps confl in

  let maybe_remove fd2 p f d =
    Disj.exists (fun q ->
      Conflict.for_all confl (fun r ->
        Formula.exists (fun d' -> Disj.implies d' d && not (Disj.implies1 q d')) (PTbl.get fd2 r)) q
(*
&& (
Format.eprintf "%a =>(%a) %a@." (Package.print_name dist) p (Package.print_name dist) q (Disj.print dist) d;
true)
*)
) d
  in
  let is_composition fd2 p f d =
  Formula.exists (fun d' ->
    not (Disj.equiv d d') && not (Disj.equiv (Disj.lit p) d') &&
(
    let f =
      Disj.fold (fun p f -> Formula.disj (PTbl.get fd2 p) f) d' Formula._false
    in
    let res1 =
      Formula.exists (fun d'' -> Disj.implies d d'') f
    in
(*
    let res2 =
      not (Formula.implies (Formula.filter (fun d' -> not (Disj.equiv d d')) f) f)
    in
    let res =
    Disj.exists (fun q ->
      Formula.exists (fun d'' ->
        Disj.implies d (Disj.cut d' q d'')) (PTbl.get fd2 q)) d'
    in
if res1 <> res2 then begin
Format.eprintf "??? %b %b@." res1 res2
end;
*)
(*
if res <> res1 then begin
(*
Format.eprintf "%a : %a => %a@." (Package.print_name dist) p
(Disj.print dist) d (Formula.print dist) f;

Disj.iter d' (fun q ->
  if not (Formula.exists (fun d'' -> Disj.equiv (Disj.lit q) d'') (PTbl.get fd2 q)) then
    Format.eprintf "!!! %a => %a@." (Package.print_name dist) q (Formula.print dist) (PTbl.get fd2 q);
  Formula.iter (PTbl.get fd2 q) (fun d'' ->
     if Disj.implies d (Disj.cut d' q d'')
     then Format.eprintf "%a <= %a / %a / %a@." (Disj.print dist) d (Disj.print dist) d' (Package.print_name dist) q (Disj.print dist) d''
));
*)

Format.eprintf "!!! %b %b %b@." res res1 res2
end;
*)
res1
)
) f
in

(*
PTbl.iteri (fun p f ->
Formula.iter f (fun d -> if Conflict.exists confl (fun q -> Disj.implies1 q d) p then Format.eprintf "YYY %a ==> %a@." (Package.print_name dist) p (Disj.print dist) d
)) fd2;
*)

  let rec remove_deps deps =
    let changed = ref false in
    let deps =
      PTbl.mapi
        (fun p f ->
           Formula.filter (fun d ->
             let b =
               not (maybe_remove deps p f d) || is_composition deps p f d
             in
             if not b then changed := true;
             b) f)
        deps
    in
    if !changed then remove_deps deps else deps
  in

  let deps = remove_deps deps in

  (deps, confl)



(****)

let new_deps pred deps1 dist2 deps2 =
  PTbl.mapi
    (fun p2 i ->
       if i = -1 then
         Formula._true
       else begin
         let p1 = Package.of_index i in
         let f1 = PTbl.get deps1 p1 in
         let f2 = PTbl.get deps2 p2 in

         let f2 =
           Formula.filter
             (fun d2 ->
                let d1 =
                  Disj.fold
                    (fun p2 d2 ->
                       let i = PTbl.get pred p2 in
                       if i = -1 then d2 else
                       Disj.disj (Disj.lit (Package.of_index i)) d2)
                    d2 Disj._false
                in
                not (Formula.implies1 f1 d1))
             f2
         in
  if debug && not (Formula.implies Formula._true f2) then begin
  Format.printf "%a ==> %a@."
   (Package.print_name dist2) p2
   (Formula.print dist2) f2;
  (*
  Format.printf "%a --> %a@."
   (Package.print_name dist1) p1
   (Formula.print dist1) f1
  *)
  end;
         f2
       end)
    pred

(****)

module IntSet =
  Set.Make (struct type t = int let compare (x : int) y = compare x y end)

module PSetSet = Set.Make (PSet)

type st =
  { dist : M.pool; deps : Formula.t PTbl.t; confl : Conflict.t;
    pieces : (int, Package.t * Disj.t) Hashtbl.t;
    pieces_in_confl : (Package.t, int list ref) Hashtbl.t;
    set : PSet.t;
    installed : IntSet.t; not_installed : IntSet.t;
    check : PSet.t -> bool }

let print_prob st =
  IntSet.iter
    (fun i ->
       let (p, d) = Hashtbl.find st.pieces i in
       Format.printf "%a => %a; "
         (Package.print_name st.dist) p
         (Disj.print st.dist) d)
    st.installed;
  Format.printf "@."

let rec add_piece st i cont =
  assert (not (IntSet.mem i st.installed || IntSet.mem i st.not_installed));
  let (p, d) = Hashtbl.find st.pieces i in
if debug then Format.printf "Try to add %a => %a@." (Package.print_name st.dist) p
         (Disj.print st.dist) d;
  (* XXX
     When adding a package in st.set, one could also check that d is not
     implied by any of the dependencies of a package already in st.set *)
  if
    not (IntSet.exists
           (fun i' ->
              let (_, d') = Hashtbl.find st.pieces i' in
              Disj.implies d d' || Disj.implies d' d)
           st.installed)
      &&
    (PSet.mem p st.set || st.check (PSet.add p st.set))
  then begin
    let st =
      {st with set = PSet.add p st.set;
       installed = IntSet.add i st.installed}
    in
    if debug then print_prob st;
    (* Make sure that there is at least one piece in conflict for all
       dependencies, then consider all possible additions *)
    Disj.fold
      (fun p cont st ->
         if
           PSet.exists
             (fun q ->
                List.exists (fun i -> IntSet.mem i st.installed)
                  (get_list st.pieces_in_confl q))
             (Conflict.of_package st.confl p)
         then
           cont st
         else
           ignore
             (PSet.fold
                (fun q st -> 
                   List.fold_right (fun j st -> do_add_piece st j cont)
                     (get_list st.pieces_in_confl q) st)
                (Conflict.of_package st.confl p) st))
      d
      (fun st ->
if debug then Format.printf "Considering all possible additions in %d: %a...@."
i (Disj.print st.dist) d;
        Disj.fold
         (fun p cont ->
            PSet.fold
              (fun q cont ->
                 List.fold_right (fun j cont st -> maybe_add_piece st j cont)
                   (get_list st.pieces_in_confl q) cont)
              (Conflict.of_package st.confl p) cont)
           d cont st)
      st
  end

and do_add_piece st i cont =
  if IntSet.mem i st.installed then begin
    cont st; st
  end else if not (IntSet.mem i st.not_installed) then begin
    add_piece st i cont;
    {st with not_installed = IntSet.add i st.not_installed}
  end else
    st

and maybe_add_piece st i cont =
  if
    not (IntSet.mem i st.installed || IntSet.mem i st.not_installed)
  then begin
    add_piece st i cont;
    cont {st with not_installed = IntSet.add i st.not_installed}
  end else
    cont st

let find_problems dist deps confl check =
  let pieces = Hashtbl.create 101 in
  let last_piece = ref (-1) in
  let pieces_in_confl = Hashtbl.create 101 in
  PTbl.iteri
    (fun p f ->
      Formula.iter f
        (fun d ->
          incr last_piece;
          let i = !last_piece in
          Hashtbl.add pieces i (p, d);
          Disj.iter d (fun p -> add_to_list pieces_in_confl p i)))
    deps;
  let st =
    { dist = dist; deps = deps; confl = confl;
      pieces = pieces; pieces_in_confl = pieces_in_confl;
      set = PSet.empty; check = check;
      installed = IntSet.empty; not_installed = IntSet.empty }
  in
  for i = 0 to !last_piece do
    add_piece st i (fun _ -> ())
  done

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

let _ =
let (dist1, deps1, confl1) = read_data [] (open_in file1) in
let (dist2, deps2, confl2) = read_data [] (open_in file2) in

let pred =
  PTbl.init dist2
    (fun p2 ->
       let nm = M.package_name dist2 (Package.index p2) in
       match M.parse_package_name dist1 nm with
         [] ->
           if debug then Format.printf "%s is a new package@." nm;
           -1
       | [p1] ->
           p1
       | _ ->
           assert false)
in

let new_conflicts = ref [] in
Conflict.iter confl2
  (fun p2 q2 ->
     let i = PTbl.get pred p2 in
     let j = PTbl.get pred q2 in
     if i <> -1 && j <> -1 then begin
       let p1 = Package.of_index i in
       let q1 = Package.of_index j in
       if not (Conflict.check confl1 p1 q1) then begin
if debug then begin
         Format.printf "possible new conflict: %a %a@."
           (Package.print_name dist1) p1
           (Package.print_name dist1) q1;
end;
         new_conflicts := (p2, q2) :: !new_conflicts;
         Conflict.remove confl2 p2 q2
       end
     end);

let (deps1', confl1) = flatten dist1 deps1 confl1 in
let (deps2', confl2) = flatten dist2 deps2 confl2 in
let st1 = generate_rules (Quotient.trivial dist1) deps1' confl1 in
let st2 = generate_rules (Quotient.trivial dist2) deps2' confl2 in
let st2init = M.generate_rules dist2 in

let results = ref PSetSet.empty in

List.iter
  (fun (p2, q2) ->
    let i = PTbl.get pred p2 in
    let j = PTbl.get pred q2 in
    let p1 = Package.of_index i in
    let q1 = Package.of_index j in
    if M.Solver.solve_lst st1 [i; j] then begin
if debug then begin
      Format.printf "new conflict: %a %a@."
        (Package.print_name dist1) p1
        (Package.print_name dist1) q1;
end;
      results := PSetSet.add (PSet.add p2 (PSet.add q2 PSet.empty)) !results
    end else begin
if debug then begin
      Format.printf "NOT new conflict: %a %a@."
        (Package.print_name dist1) p1
        (Package.print_name dist1) q1;
      M.show_reasons dist1 (M.Solver.collect_reasons_lst st1 [i; j])
end
    end;
    M.Solver.reset st1)
  !new_conflicts;

(* Only consider new dependencies. *)
let deps2 = new_deps pred deps1 dist2 deps2 in
(* Compute the corresponding flattened dependencies. *)
let deps2 =
  PTbl.mapi
     (fun p f ->
        Formula.fold
          (fun d f ->
             Formula.conj
               (PSet.fold
                  (fun p f -> Formula.disj (PTbl.get deps2' p) f)
                  (Disj.to_lits d) Formula._false) f)
          f Formula._true)
    deps2
in
(* Only keep those that are new... *)
let deps2 = new_deps pred deps1' dist2 deps2 in
(* ...and that are indeed in the flattened repository *)
let deps2 =
  PTbl.mapi
    (fun p f ->
       let f' = PTbl.get deps2' p in
       Formula.filter
         (fun d ->
            Formula.exists (fun d' -> Disj.equiv d d') f') f)
    deps2
in

(* Only keep relevant conflicts. *)
let dep_targets = ref PSet.empty in
PTbl.iteri
  (fun _ f ->
     Formula.iter f
       (fun d ->
          Disj.iter d (fun p -> dep_targets := PSet.add p !dep_targets)))
  deps2;
Conflict.iter confl2
  (fun p2 q2 ->
     let i1 = PTbl.get pred p2 in
     let j1 = PTbl.get pred q2 in
     if
       not ((PSet.mem p2 !dep_targets && j1 <> -1) ||
            (PSet.mem q2 !dep_targets && i1 <> -1) ||
            (PSet.mem p2 !dep_targets && PSet.mem q2 !dep_targets))
     then
       Conflict.remove confl2 p2 q2);
(* As a consequence, some new dependencies might not be relevant anymore. *)
let deps2 = flatten_dependencies dist2 deps2 confl2 in

Graph.output "/tmp/update.dot"
  ~package_weight:(fun p ->
    if Formula.implies (Formula.lit p) (PTbl.get deps2 p) then
      (if PTbl.get pred p = -1 then 1. else 10.)
    else 1000.)
  (Quotient.trivial dist2) deps2 confl2;

(* Flattening may have added self dependencies for new packages,
   which are not relevant. *)
let deps2 =
  PTbl.mapi
    (fun p f -> if PTbl.get pred p = -1 then Formula._true else f)
    deps2
in


let check s =
  let now_installable s =
    let res =
      M.Solver.solve_lst st2 (List.map Package.index (PSet.elements s)) in
    M.Solver.reset st2;
    res
  in
  let l = PSet.elements s in
  let was_coinstallable =
    M.Solver.solve_lst st1 (List.map (fun p -> PTbl.get pred p) l)
  in
  M.Solver.reset st1;
  if not was_coinstallable then begin
if debug then begin
Format.printf "Was not co-installable:";
List.iter (fun p -> Format.printf " %a" (Package.print_name dist2) p) l;
Format.printf "@.";
end;
    false
  end else if now_installable s then begin
if debug then begin
Format.printf "Still co-installable:";
List.iter (fun p -> Format.printf " %a" (Package.print_name dist2) p) l;
Format.printf "@.";
end;
    true
  end else begin
    if
      PSet.exists (fun p -> not (now_installable (PSet.remove p s))) s
    then begin
if true (*debug*) then begin
Format.printf "Not minimal:";
List.iter (fun p -> Format.printf " %a" (Package.print_name dist2) p) l;
Format.printf "@.";
end;
    end else begin
Format.printf "==>";
List.iter (fun p -> Format.printf " %a" (Package.print_name dist2) p) l;
Format.printf "@.";
      results := PSetSet.add s !results
    end;
    false
  end
in
find_problems dist2 deps2 confl2 check;

(****)

let ch = open_out "/tmp/index.html" in
let f = Format.formatter_of_out_channel ch in
Format.fprintf f "<h1>Upgrade issues</h1>@.";
output_conflicts "/tmp/conflicts.dot" dist2 !results;
let basename = "/tmp/conflicts" in
ignore
  (Sys.command
     (Format.sprintf "dot %s.dot -Tsvg -o %s.svg" basename basename));
Format.fprintf f "<h2>The graph of new conflicts</h2>@.";
Format.fprintf f
  "<p><object data=\"conflicts.svg\" type=\"image/svg+xml\"></object></p>@.";

let all_pkgs = ref PSet.empty in
let all_conflicts = Conflict.create dist2 in
let dep_src = PTbl.create dist2 PSet.empty in
let dep_trg = PTbl.create dist2 PSet.empty in
let add_rel r p q = PTbl.set r p (PSet.add q (PTbl.get r p)) in

let graphs =
  List.map
    (fun s ->
       let l = List.map Package.index (PSet.elements s) in
       let nm =
         String.concat ","
           (List.map (fun p -> M.package_name dist2 (Package.index p))
              (PSet.elements s))
       in
       let res = M.Solver.solve_lst st2init l in
       assert (not res);
       let r = M.Solver.collect_reasons_lst st2init l in
       M.Solver.reset st2init;
       let confl = Conflict.create dist2 in
       let deps = PTbl.create dist2 Formula._true in
       let pkgs = ref PSet.empty in
       let package i =
         let p = Package.of_index i in pkgs := PSet.add p !pkgs; p in
       List.iter
         (fun r ->
            match r with
              M.R_conflict (n1, n2) ->
                Conflict.add confl (package n1) (package n2);
                Conflict.add all_conflicts (package n1) (package n2)
            | M.R_depends (n, l) ->
                let p = package n in
                let l =
                  List.map package
                    (List.flatten (List.map (M.resolve_package_dep dist2) l))
                in
                List.iter
                  (fun q ->
                     add_rel dep_src q p;
                     add_rel dep_trg p q)
                  l;
                PTbl.set deps p
                  (Formula.conj (PTbl.get deps p)
                     (Formula.of_disj (Disj.lit_disj l))))
         r;
       all_pkgs := PSet.union !all_pkgs !pkgs;
       (s, nm, !pkgs, deps, confl))
    (PSetSet.elements !results)
in

let involved = PSet.elements !all_pkgs in
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
let graphs =
  List.filter
    (fun (s, _, _, _, _) -> PSet.for_all (fun p -> Hashtbl.find repr p = p) s)
    graphs
in

Format.fprintf f "<h2>Explanations of conflicts</h2>@.";
List.iter
  (fun (s, nm, pkgs, deps, confl) ->
     let quotient = Quotient.from_partition dist2 pkgs partition in
     let deps = Quotient.dependencies quotient deps in
     let confl = Quotient.conflicts quotient confl in
     let basename = "/tmp/" ^ nm in
     let edge_color p2 _ d2 =
Format.printf "%a:@."(Package.print dist2) p2;
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
Format.printf "  %a / %a@."
(Formula.print dist1) (PTbl.get deps1 (Package.of_index i1))
(Disj.print dist1) d1;
         not (Formula.implies1 (PTbl.get deps1 (Package.of_index i1)) d1)
       in
       if is_new then
         Some "violet"
       else
         Some "blue"
     in
    Graph.output (basename ^ ".dot")
       ~options:["rankdir=LR;"; "node[fontsize=8];"; "node[margin=0,0];"; "node[height=0.2];"]
       ~edge_color
       ~package_weight:(fun p -> if PSet.mem p s then 10. else 1.)
       ~mark_all:true quotient deps confl;
(*
     ignore
       (Sys.command
          (Format.sprintf "dot %s.dot -Tpng -o %s.png" basename basename));
     Format.fprintf f "<p><img src=\"%s.png\" alt=\"%s\"/></p>@." nm nm;
*)
     ignore
       (Sys.command
          (Format.sprintf "dot %s.dot -Tsvg -o %s.svg" basename basename));
     Format.fprintf f
       "<p><object data=\"%s.svg\" type=\"image/svg+xml\"></object></p>@." nm)
  graphs;
close_out ch;

(****)

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

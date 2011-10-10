
module F (M : Api.S) = struct

module Repository = Repository.F(M)
open Repository
module Quotient = Quotient.F (Repository)
module PSetMap = Map.Make (PSet)

(****)

let compute_dependencies_and_conflicts dist =
  let confl = Conflict.create dist in
  let c = M.compute_conflicts dist in
  Array.iteri
    (fun p1 l ->
       List.iter
         (fun p2 ->
            Conflict.add confl (Package.of_index p1) (Package.of_index p2))
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
  (deps, confl)

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
  Conflict.iter confl (fun p1 p2 -> try_remove_conflict p2 p1)

(****)

let remove_self_conflicts dist deps confl =
  let clearly_broken p f =
    Formula.exists
      (fun d ->
         match Disj.to_lit d with
           Some q -> Conflict.check confl p q
         | None   -> false)
      f
  in
  let changed = ref false in
  let deps =
    PTbl.mapi
      (fun p f ->
         if clearly_broken p f then begin
Format.printf "self conflict: %a@." (Package.print_name dist) p;
           changed := true; Formula._false
         end else
           f)
      deps
  in
  (deps, !changed)

(****)

let remove_irrelevant_deps confl deps = PTbl.map (simplify_formula confl) deps

let flatten_and_simplify ?(aggressive=false) dist deps confl =
  let confl = Conflict.copy confl in
let t = Unix.gettimeofday () in
  let deps = flatten_dependencies dist deps confl in
  let rec remove_conflicts deps =
    let (deps, changed) = remove_self_conflicts dist deps confl in
    remove_redundant_conflicts dist deps confl;
    let deps = flatten_dependencies dist deps confl in
    if changed then
      remove_conflicts deps
    else
      deps
  in
  let deps =
    if aggressive then
      remove_conflicts deps
    else begin
      remove_redundant_conflicts dist deps confl;
      remove_irrelevant_deps confl deps
    end
  in

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
  let compositions = ref PSetMap.empty in
  let is_composition fd2 p f d =
    Formula.exists
      (fun d' ->
         not (Disj.equiv d d') && not (Disj.equiv (Disj.lit p) d') &&
         let f' =
           try
             PSetMap.find (Disj.to_lits d') !compositions
           with Not_found ->
             let f' =
               Disj.fold (fun p f -> Formula.disj (PTbl.get fd2 p) f)
                 d' Formula._false
             in
             compositions := PSetMap.add (Disj.to_lits d') f' !compositions;
             f'
         in
         Formula.exists (fun d'' -> Disj.implies d d'') f')
      f
  in

  let rec remove_deps deps =
    let changed = ref false in
    compositions := PSetMap.empty;
    let deps =
      PTbl.mapi
        (fun p f ->
           Formula.filter (fun d ->
(*if maybe_remove deps p f d then Format.printf ">>>> %a@." (Formula.print dist) f;*)
             let remove =
               maybe_remove deps p f d && not (is_composition deps p f d)
             in
             if remove then changed := true;
             not remove) f)
        deps
    in
    if !changed then remove_deps deps else deps
  in

  let deps = remove_deps deps in

Format.eprintf "Flattening: %fs@." (Unix.gettimeofday () -. t);
  (deps, confl)

end

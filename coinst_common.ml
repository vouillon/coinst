(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2010-2011 Jérôme Vouillon
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

let debug_time = Debug.make "time" "Print execution times" []

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

(*
let t = ref (Unix.gettimeofday ())
let sample f =
  let t' = Unix.gettimeofday () in
  if t' -. !t > 1. then begin t := t'; f () end
*)

let simplify_formula confl f =
  Formula.filter
    (fun d ->
       Disj.for_all
         (fun p ->
            Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d)
    f

type flatten_data = {
  f_tbl : (Package.t, Formula.t) Hashtbl.t;
  f_dist : pool; f_deps : dependencies; f_confl : Conflict.t }

let rec flatten_deps data visited l =
  Formula.fold
    (fun d (l, r) ->
       let (l', r') =
         Disj.fold
           (fun i (l, r) ->
(*
sample (fun () -> Format.eprintf "(2) %a@." (Formula.print data.f_dist) l);
*)
              let (l', r') = flatten_dep data visited i in
              (simplify_formula data.f_confl (Formula.disj l' l),
               PSet.union r r'))
           d (Formula._false, r)
       in
       (Formula.conj l' l, r'))
    l (Formula._true, PSet.empty)

and flatten_dep data visited i =
let res =
  try
    (Hashtbl.find data.f_tbl i, PSet.empty)
  with Not_found ->
    let res =
      if List.mem i visited then
        (Formula._true, PSet.singleton i)
      else begin
        let (l, r) =
          flatten_deps data (i :: visited) (PTbl.get data.f_deps i)
        in
        let r = PSet.remove i r in
        if Conflict.has data.f_confl i then
          (Formula.conj (Formula.lit i) l, r)
        else
          (l, r)
      end
    in
    (* Only cache the result if it is unconditionally true *)
    if PSet.is_empty (snd res) then Hashtbl.add data.f_tbl i (fst res);
    res
in
(*
sample (fun () -> Format.eprintf "(1) %a@." (Formula.print data.f_dist) (fst res));
*)
res

let flatten_dependencies dist deps confl =
  let data =
    { f_tbl = Hashtbl.create 17; f_dist = dist;
      f_deps = deps; f_confl = confl }
  in
  PTbl.init dist (fun p -> fst (flatten_dep data [] p))

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
  let deps0 = deps in
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
               Disj.fold
                 (fun p f ->
                    simplify_formula confl (Formula.disj (PTbl.get fd2 p) f))
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

  if debug_time () then
    Format.eprintf "Flattening: %fs@." (Unix.gettimeofday () -. t);
  (deps, confl)

end

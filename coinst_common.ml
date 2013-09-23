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
let debug_irrelevant =
  Debug.make "irrelevant" "Debug irrelevant dependency removal" []

module F (M : Api.S) = struct

module Repository = Repository.F(M)
open Repository
module Quotient = Quotient.F (Repository)

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

let not_clearly_irrelevant confl d =
  Disj.for_all
    (fun p -> Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d

let simplify_formula confl f =
  Formula.filter (fun d -> not_clearly_irrelevant confl d) f

type flatten_data = {
  f_computed : bool PTbl.t; f_flatten_deps : dependencies;
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
  if PTbl.get data.f_computed i then
    (PTbl.get data.f_flatten_deps i, PSet.empty)
  else
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
    if PSet.is_empty (snd res) then begin
      PTbl.set data.f_flatten_deps i (fst res);
      PTbl.set data.f_computed i true
    end;
    res
in
(*
sample (fun () -> Format.eprintf "(1) %a@." (Formula.print data.f_dist) (fst res));
*)
res

let flatten_dependencies dist deps confl =
  let data =
    { f_flatten_deps = PTbl.create dist Formula._true;
      f_computed = PTbl.create dist false;
      f_dist = dist; f_deps = deps; f_confl = confl }
  in
  PTbl.iteri (fun p _ -> ignore (flatten_dep data [] p)) data.f_computed;
  data.f_flatten_deps

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

let remove_clearly_irrelevant_deps confl deps =
  PTbl.map (simplify_formula confl) deps

let is_composition add_dependency (*dist*) confl deps p d0 =
  let f = PTbl.get deps p in
  Formula.exists
    (fun d' ->
       not (Disj.equiv d0 d') && not (Disj.equiv (Disj.lit p) d') &&
       let s = Disj.diff d0 d' in
       Disj.exists
         (fun q ->
            let d = if Disj.implies1 q d0 then Disj.disj1 q s else s in
            Formula.exists
              (fun d'' ->
                 let res =
                   Disj.implies d d'' &&
                   not_clearly_irrelevant confl (Disj.cut d' q d'')
                 in
(*
if res then Format.eprintf "%a %a@." (Package.print_name dist) p (Package.print_name dist) q;
*)
                 if res then begin add_dependency p q; add_dependency p p end;
                 res)
              (PTbl.get deps q))
         d')
    f

let possibly_irrelevant confl deps d =
  Disj.exists
    (fun q ->
       Conflict.for_all confl
         (fun r ->
            Formula.exists
              (fun d' ->
                 Disj.implies d' d && not (Disj.implies1 q d'))
              (PTbl.get deps r))
         q)
    d

let remove_irrelevant_deps dist confl deps blacklist =
  let deps = PTbl.copy deps in
  let removed_deps = PTbl.create dist Formula._true in
  let dependencies = PTbl.create dist PSet.empty in
  let considered = PTbl.create dist false in
  let in_queue = PTbl.create dist false in
  let queue = Queue.create () in
  let push p =
    if not (PTbl.get in_queue p) then begin
      Queue.push p queue;
      PTbl.set in_queue p true
    end
  in
  let add_dependency p q =
    if debug_irrelevant () then
      Format.eprintf "YY %a => %a@."
        (Package.print_name dist) p (Package.print_name dist) q;
    PTbl.set dependencies q (PSet.add p (PTbl.get dependencies q));
    if not (PTbl.get considered q) then push q
  in
  let rec dequeue f =
    try
      let p = Queue.pop queue in
      PTbl.set in_queue p false;
      PTbl.set considered p true;
      let changed = f p in
      if changed then begin
        let s = PTbl.get dependencies p in
        PTbl.set dependencies p PSet.empty;
        if PSet.mem p s then push p;
        PSet.iter push s
      end;
      dequeue f
    with Queue.Empty ->
      ()
  in
  let check_all f =
    PTbl.iteri (fun p c -> if not c then begin push p; dequeue f end)
      considered
  in
  check_all
    (fun p ->
       let changed = ref false in
       let f = PTbl.get deps p in
       let count f = Formula.fold (fun _ n -> n + 1) f 0 in
       PTbl.set deps p
         (Formula.filter
           (fun d ->
             let remove =
               Disj.cardinal d > 1 &&
               not (Disj.Set.mem d blacklist) &&
               possibly_irrelevant confl deps d &&
               not (is_composition add_dependency confl deps p d)
             in
             if remove then begin
               changed := true;
               PTbl.set removed_deps p
                 (Formula.conj (PTbl.get removed_deps p) (Formula.of_disj d))
             end;
             not remove)
           f);
       if debug_irrelevant () then
         Format.eprintf "XXX %a %b (%d %d)@."
           (Package.print_name dist) p !changed
           (count f) (count (PTbl.get deps p));
       !changed);
  (deps, removed_deps)

let flatten_and_simplify ?(aggressive=false) dist deps0 confl =
  let confl = Conflict.copy confl in
let t = Unix.gettimeofday () in
  let deps = flatten_dependencies dist deps0 confl in
  let rec remove_conflicts deps =
    let (deps, changed) = remove_self_conflicts dist deps confl in
    remove_redundant_conflicts dist deps confl;
    let deps = flatten_dependencies dist deps confl in
    if changed then remove_conflicts deps else deps
  in
  let deps =
    if aggressive then
      remove_conflicts deps
    else begin
      remove_redundant_conflicts dist deps confl;
      remove_clearly_irrelevant_deps confl deps
    end
  in

  let rec try_remove_deps blacklist deps =
    let (deps', removed_deps) =
      remove_irrelevant_deps dist confl deps blacklist in

    let problems = ref Disj.Set.empty in
    PTbl.iteri
      (fun p f ->
         if not (Formula.implies Formula._true f) then begin
           Formula.iter (PTbl.get deps0 p)
             (fun d' ->
                Disj.iter d'
                  (fun q ->
                     Formula.iter (PTbl.get removed_deps q)
                       (fun d'' ->
                          if
                            Formula.exists (fun d -> Disj.implies d'' d) f
                          then begin
                            problems :=
                              Disj.Set.add d'' !problems;
  (*
      Format.eprintf "XXXX %a => %a => %a  (%a)@." (Package.print_name dist) p (Package.print_name dist) q (Disj.print dist) d''
                            (Formula.print dist) f
  *)
                          end)))
         end)
      deps';

    if Disj.Set.is_empty !problems then
      deps'
    else
      try_remove_deps (Disj.Set.union blacklist !problems) deps
  in

  let t' = Unix.gettimeofday () in
  let deps = try_remove_deps Disj.Set.empty deps in

  if debug_time () then
    Format.eprintf "  Removing irrelevant deps: %fs@."
      (Unix.gettimeofday () -. t');
  if debug_time () then
    Format.eprintf "Flattening: %fs@." (Unix.gettimeofday () -. t);
  (deps, confl)

end

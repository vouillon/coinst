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

module S (R : Repository.S) = struct
  module type S = sig
    open R

    type t

    val perform : pool -> ?packages:PSet.t -> Formula.t PTbl.t -> t
    val trivial : pool -> t
    val subset : pool -> PSet.t -> t
    val from_partition : pool -> PSet.t -> Package.t list list -> t

    val formula : t -> Formula.t -> Formula.t
    val dependencies : t -> dependencies -> dependencies
    val conflicts : t -> Conflict.t -> Conflict.t
    val package_set : t -> PSet.t -> PSet.t

    val iter : (Package.t -> unit) -> t -> unit

    val print : t -> dependencies -> unit
    val print_class : t -> Format.formatter -> Package.t -> unit
    val clss : t -> Package.t -> PSet.t
    val class_size : t -> Package.t -> int

    val pool : t -> pool
  end
end

module F (R : Repository.S) = struct

open R

type t =
  { pool : pool;
    count : int;
    repr_tbl : Package.t PTbl.t;
    repr_map : PSet.t PMap.t }

let trivial pool =
  let count = ref 0 in
  let repr_map = ref PMap.empty in
  let repr_tbl =
    PTbl.init pool
      (fun p ->
        incr count; repr_map := PMap.add p (PSet.singleton p) !repr_map; p)
  in
  { pool = pool; count = !count; repr_tbl = repr_tbl; repr_map = !repr_map }

let subset pool s =
  let count = ref 0 in
  let repr_map = ref PMap.empty in
  let repr_tbl =
    PTbl.init pool
      (fun p ->
         if PSet.mem p s then begin
           incr count; repr_map := PMap.add p (PSet.singleton p) !repr_map
         end;
         p)
  in
  { pool = pool; count = !count; repr_tbl = repr_tbl; repr_map = !repr_map }

let from_partition pool pkgs partition =
  let repr_tbl = PTbl.create pool (Package.of_index (-1)) in
  let repr_map = ref PMap.empty in
  List.iter
    (fun l ->
       let s = List.fold_left (fun s p -> PSet.add p s) PSet.empty l in
       let s' = PSet.filter (fun p -> PSet.mem p pkgs) s in
       if not (PSet.is_empty s') then begin
         let p = PSet.choose s' in
         repr_map := PMap.add p s !repr_map;
         PSet.iter (fun q -> PTbl.set repr_tbl q p) s
       end)
    partition;
  { pool = pool; count = List.length partition;
    repr_tbl = repr_tbl; repr_map = !repr_map }

let perform pool ?packages deps =
  let classes_by_dep = Hashtbl.create 17 in
  let class_count = ref 0 in
  let add_package p f =
    let f = Formula.normalize f in
    let s =
      try
        Hashtbl.find classes_by_dep f
      with Not_found ->
        incr class_count;
        let s = ref PSet.empty in
        Hashtbl.add classes_by_dep f s;
        s
    in
    s := PSet.add p !s
  in
  begin match packages with
    None   -> PTbl.iteri add_package deps
  | Some s -> PSet.iter (fun p -> add_package p (PTbl.get deps p)) s
  end;
  (* Compute good representatives *)
  let repr_tbl = PTbl.create pool (Package.of_index (-1)) in
  let repr_map = ref PMap.empty in
  Hashtbl.iter
    (fun f {contents = s} ->
       let s' =
         Formula.fold (fun d s -> PSet.union (Disj.to_lits d) s)
           f PSet.empty
       in
       let s' = PSet.inter s s' in
       let p = try PSet.choose s' with Not_found -> PSet.choose s in
       repr_map := PMap.add p s !repr_map;
       PSet.iter (fun q -> PTbl.set repr_tbl q p) s)
    classes_by_dep;
  {pool = pool; count = !class_count;
   repr_map = !repr_map; repr_tbl = repr_tbl}

let print_class quotient ch p =
  let n = PSet.cardinal (PMap.find p quotient.repr_map) in
  if n = 1 then
    Format.fprintf ch "%a" (Package.print_name quotient.pool) p
  else
    Format.fprintf ch "%a (x %d)" (Package.print_name quotient.pool) p n

let print quotient deps =
  (* Output equivalence classes *)
  Util.title (Format.sprintf "EQUIVALENCE CLASSES (%d)" quotient.count);
  PMap.iter
    (fun p s ->
       (* Skip the class of always installable packages *)
       if not (Formula.implies Formula._true (PTbl.get deps p)) then begin
         Format.printf "%a:" (print_class quotient) p;
         PSet.iter
           (fun q -> Format.printf " %a" (Package.print quotient.pool) q) s;
         Format.printf "@."
       end)
    quotient.repr_map;
  Format.printf "@."

let repr quotient p = PTbl.get quotient.repr_tbl p

let formula quotient f =
  Formula.fold
    (fun d f ->
       Formula.conj
         (Disj.fold
            (fun p d -> Formula.disj (Formula.lit (repr quotient p)) d)
            d Formula._false)
         f)
    f Formula._true

let dependencies quotient deps =
  let class_deps = PTbl.create quotient.pool Formula._false in
  PMap.iter
    (fun p _ -> PTbl.set class_deps p (formula quotient (PTbl.get deps p)))
    quotient.repr_map;
  class_deps

let conflicts quotient confl =
  let c = Conflict.create quotient.pool in
  Conflict.iter confl
    (fun p1 p2 -> Conflict.add c (repr quotient p1) (repr quotient p2));
  c

let package_set quotient s =
  PSet.fold (fun p s -> PSet.add (repr quotient p) s) s PSet.empty

let clss quotient p = PMap.find p quotient.repr_map

let class_size quotient p = PSet.cardinal (clss quotient p)

let iter f quotient = PMap.iter (fun p _ -> f p) quotient.repr_map

let pool q = q.pool

end

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

module type S = sig
  type pool

  module Package : sig
    type t
    val compare : t -> t -> int
    val print : pool -> Format.formatter -> t -> unit
    val print_name : pool -> Format.formatter -> t -> unit
    val index : t -> int
    val of_index : int -> t
    val of_index_list : int list -> t list
  end

  module PSet : Ptset.SET_SIG with type elt = Package.t
  module PMap : Map.S with type key = Package.t
  val pset_indices : PSet.t -> Util.IntSet.t

  module PTbl : sig
    type 'a t
    val create : pool -> 'a -> 'a t
    val init : pool -> (Package.t -> 'a) -> 'a t
    val get : 'a t -> Package.t -> 'a
    val set : 'a t -> Package.t -> 'a -> unit
    val iteri : (Package.t -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (Package.t -> 'a -> 'b) -> 'a t -> 'b t
    val copy : 'a t -> 'a t
  end

  module type DISJ = sig
    type t
    val print : ?compact:bool -> pool -> Format.formatter -> t -> unit
    val implies : t -> t -> bool
    val equiv : t -> t -> bool
    val lit : Package.t -> t
    val lit_disj : Package.t list -> t
    val _false : t
    val disj : t -> t -> t
  end

  module Disj : sig
    include DISJ
    val iter : t -> (Package.t -> unit) -> unit
    val fold : (Package.t -> 'a -> 'a) -> t -> 'a -> 'a
    val cut : t -> Package.t -> t -> t
    val for_all : (Package.t -> bool) -> t -> bool
    val exists : (Package.t -> bool) -> t -> bool
    val filter : (Package.t -> bool) -> t -> t
    val compare : t -> t -> int
    val implies1 : Package.t -> t -> bool
    val to_lit : t -> Package.t option
    val to_lits : t -> PSet.t
    val of_lits : PSet.t -> t

    val diff : t -> t -> t
    val disj1 : Package.t -> t -> t
    val cardinal : t -> int
    module Set : Set.S with type elt = t
  end

  module Formula : sig
    include DISJ
    val _true : t
    val conj : t -> t -> t
    val conjl : t list -> t
    val implies1 : t -> Disj.t -> bool
    val iter : t -> (Disj.t -> unit) -> unit
    val of_disj : Disj.t -> t
    val fold : (Disj.t -> 'a -> 'a) -> t -> 'a -> 'a
    val filter : (Disj.t -> bool) -> t -> t
    val exists : (Disj.t -> bool) -> t -> bool
    val normalize : t -> t
  end

  type dependencies = Formula.t PTbl.t

  module Conflict : sig
    type t
    val create : pool -> t

    val check : t -> Package.t -> Package.t -> bool
    val add : t -> Package.t -> Package.t -> unit
    val remove : t -> Package.t -> Package.t -> unit
    val iter : t -> (Package.t -> Package.t -> unit) -> unit
    val copy : t -> t

    val has : t -> Package.t -> bool
    val of_package : t -> Package.t -> PSet.t

    val iter_on_packages : t -> (Package.t -> PSet.t -> unit) -> unit
    val exists : t -> (Package.t -> bool) -> Package.t -> bool
    val for_all : t -> (Package.t -> bool) -> Package.t -> bool
  end
end

module F (M : Api.S) = struct
  type pool = M.pool

  module Package = struct
    type t = int
    let compare (x : int) y = compare x y
    let print = M.print_pack
    let print_name = M.print_pack_name
    let index p = p
    let of_index p = p
    let of_index_list p = p
  end

  module PSet = Util.IntSet
  let print_set ch pr sep l = Util.print_list ch pr sep (PSet.elements l)
  let pset_of_lst l = List.fold_left (fun s x -> PSet.add x s) PSet.empty l
  let pset_map f s = pset_of_lst (List.map f (PSet.elements s))
  let pset_indices s = s

  module PMap = Map.Make (Package)

  module PTbl = struct
    type 'a t = 'a array
    let create pool v = Array.make (M.pool_size pool) v
    let init pool f =
      Array.init (M.pool_size pool) (fun i -> f (Package.of_index i))
    let get a i = a.(Package.index i)
    let set a i v = a.(Package.index i) <- v
    let iteri f a = Array.iteri (fun i v -> f (Package.of_index i) v) a
    let map = Array.map
    let mapi f a = Array.mapi (fun i v -> f (Package.of_index i) v) a
    let copy = Array.copy 
  end

  module type DISJ = sig
    type t
    val print : ?compact:bool -> pool -> Format.formatter -> t -> unit
    val implies : t -> t -> bool
    val equiv : t -> t -> bool
    val lit : Package.t -> t
    val lit_disj : Package.t list -> t
    val _false : t
    val disj : t -> t -> t
  end

  module Disj  = struct
    type t = PSet.t
    let print ?(compact=false) pool ch l =
      if PSet.is_empty l then
        Format.fprintf ch "MISSING"
      else
        print_set
          (if compact then Package.print_name pool else Package.print pool)
          " | " ch l
    let implies = PSet.subset
    let equiv = PSet.equal
    let lit = PSet.singleton
    let lit_disj l = List.fold_right PSet.add l PSet.empty
    let _false = PSet.empty
    let disj = PSet.union
    let iter s f = PSet.iter f s
    let cut d p d' = assert (PSet.mem p d); PSet.union (PSet.remove p d) d'
    let fold = PSet.fold
    let for_all = PSet.for_all
    let exists = PSet.exists
    let implies1 = PSet.mem
    let to_lit l = if PSet.cardinal l = 1 then Some (PSet.choose l) else None
    let to_lits l = l
    let of_lits l = l
    let filter = PSet.filter

    let normalize d =  pset_map (fun i -> i) d
    let compare = PSet.compare

    let diff = PSet.diff
    let cardinal = PSet.cardinal
    let disj1 = PSet.add
    module Set = Set.Make (struct type t = PSet.t let compare = compare end)
  end

  module Formula  = struct
    type t = Disj.t list
    let print ?compact pool ch d =
      Util.print_list (Disj.print ?compact pool) ", " ch d

    let of_disj d = [d]
    let lit p = of_disj (Disj.lit p)

    let lit_disj l = of_disj (Disj.lit_disj l)

    let implies1 l1 y = List.exists (fun x -> Disj.implies x y) l1
    let implies l1 l2 =
      List.for_all (fun y -> implies1 l1 y) l2
    let equiv l1 l2 =
      List.for_all (fun y -> List.exists (fun x -> Disj.equiv x y) l1) l2 &&
      List.for_all (fun y -> List.exists (fun x -> Disj.equiv x y) l2) l1

    let _true = []
    let conj1 l x =
      if implies1 l x then l else
      x :: List.filter (fun y -> not (Disj.implies x y)) l
    let conj l1 l2 = List.fold_left conj1 l1 l2
    let conjl l = List.fold_left conj _true l

    let _false = of_disj (Disj._false)
    let disj l1 l2 =
      List.fold_left
        (fun l x -> List.fold_left (fun l y -> conj1 l (Disj.disj x y)) l l2)
        _true l1
    let iter l f = List.iter f l
    let fold f l = List.fold_right f l
    let filter = List.filter
    let exists = List.exists

    let normalize f =
      let f = List.map Disj.normalize f in
      let f = List.sort PSet.compare f in
      f
  end

  type dependencies = Formula.t PTbl.t

  module Conflict  = struct
    type t = PSet.t PTbl.t
    let create pool = PTbl.create pool PSet.empty
    let has c p1 = not (PSet.is_empty (PTbl.get c p1))
    let check c p1 p2 = PSet.mem p1 (PTbl.get c p2)
    let add c p1 p2 =
      PTbl.set c p1 (PSet.add p2 (PTbl.get c p1));
      PTbl.set c p2 (PSet.add p1 (PTbl.get c p2))
    let remove c p1 p2 =
      PTbl.set c p1 (PSet.remove p2 (PTbl.get c p1));
      PTbl.set c p2 (PSet.remove p1 (PTbl.get c p2))
    let iter c f =
      PTbl.iteri (fun i s -> PSet.iter (fun j -> if i < j then f i j) s) c
    let iter_on_packages c f = PTbl.iteri f c
    let of_package = PTbl.get
    let copy = PTbl.copy 

    let exists c f p = PSet.exists f (PTbl.get c p)
    let for_all c f p = PSet.for_all f (PTbl.get c p)
  end

end

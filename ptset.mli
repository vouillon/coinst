(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: ptset.mli,v 1.10 2008-07-21 14:53:06 filliatr Exp $ i*)

(*s Restricted Set.S signature *)

module type SET_SIG = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val find : elt -> t -> int
  val add : elt -> t -> t
  val of_list : elt list -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val subset : t -> t -> bool
  val inter : t -> t -> t
  val diff : t -> t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val elements : t -> elt list
  val choose : t -> elt
  val cardinal : t -> int
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val split : elt -> t -> t * bool * t
  val min_elt : t -> int
  val max_elt : t -> int

  (*s Additional functions not appearing in the signature [Set.S] from ocaml
    standard library. *)

  (* [intersect u v] determines if sets [u] and [v] have a non-empty
     intersection. *)
  val intersect : t -> t -> bool
end

(*s Sets of integers implemented as Patricia trees.  The following
    signature is exactly [Set.S with type elt = int], with the same
    specifications. This is a purely functional data-structure. The
    performances are similar to those of the standard library's module
    [Set]. The representation is unique and thus structural comparison
    can be performed on Patricia trees. *)

include SET_SIG with type elt = int

(*s Warning: [min_elt] and [max_elt] are linear w.r.t. the size of the
    set. In other words, [min_elt t] is barely more efficient than [fold
    min t (choose t)]. *)

(*s Big-endian Patricia trees *)

module Big : SET_SIG with type elt = int


(*s Big-endian Patricia trees with non-negative elements. Changes:
    - [add] and [singleton] raise [Invalid_arg] if a negative element is given
    - [mem] is slightly faster (the Patricia tree is now a search tree)
    - [min_elt] and [max_elt] are now O(log(N))
    - [elements] returns a list with elements in ascending order
 *)

module BigPos : SET_SIG with type elt = int

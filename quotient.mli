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

module S (R : Repository.S) : sig
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

module F (R : Repository.S) : sig
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

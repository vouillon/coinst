(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2005-2011 Jérôme Vouillon
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
  type reason

  module Solver : Solver.SOLVER with type reason = reason

  val new_pool : unit -> pool

  val parse_packages : pool -> string list -> in_channel -> unit

  val parse_package_dependency : pool -> string -> int list
  val parse_package_name : pool -> string -> int list

  val print_pack : pool -> Format.formatter -> int -> unit
  val print_pack_name : pool -> Format.formatter -> int -> unit

  val show_reasons : pool -> reason list -> unit
  val conflicts_in_reasons : reason list -> (int * int) list

  val generate_rules : pool -> Solver.state

  val compute_conflicts : pool -> int list array
  val compute_deps : pool -> int list list array

  val pool_size : pool -> int


end


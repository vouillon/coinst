(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2011 Jérôme Vouillon
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
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int

  type id

  val initialize : ?signal_assign:(var array -> reason -> unit) -> int -> state
  val extend : state -> int -> unit
  val set_var_printer : state -> (Format.formatter -> var -> unit) -> unit

  val assignment : state -> Util.BitVect.t
  val direct_reasons : state -> var -> (var array * reason) list
  val reason : state -> var -> (var array * reason) option
  val assumptions : state -> var -> reason list

  val add_rule : state -> var array -> reason -> id
  val assume : state -> var -> reason -> unit
  val retract_rule : state -> id -> unit
  val retract_assumptions : state -> var -> unit
end

module F (X : S) : SOLVER  with type reason = X.reason

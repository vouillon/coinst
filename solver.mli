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
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int
  type lit
  val lit_of_var : var -> bool -> lit

  val initialize_problem :
    ?print_var:(Format.formatter -> int -> unit) -> int -> state
  val propagate : state -> unit

  val protect : state -> unit
  val reset : state -> unit

  type value = True | False | Unknown
  val assignment : state -> value array

  val add_rule : state -> lit array -> reason list -> unit
  val associate_vars : state -> lit -> var list -> unit

  val solve : state -> var -> bool
  val solve_lst : state -> var list -> bool

  val collect_reasons : state -> var -> reason list
  val collect_reasons_lst : state -> var list -> reason list
end

module F (X : S) : SOLVER with type reason = X.reason

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

module F (M : Api.S) : sig
  module Repository : Repository.S with type pool = M.pool
  open Repository
  module Quotient : Quotient.S(Repository).S

  val compute_dependencies_and_conflicts :
    pool -> Formula.t PTbl.t * Conflict.t
  val generate_rules :
    Quotient.t -> Formula.t PTbl.t -> Conflict.t -> M.Solver.state

  val remove_irrelevant_deps :
    Conflict.t -> Formula.t PTbl.t -> Formula.t PTbl.t
  val flatten_and_simplify :
    ?aggressive:bool ->
    pool -> Formula.t PTbl.t -> Conflict.t -> Formula.t PTbl.t * Conflict.t
end

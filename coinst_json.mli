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

module F (R : Repository.S) : sig
  open R

  val output :
    ?options:string list ->
    ?package_weight:(Package.t -> float) ->
    ?package_emph:(Package.t -> bool) ->
    ?edge_color:(Package.t -> Formula.t -> Disj.t -> string option) ->
    ?grayscale:bool ->
    string -> ?mark_all:bool -> ?mark_reversed:bool -> ?roots:Package.t list ->
    Quotient.F(R).t -> dependencies -> Conflict.t -> unit

  val output_non_coinstallable_sets :
    string -> Quotient.F(R).t -> PSet.t list -> unit
end

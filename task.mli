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

type 'a t
type 'a future

val spawn : (unit -> 'a) -> 'a t
val funct : ('a -> 'b -> 'c) -> 'a t -> 'b -> 'c future
val wait : 'a future -> 'a
val kill : 'a t -> unit

val map : 'a list -> ('a -> 'b future) -> ('b -> 'c) -> 'c list
val iter : 'a list -> ('a -> 'b future) -> ('b -> unit) -> unit
val iteri : 'a list -> ('a -> ('b * 'c future)) -> ('b -> 'c -> unit) -> unit
val iter_ordered : 'a list -> ('a -> 'b future) -> ('b -> unit) -> unit
val iteri_ordered :
  'a list -> ('a -> ('b * 'c future)) -> ('b -> 'c -> unit) -> unit

type scheduler

val scheduler : unit -> scheduler
val async : scheduler -> 'a future -> ('a -> unit) -> unit
val run : scheduler -> unit

val get_processor_count : unit -> int
val set_processor_count : int -> unit

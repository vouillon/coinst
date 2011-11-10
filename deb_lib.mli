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

type rel
type version = int * string * string option
type deb_reason =
    R_conflict
    of int * int * (int * (string * (rel * version) option) list) option
  | R_depends
    of int * (string * (rel * version) option) list
type dep = (string * (rel * version) option) list list

type p =
  { mutable num : int;
    mutable package : string;
    mutable version : version;
    mutable source : string * version;
    mutable section : string;
    mutable architecture : string;
    mutable depends : dep;
    mutable recommends : dep;
    mutable suggests : dep;
    mutable enhances : dep;
    mutable pre_depends : dep;
    mutable provides : dep;
    mutable conflicts : dep;
    mutable breaks : dep;
    mutable replaces : dep }

type deb_pool =
  { mutable size : int;
    packages : (string * version, p) Hashtbl.t;
    packages_by_name : (string, p) Util.ListTbl.t;
    packages_by_num : (int, p) Hashtbl.t;
    provided_packages : (string, p) Util.ListTbl.t }

include Api.S with type reason = deb_reason and type pool = deb_pool

val package_name : pool -> int -> string

val resolve_package_dep :
  pool -> string * (rel * (int * string * string option)) option -> int list
val resolve_package_dep_raw :
  pool -> string * (rel * (int * string * string option)) option -> p list

val only_latest : pool -> pool

val copy : pool -> pool
val merge : pool -> (int -> bool) -> pool -> unit
val merge2 : pool -> (p -> bool) -> pool -> unit
val add_package : pool -> p -> int

val parse_version : string -> version
val print_version : Format.formatter -> version -> unit
val compare_version : version -> version -> int

type s =
  { mutable s_name : string;
    mutable s_version : version;
    mutable s_section : string }

type s_pool =
  { mutable s_size : int;
    s_packages : (string, s) Hashtbl.t }

val new_src_pool : unit -> s_pool
val parse_src_packages : s_pool -> in_channel -> unit
val src_only_latest : s_pool -> s_pool

val generate_rules_restricted : pool -> Util.IntSet.t -> Solver.state

val print_package_dependency : Format.formatter -> dep -> unit

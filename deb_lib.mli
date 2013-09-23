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
type package_name
type version
type 'a dep = ('a * (rel * version) option) list
type deps = package_name dep list
type deb_reason =
    R_conflict of int * int * (int * package_name dep) option
  | R_depends of int * package_name dep

type p =
  { mutable num : int;
    mutable package : package_name;
    mutable version : version;
    mutable source : string * version;
    mutable section : string;
    mutable architecture : string;
    mutable depends : deps;
    mutable recommends : deps;
    mutable suggests : deps;
    mutable enhances : deps;
    mutable pre_depends : deps;
    mutable provides : deps;
    mutable conflicts : deps;
    mutable breaks : deps;
    mutable replaces : deps }

type deb_pool

include Api.S with type reason = deb_reason and type pool = deb_pool

module Dict : sig type t end
val name_of_id : package_name -> string
val id_of_name : string -> package_name
val add_name : string -> package_name
val set_dict : Dict.t -> unit
val current_dict : unit -> Dict.t

module PkgTbl : Hashtbl.S with type key = package_name
module PkgSet : Set.S with type elt = package_name

val find_package_by_num : pool -> int -> p
val find_packages_by_name : pool -> package_name -> p list
val has_package_of_name : pool -> package_name -> bool
val find_provided_packages : pool -> package_name -> p list
val iter_packages : pool -> (p -> unit) -> unit
val iter_packages_by_name : pool -> (package_name -> p list -> unit) -> unit
val pool_size : pool -> int

val package_name : pool -> int -> string

val resolve_package_dep :
  pool -> package_name * (rel * version) option -> int list
val resolve_package_dep_raw :
  pool -> package_name * (rel * version) option -> p list
val dep_can_be_satisfied :
  pool -> package_name * (rel * version) option -> bool

val copy : pool -> pool
val merge : pool -> (p -> bool) -> pool -> unit
val only_latest : pool -> pool
val add_package : pool -> p -> int
val remove_package : pool -> p -> unit
val replace_package : pool -> p -> p -> unit

val parse_version : string -> version
val print_version : Format.formatter -> version -> unit
val compare_version : version -> version -> int

type s =
  { mutable s_name : string;
    mutable s_version : version;
    mutable s_section : string;
    mutable s_extra_source : bool }

type s_pool =
  { mutable s_size : int;
    s_packages : s Util.StringTbl.t }

val new_src_pool : unit -> s_pool
val parse_src_packages : s_pool -> in_channel -> unit
val src_only_latest : s_pool -> s_pool

val generate_rules_restricted : pool -> Util.IntSet.t -> Solver.state

val print_package_dependency : Format.formatter -> string dep list -> unit

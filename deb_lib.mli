
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
    packages_by_name : (string, p list ref) Hashtbl.t;
    packages_by_num : (int, p) Hashtbl.t;
    provided_packages : (string, p list ref) Hashtbl.t }

include Api.S with type reason = deb_reason and type pool = deb_pool

val package_name : pool -> int -> string

val resolve_package_dep :
  pool -> string * (rel * (int * string * string option)) option -> int list

val only_latest : pool -> pool

val merge : pool -> (int -> bool) -> pool -> unit
val merge2 : pool -> (p -> bool) -> pool -> unit
val add_package : pool -> p -> unit

val parse_version : string -> version
val print_version : Format.formatter -> version -> unit
val compare_version : version -> version -> int

val parse_src_packages : (string, version) Hashtbl.t -> in_channel -> unit
val src_only_latest :
  (string, version) Hashtbl.t -> (string, version) Hashtbl.t

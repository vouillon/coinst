
type rel
type version = int * string * string option
type deb_reason =
    R_conflict
    of int * int * (int * (string * (rel * version) option) list) option
  | R_depends
    of int * (string * (rel * version) option) list

include Api.S with type reason = deb_reason

val package_name : pool -> int -> string

val resolve_package_dep :
  pool -> string * (rel * (int * string * string option)) option -> int list


module StringSet : Set.S with type elt = string

module Repository : Repository.S with type pool = Deb_lib.pool
open Repository
module PSetSet : Set.S with type elt = PSet.t

val analyze :
  ?check_new_packages:bool ->
  pool ->
  pool ->
  Formula.t PTbl.t *
  Formula.t PTbl.t *
  Deb_lib.Solver.var PTbl.t * Deb_lib.Solver.state * PSetSet.t *
  PSet.t * Conflict.t *
  PSet.t PTbl.t *
  (PSet.t * string * PSet.t *
   Formula.t PTbl.t * Conflict.t *
   Formula.t)
  list * PSet.t

val find_problematic_packages :
  ?check_new_packages:bool ->
  Deb_lib.pool -> Deb_lib.pool -> (string -> bool) -> StringSet.t


module StringSet : Set.S with type elt = string

module Repository : Repository.S with type pool = Deb_lib.pool
open Repository
module PSetSet : Set.S with type elt = PSet.t

type state =
  { dist : Deb_lib.deb_pool;
    deps : Formula.t PTbl.t;
    confl : Conflict.t;
    deps' : Formula.t PTbl.t;
    confl' : Conflict.t;
    st : Deb_lib.Solver.state }

val prepare_analyze : pool -> state

val analyze :
  ?check_new_packages:bool ->
  state ->
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
  ?check_new_packages:bool -> state -> pool -> (string -> bool) -> StringSet.t

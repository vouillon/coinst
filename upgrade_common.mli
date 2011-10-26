
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

type clause = { pos : StringSet.t; neg : StringSet.t }
type issue =
  { i_issue : PSet.t;
    i_clause : clause;
    (* FIX: compute this information at a later stage? *)
    i_nodes : PSet.t;
    i_deps : Formula.t PTbl.t;
    i_confl : Conflict.t }

val analyze :
  ?check_new_packages:bool ->
  ?reference:state ->
  state -> pool ->
  Formula.t PTbl.t * Formula.t PTbl.t *
  Deb_lib.Solver.var PTbl.t * Deb_lib.Solver.state * PSetSet.t *
  PSet.t * Conflict.t * PSet.t PTbl.t *
  issue list * (Package.t * clause) list

val find_problematic_packages :
  ?check_new_packages:bool ->
  state -> state -> (string -> bool) -> clause list

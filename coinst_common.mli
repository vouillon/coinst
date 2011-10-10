
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


module F (R : Repository.S) : sig
  open R

  val output :
    string -> bool ->
    ?package_weight:(Package.t -> float) ->
    ?edge_color:(Package.t -> Formula.t -> Disj.t -> string) ->
    Quotient.F(R).t -> dependencies -> Conflict.t -> unit
end


module F (R : Repository.S) : sig
  val output :
    string -> R.pool -> bool -> R.Conflict.t ->
    (R.Package.t, R.Package.t list * R.Formula.t * R.PSet.t list) Hashtbl.t ->
    unit
end


module F (R : Repository.S) : sig
  open R

  type t

  val perform : pool -> Formula.t PTbl.t -> t

  val formula : t -> Formula.t -> Formula.t
  val dependencies : t -> dependencies -> dependencies
  val conflicts : t -> Conflict.t -> Conflict.t

  val iter : (Package.t -> unit) -> t -> unit

  val print : t -> dependencies -> unit
  val print_class : t -> Format.formatter -> Package.t -> unit
  val class_size : t -> Package.t -> int

  val pool : t -> pool
end

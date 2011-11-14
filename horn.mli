module BitVect : sig
  type t
  val make : int -> bool -> t
  val test : t -> int -> bool
  val set : t -> int -> unit
  val clear : t -> int -> unit
  val sub : t -> int -> int -> t
end

module type S = sig
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int

  val initialize : ?signal_assign:(var array -> reason -> unit) -> int -> state
  val extend : state -> int -> unit
  val set_var_printer : state -> (Format.formatter -> var -> unit) -> unit

  val assignment : state -> BitVect.t
  val direct_reasons : state -> var -> (var array * reason) list
  val reason : state -> var -> (var array * reason) option
  val assumptions : state -> var -> reason list

  val add_rule : state -> var array -> reason -> unit
  val assume : state -> var -> reason -> unit
  val retract_assumptions : state -> var -> unit
end

module F (X : S) : SOLVER  with type reason = X.reason

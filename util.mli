
val set_msg : string -> unit
val hide_msg : unit -> unit
val show_msg : unit -> unit

val set_warning_location : string -> unit
val reset_warning_location : unit -> unit
val print_warning : string -> unit

val fail : string -> 'a

val title : string -> unit

module Timer : sig
  type t
  val start : unit -> t
  val stop : t -> float
end


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

module IntSet : Set.S with type elt = int

module ListTbl : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val iter : ('a -> 'b list -> unit) -> ('a, 'b) t -> unit
end

val print_list :
  (Format.formatter -> 'a -> unit) -> string ->
  Format.formatter -> 'a list -> unit

val make_directories : string -> unit
(* Make sure that the directory containing the file given in argument
   exists. *)

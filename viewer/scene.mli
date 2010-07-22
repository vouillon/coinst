
type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float
type element =
    Path of command array * color option * color option
  | Polygon of (float * float) array * color option * color option
  | Ellipse of float * float * float * float * color option * color option
  | Text of
      float * float * string * string * float * color option * color option

(****)

val rectangle :
  float * float * float * float -> color option -> color option -> element

(****)

type t

val make : unit -> t
val add : t -> element -> unit
val get : t -> element array

(***)

val to_json : Format.formatter -> t -> unit

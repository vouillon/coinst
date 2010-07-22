
type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float

type 'c element =
    Path of command array * 'c option * 'c option
  | Polygon of (float * float) array * 'c option * 'c option
  | Ellipse of float * float * float * float * 'c option * 'c option
  | Text of
      float * float * string * string * float * 'c option * 'c option

(****)

val rectangle :
  float * float * float * float -> 'c option -> 'c option -> 'c element

(****)

type t

val make : unit -> t
val add : t -> color element -> unit
val get : t -> color element array

(***)

val to_json : Format.formatter -> t -> unit

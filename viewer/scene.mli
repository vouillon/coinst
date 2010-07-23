
type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float

type ('color, 'font, 'text) element =
    Path of command array * 'color option * 'color option
  | Polygon of (float * float) array * 'color option * 'color option
  | Ellipse of float * float * float * float * 'color option * 'color option
  | Text of
      float * float * 'text * 'font * 'color option * 'color option

(****)

val rectangle :
  float * float * float * float -> 'color option -> 'color option ->
  ('color, 'font, 'text) element

(****)

type ('color, 'font, 'text) t
type cairo_t = (float * float * float, string * float, string) t

val make : unit -> ('color, 'font, 'text) t
val add : ('color, 'font, 'text) t -> ('color, 'font, 'text) element -> unit
val get : ('color, 'font, 'text) t -> ('color, 'font, 'text) element array

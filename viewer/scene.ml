
type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float

type ('color, 'font, 'text) element =
    Path of command array * 'color option * 'color option
  | Polygon of (float * float) array * 'color option * 'color option
  | Ellipse of float * float * float * float * 'color option * 'color option
  | Text of float * float * 'text * 'font * 'color option * 'color option

(****)

let rectangle (x1, y1, x2, y2) fill stroke =
  Polygon ([|(x1, y1); (x2, y1); (x2, y2); (x1, y2)|], fill, stroke)

(****)

type ('color, 'font, 'text) t = ('color, 'font, 'text) element list ref
type cairo_t = (float * float * float, string * float, string) t

let make () = ref []

let add sc e = sc := e :: !sc

let get sc = Array.of_list (List.rev !sc)


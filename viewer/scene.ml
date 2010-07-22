
(*
XXX
use a separate table of colors and font information?
*)

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

let rectangle (x1, y1, x2, y2) fill stroke =
  Polygon ([|(x1, y1); (x2, y1); (x2, y2); (x1, y2)|], fill, stroke)

(****)

type t = element list ref

let make () = ref []

let add sc e = sc := e :: !sc

let get sc = Array.of_list (List.rev !sc)

(****)

let string_to_json ch s =
(*XXX Escape! *)
  Format.fprintf ch "\"%s\"" s

let color_to_json ch c =
  match c with
    None ->
      Format.fprintf ch "0"
  | Some (r, g, b) ->
      Format.fprintf ch "@[<1>[0,@,%.3g,@,%.3g,@,%.3g]@]" r g b

let commands_to_json ch c =
  Format.fprintf ch "@[<1>[0";
  Array.iter
    (fun c ->
       match c with
         Move_to (x, y) ->
           Format.fprintf ch ",@,@[<1>[0,@,%g,@,%g]@]" x y
       | Curve_to (x1, y1, x2, y2, x3, y3) ->
           Format.fprintf ch
             ",@,@[<1>[1,@,%g,@,%g,@,%g,@,%g,@,%g,@,%g]@]"
             x1 y1 x2 y2 x3 y3)
    c;
  Format.fprintf ch "]@]"

let points_to_json ch p =
  Format.fprintf ch "@[<1>[0";
  Array.iter (fun (x, y) -> Format.fprintf ch ",@,@[<1>[0,@,%g,@,%g]@]" x y)
    p;
  Format.fprintf ch "]@]"

let element_to_json ch e =
  match e with
    Path (cmds, fill, stroke) ->
      Format.fprintf ch ",@,@[<1>[0,@,%a,@,%a,@,%a]@]"
        commands_to_json cmds color_to_json fill color_to_json stroke
  | Polygon (l, fill, stroke) ->
      Format.fprintf ch ",@,@[<1>[1,@,%a,@,%a,@,%a]@]"
        points_to_json l color_to_json fill color_to_json stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Format.fprintf ch ",@,@[<1>[2,@,%g,@,%g,@,%g,@,%g,@,%a,@,%a]@]"
        cx cy rx ry color_to_json fill color_to_json stroke
  | Text (x, y, txt, font, size, fill, stroke) ->
      Format.fprintf ch ",@,@[<1>[3,@,%g,@,%g,@,%a,@,%a,@,%g,@,%a,@,%a]@]"
        x y string_to_json txt string_to_json font size
        color_to_json fill color_to_json stroke

let to_json ch sc =
  let l = get sc in
  Format.fprintf ch "@[<1>[0";
  Array.iter (fun e -> element_to_json ch e) l;
  Format.fprintf ch "]@]@."

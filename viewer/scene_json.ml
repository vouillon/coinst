
open Scene

let array_stringify f ch l =
  Format.fprintf ch "@[<1>[0";
  Array.iter (fun e -> Format.fprintf ch ",@,%a" f e) l;
  Format.fprintf ch "]@]"

let string_stringify ch s =
(*XXX Escape! *)
  Format.fprintf ch "\"%s\"" s

let color_stringify ch c =
  match c with
    None ->
      Format.fprintf ch "0"
  | Some (r, g, b) ->
      let h v = truncate (v *. 255.99) in
      Format.fprintf ch "@[<1>[0,@,%a]@]"
        string_stringify (Format.sprintf "#%02x%02x%02x" (h r) (h g) (h b))

let font_stringify ch (font, size) =
  Format.fprintf ch "%a" string_stringify (Format.sprintf "%gpx %s" size font)

let command_stringify ch c =
  match c with
    Move_to (x, y) ->
      Format.fprintf ch "@[<1>[0,@,%g,@,%g]@]" x y
  | Curve_to (x1, y1, x2, y2, x3, y3) ->
      Format.fprintf ch "@[<1>[1,@,%g,@,%g,@,%g,@,%g,@,%g,@,%g]@]"
        x1 y1 x2 y2 x3 y3

let commands_stringify = array_stringify command_stringify

let point_stringify ch (x, y) = Format.fprintf ch "@[<1>[0,@,%g,@,%g]@]" x y

let points_stringify = array_stringify point_stringify

let rect_stringify ch (x1, y1, x2, y2) =
  Format.fprintf ch "@[<1>[0,@,%g,@,%g,@,%g,@,%g]@]" x1 y1 x2 y2

let rect_array_stringify = array_stringify rect_stringify

let element_stringify ch e =
  match e with
    Path (cmds, fill, stroke) ->
      Format.fprintf ch "@[<1>[0,@,%a,@,%a,@,%a]@]"
        commands_stringify cmds color_stringify fill color_stringify stroke
  | Polygon (l, fill, stroke) ->
      Format.fprintf ch "@[<1>[1,@,%a,@,%a,@,%a]@]"
        points_stringify l color_stringify fill color_stringify stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Format.fprintf ch "@[<1>[2,@,%g,@,%g,@,%g,@,%g,@,%a,@,%a]@]"
        cx cy rx ry color_stringify fill color_stringify stroke
  | Text (x, y, txt, font, fill, stroke) ->
      Format.fprintf ch "@[<1>[3,@,%g,@,%g,@,%a,@,%a,@,%a,@,%a]@]"
        x y string_stringify txt font_stringify font
        color_stringify fill color_stringify stroke

let stringify = array_stringify element_stringify

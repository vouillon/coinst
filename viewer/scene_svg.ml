
open Scene

let color c =
  match c with
    None ->
      "none"
  | Some (r, g, b) ->
      let h v = truncate (v *. 255.99) in
      Format.sprintf "#%02x%02x%02x" (h r) (h g) (h b)

let command c =
  match c with
    Move_to (x, y) ->
      Format.sprintf "M%g,%g" x y
  | Curve_to (x1, y1, x2, y2, x3, y3) ->
      Format.sprintf "C%g,%g %g,%g %g,%g" x1 y1 x2 y2 x3 y3

let style s =
  match s with
    "dashed" -> " stroke-dasharray='5,2'"
  | "dotted" -> " stroke-dasharray='1,3'"
  | _        -> ""

let (>>) v f = f v

let comma_re = Str.regexp ","

let format f ((x1, y1, x2, y2), scene) =
  Format.fprintf f "<svg width='%gpt' height='%gpt' viewBox='%g %g %g %g'>\n"
    (x2 -. x1) (y2 -. y1) x1 y1 (x2 -. x1) (y2 -. y1);
  Array.iter
    (fun e ->
       match e with
         Path (cmds, fill, stroke, st) ->
           Format.fprintf f "<path d='%s' fill='%s' stroke='%s'%s/>\n"
             (cmds >> Array.map command >> Array.to_list >> String.concat "")
             (color fill) (color stroke) (style st)
       | Polygon (pts, fill, stroke, st) ->
           Format.fprintf f "<polygon points='%s' fill='%s' stroke='%s'%s/>\n"
             (pts
              >> Array.map (fun (x, y) -> Format.sprintf "%g,%g" x y)
              >> Array.to_list
              >> String.concat " ")
             (color fill) (color stroke) (style st)
       | Ellipse (cx, cy, rx, ry, fill, stroke, st) ->
           Format.fprintf f
             "<ellipse cx='%g' cy='%g' rx='%g' ry='%g' \
                 fill='%s' stroke='%s'%s/>\n"
             cx cy rx ry (color fill) (color stroke) (style st)
       | Text (x1, y1, txt, (font, size), fill, stroke) ->
           Format.fprintf f
             "<text x='%g' y='%g' font-family='%s' font-size='%g' \
                 style='dominant-baseline:central' text-anchor='middle' \
                 fill='%s' stroke='%s'>\
              %s</text>\n"
             x1 y1 font size (color fill) (color stroke) txt)
    scene;
  Format.fprintf f "</svg>\n@?";


open Scene

let offset_command x y c =
  match c with
    Move_to (x1, y1) ->
      Move_to (x1 -. x, y1 -. y)
  | Curve_to (x1, y1, x2, y2, x3, y3) ->
      Curve_to (x1 -. x, y1 -. y, x2 -. x , y2 -. y, x3 -. x , y3 -. y)

let offset_element x y e =
  match e with
    Path (cmds, c1, c2) ->
      Path (Array.map (fun c -> offset_command x y c) cmds, c1, c2)
  | Polygon (pts, c1, c2) ->
      Polygon (Array.map (fun (x1, y1) -> (x1 -. x, y1 -. y)) pts, c1, c2)
  | Ellipse (cx, cy, rx, ry, c1, c2) ->
      Ellipse (cx -. x, cy -. y, rx, ry, c1, c2)
  | Text (x1, y1, txt, font, c1, c2) ->
      Text (x1 -. x, y1 -. y, txt, font, c1, c2)

let _ =
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let (_, g) = Dot_graph.of_channel ch in
  let (bbox, scene) = Dot_render.f g in
  let (x1, y1, x2, y2) = bbox in
  let l = Scene.get scene in
  let l = Array.map (fun e -> offset_element x1 y1 e) l in
  let bbox = (0., 0., x2 -. x1, y2 -. y1) in
  let i = Cairo.image_surface_create Cairo.FORMAT_ARGB32 1024 1024 in
  let bboxes = Scene_extents.compute (Cairo.create i) l in
  Format.printf "@[<1>[0,@,%a,@,%a,@,%a]@]@."
    Scene_json.rect_stringify bbox
    Scene_json.rect_array_stringify bboxes
    Scene_json.stringify l

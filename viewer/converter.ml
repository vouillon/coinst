let _ =
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let (_, g) = Dot_graph.of_channel ch in
  let (bbox, scene) = Dot_render.f g in
  let l = Scene.get scene in
  let i = Cairo.image_surface_create Cairo.FORMAT_ARGB32 1024 1024 in
  let bboxes = Scene_extents.compute (Cairo.create i) l in
  Format.printf "@[<1>[0,@,%a,@,%a,@,%a]@]@."
    Scene_json.rect_stringify bbox
    Scene_json.rect_array_stringify bboxes
    Scene_json.stringify l


let _ =
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let (_, g) = Dot_graph.of_channel ch in
  let (bbox, scene) = Dot_render.f g in

  ignore (GMain.Main.init ());

  Viewer.create (*~full_screen:true*) bbox scene;

  GMain.main ()

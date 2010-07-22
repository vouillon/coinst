(*
===> browser version

===> metadata ===> tooltips
===> if we do not have a bb, run the file through dot
*)

(*
Try to make this generic (!)

Functionalities
===============
- search for a package
  ===> use metadata in the dot file
- remove a package and recompute
- follow an edge backwards and forwards
- information on a node/edges
*)

let _ =
  let ch = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
  let (_, g) = Dot_graph.of_channel ch in
  let (bbox, scene) = Dot_render.f g in

(*
Format.eprintf "%a" Scene.to_json scene;
*)

  ignore (GMain.Main.init ());

  Viewer.create (*~full_screen:true*) bbox scene;

  GMain.main ()

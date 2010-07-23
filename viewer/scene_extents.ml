
open Scene

let pi = 4. *. atan 1.

let path_extent ctx fill stroke =
  if stroke <> None then Cairo.stroke_extents ctx
  else Cairo.fill_extents ctx

let compute_extent ctx e =
  Cairo.new_path ctx;
  match e with
    Path (cmd, fill, stroke) ->
      Array.iter
        (fun c ->
           match c with
             Move_to (x, y) ->
               Cairo.move_to ctx x y
           | Curve_to (x1, y1, x2, y2, x3, y3) ->
               Cairo.curve_to ctx x1 y1 x2 y2 x3 y3)
        cmd;
      path_extent ctx fill stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Cairo.save ctx;
      Cairo.translate ctx cx cy;
      Cairo.scale ctx rx ry;
      Cairo.arc ctx 0. 0. 1. 0. (2. *. pi);
      Cairo.restore ctx;
      path_extent ctx fill stroke
  | Polygon (points, fill, stroke) ->
      Array.iteri
        (fun i (x, y) ->
           if i = 0 then Cairo.move_to ctx x y else Cairo.line_to ctx x y)
        points;
      Cairo.close_path ctx;
      path_extent ctx fill stroke
  | Text (x, y, txt, (font, font_size), fill, stroke) ->
      Cairo.select_font_face ctx font
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size ctx font_size;
      let ext = Cairo.text_extents ctx txt in
      (x -. ext.Cairo.text_width /. 2. -. 5.,
       y +. ext.Cairo.y_bearing -. 5.,
       x +. ext.Cairo.text_width /. 2. +. 5.,
       y +. ext.Cairo.y_bearing +. ext.Cairo.text_height +. 5.)

let compute ctx l = Array.map (fun e -> compute_extent ctx e) l

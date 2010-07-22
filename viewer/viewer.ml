
open Scene

let pi = 4. *. atan 1.

(****)

let set_visible w vis =
  if vis then begin
    if not w#misc#visible then w#misc#show ()
  end else begin
    if w#misc#visible then w#misc#hide ()
  end

(****)

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
  | Text (x, y, txt, font, font_size, fill, stroke) ->
      Cairo.select_font_face ctx font
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size ctx font_size;
      let ext = Cairo.text_extents ctx txt in
      (x -. ext.Cairo.text_width /. 2. -. 5.,
       y +. ext.Cairo.y_bearing -. 5.,
       x +. ext.Cairo.text_width /. 2. +. 5.,
       y +. ext.Cairo.y_bearing +. ext.Cairo.text_height +. 5.)

(****)

module Common = Viewer_common.F (struct
  type ctx = Cairo.t
  type color = float * float * float

  let save = Cairo.save
  let restore = Cairo.restore

  let scale = Cairo.scale
  let translate = Cairo.translate

  let begin_path = Cairo.new_path
  let close_path = Cairo.close_path
  let move_to = Cairo.move_to
  let line_to = Cairo.line_to
  let curve_to = Cairo.curve_to
  let arc = Cairo.arc
  let rectangle = Cairo.rectangle

  let fill ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b; Cairo.fill_preserve ctx
  let stroke ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b; Cairo.stroke_preserve ctx
  let clip = Cairo.clip

  type window = < misc : GDraw.misc_ops >
  type drawable = GDraw.drawable
  type pixmap = GDraw.pixmap
  let get_drawable w = new GDraw.drawable (w#misc#window)
  let make_pixmap window width height =
    GDraw.pixmap ~width ~height ~window ()
  let drawable_of_pixmap p = (p : GDraw.pixmap :> GDraw.drawable)
  let get_context p = Cairo_lablgtk.create p#pixmap
  let put_pixmap ~(dst : GDraw.drawable) ~x ~y ~xsrc ~ysrc ~width ~height p =
    dst#put_pixmap ~x ~y ~xsrc ~ysrc ~width ~height p#pixmap;

  (****)

  type rectangle = Gtk.rectangle = {x : int; y : int; width : int; height: int}
  let compute_extent = compute_extent
end)

type rectangle = Gtk.rectangle = {x : int; y : int; width : int; height: int}
let empty_rectangle = {x = 0; y = 0; width = 0; height = 0}
let rectangle_is_empty r = r.width = 0 || r.height = 0

type st =
  { mutable bboxes : (float * float * float * float) array;
    scene : Scene.color Scene.element array;
    mutable zoom_factor : float;
    st_x : float; st_y : float; st_width : float; st_height : float }

(****)

let perform_draw ctx fill stroke =
  begin match fill with
    Some (r, g, b) ->
      Cairo.set_source_rgb ctx r g b;
      if stroke <> None then Cairo.fill_preserve ctx
      else Cairo.fill ctx
  | None ->
      ()
  end;
  begin match stroke with
    Some (r, g, b) ->
      Cairo.set_source_rgb ctx r g b;
      Cairo.stroke ctx
  | None ->
      ()
  end

let draw_element ctx e =
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
      perform_draw ctx fill stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Cairo.save ctx;
      Cairo.translate ctx cx cy;
      Cairo.scale ctx rx ry;
      Cairo.arc ctx 0. 0. 1. 0. (2. *. pi);
      Cairo.restore ctx;
      perform_draw ctx fill stroke
  | Polygon (points, fill, stroke) ->
      Array.iteri
        (fun i (x, y) ->
           if i = 0 then Cairo.move_to ctx x y else Cairo.line_to ctx x y)
        points;
      Cairo.close_path ctx;
      perform_draw ctx fill stroke
  | Text (x, y, txt, font, font_size, fill, stroke) ->
      Cairo.select_font_face ctx font
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size ctx font_size;
      let ext = Cairo.text_extents ctx txt in
      Cairo.move_to ctx
        (x -. ext.Cairo.x_bearing -. ext.Cairo.text_width /. 2.) y;
      Cairo.show_text ctx txt;
      perform_draw ctx fill stroke

let intersects (x1, y1, x2, y2) (x3, y3, x4, y4) =
  x1 <= x4 && y1 <= y4 && x3 <= x2 && y3 <= y2

let compute_scale st range =
  st.zoom_factor ** range#adjustment#value /. st.zoom_factor

type pixmap =
  { mutable pixmap : GDraw.pixmap;
    mutable p_width : int; mutable p_height : int;
    mutable valid_rect : rectangle }

let make_pixmap window width height =
  { pixmap = GDraw.pixmap ~width ~height ~window ();
    p_width = width; p_height = height;
    valid_rect = empty_rectangle }

let grow_pixmap pm window width height =
  let width = max width pm.p_width in
  let height = max height pm.p_height in
  if width > pm.p_width || height > pm.p_height then begin
    let old_pm = pm.pixmap in
    pm.pixmap <- GDraw.pixmap ~width ~height ~window ();
    let r = pm.valid_rect in
    pm.pixmap#put_pixmap
      ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:r.width ~height:r.height
      old_pm#pixmap;
    pm.p_width <- width;
    pm.p_height <- height
  end

let redraw st pm scale x y x' y' w h =
Format.eprintf "REDRAW %d %d %d %d@." x' y' w h;
  let ctx = Cairo_lablgtk.create pm.pixmap#pixmap in
  Cairo.save ctx;
  if Array.length st.bboxes = 0 && Array.length st.scene > 0 then
    st.bboxes <- Array.map (fun e -> compute_extent ctx e) st.scene;
  Cairo.new_path ctx;
  Cairo.rectangle ctx (float x') (float y') (float w) (float h);
(*
  Cairo_lablgtk.region ctx (GdkEvent.Expose.region ev);
  let rect = Gdk.Rectangle.create 0 0 0 0 in
  Gdk.Region.get_clipbox (GdkEvent.Expose.region ev) rect;
  *)
  let x = float x /. scale in
  let y = float y /. scale in
  Cairo.clip ctx;
  Cairo.set_source_rgb ctx 1. 1. 1.;
  Cairo.paint ctx;
  Cairo.scale ctx scale scale;
  Cairo.translate ctx (-. st.st_x -. x) (-. st.st_y -. y);
  let bbox =
    let x = st.st_x +. x +. float x' /. scale in
    let y = st.st_y +. y +. float y' /. scale in
    (x, y,
     x +. float pm.p_width /. scale,
     y +. float pm.p_height /. scale)
  in
  for i = 0 to Array.length st.scene - 1 do
    let box = st.bboxes.(i) in
    let e = st.scene.(i) in
    if intersects box bbox then draw_element ctx e
  done;
  Cairo.restore ctx

let scroll_view ?width ?height ?packing st =
  let table = GPack.table ?width ?height ~columns:2 ~rows:2 ?packing () in
(*  table#misc#set_can_focus true;*)
  let hadj = GData.adjustment () in
  let hbar =
    GRange.scrollbar `HORIZONTAL ~adjustment:hadj
      ~packing:(table#attach ~left:0 ~top:1 ~fill:`BOTH ~expand:`NONE) ()
  in
  hbar#misc#hide ();
  let vadj = GData.adjustment () in
  let vbar =
    GRange.scrollbar `VERTICAL ~adjustment:vadj
      ~packing:(table#attach ~left:1 ~top:0 ~fill:`BOTH ~expand:`NONE) ()
  in
  vbar#misc#hide ();
  let display =
    GMisc.drawing_area
      ~packing:(table#attach ~left:0 ~top:0 ~fill:`BOTH ~expand:`BOTH) ()
  in
  display#misc#set_can_focus true;

  let sadj =
    GData.adjustment ~upper:20. ~step_incr:1. ~page_incr:0. ~page_size:0. () in
  let zoom_steps = 8. in (* Number of steps to get a factor of 2 *)
  let set_zoom_factor f =
    let count = ceil (log f /. log 2. *. zoom_steps) in
    let f = 2. ** (count /. zoom_steps) in
    sadj#set_bounds ~upper:count ();
Format.eprintf "Factor: %f@." f;
    st.zoom_factor <- f
  in

  let pm = make_pixmap display 10 10 in

  display#misc#set_double_buffered false;

  let get_scale () =
    let r = 2. ** (sadj#value /. zoom_steps) /. st.zoom_factor in
Format.eprintf "%f --> %f@." sadj#value r;
r
 in

  let update_scrollbars () =
    let a = display#misc#allocation in
    let scale = get_scale () in
    let aw = ceil (float a.width /. scale) in
    let ah = ceil (float a.height /. scale) in
    hadj#set_bounds ~step_incr:(aw /. 20.) ~page_incr:(aw /. 2.)
      ~page_size:(min aw st.st_width) ~upper:st.st_width ();
    let mv = st.st_width -. hadj#page_size in
    if hadj#value > mv then hadj#set_value mv;
    vadj#set_bounds ~step_incr:(ah /. 20.) ~page_incr:(ah /. 2.)
      ~page_size:(min ah st.st_height) ~upper:st.st_height ();
    let mv = st.st_height -. vadj#page_size in
    if vadj#value > mv then vadj#set_value mv;
    set_visible hbar (aw < st.st_width);
    set_visible vbar (ah < st.st_height)
  in

  ignore (display#event#connect#configure
    (fun ev ->
prerr_endline "CONFIGURE";
       update_scrollbars (); false));
  ignore (display#event#connect#map
    (fun ev ->
       let a = display#misc#allocation in
Format.eprintf "alloc: %d %d@." a.width a.height;
       let zoom_factor =
         max (st.st_width /. float a.width) (st.st_height /. float a.height) in
       set_zoom_factor zoom_factor;
       update_scrollbars (); false));
  display#event#add [`STRUCTURE];

  ignore (display#event#connect#expose
    (fun ev ->
       let scale = get_scale () in
       let a = display#misc#allocation in
       grow_pixmap pm display a.width a.height;
       let round x = truncate (x *. scale +. 0.5) in
       let x0 = round (hadj#value) in
       let x0' = round ((float a.width /. scale -. hadj#upper) /. 2.) in
       let x0 = if x0' > 0 then - x0' else x0 in
       let y0 = round (vadj#value) in
       let y0' = round ((float a.height /. scale -. vadj#upper) /. 2.) in
       let y0 = if y0' > 0 then - y0' else y0 in

Format.eprintf "%d %d@." x0 x0';
Format.eprintf "%f %f %f %f %d@." hadj#value hadj#upper hadj#page_size ((hadj#upper *. scale -. float a.width) /. 2.) x0;
       let dx = pm.valid_rect.x - x0  in
       let dy = pm.valid_rect.y - y0  in
       if
         (dx > 0 && pm.valid_rect.width + dx < a.width) ||
         (dy > 0 && pm.valid_rect.height + dy < a.height)
       then begin
Format.eprintf "Invalidate (%d %d) %d %d %d %d@." dx dy (pm.valid_rect.width + dx) a.width (pm.valid_rect.height + dy) a.height;
         pm.valid_rect <- empty_rectangle
       end else if not (rectangle_is_empty pm.valid_rect) then begin
(*XXX FIX: should redraw up to four rectangles here *)
         let p = pm.pixmap in
         let r = pm.valid_rect in
Format.eprintf "Translation: %d %d@." dx dy;
         if (dx <> 0 || dy <> 0) then
           p#put_pixmap
             ~x:dx ~y:dy
             ~xsrc:0 ~ysrc:0 ~width:r.width ~height:r.height p#pixmap;
         let offset p l d m = (* 0 <= p; 0 <= l; p + l <= m *)
           if p + d + l <= 0 then
             (0, 0)
           else if p + d < 0 then
             (0, l + p + d)
           else if p + d >= m then
             (m, 0)
           else if p + d + l > m then
             (p + d, m - p - d)
           else
             (p + d, l)
         in
         let (x, width) = offset 0 r.width dx pm.p_width in
         let (y, height) = offset 0 r.height dy pm.p_height in
         if height > 0 then begin
           if x > 0 then begin
             assert (x + width >= a.width);
             redraw st pm scale x0 y0 0 y x height
           end else begin
             assert (x = 0);
             if a.width > width then
               redraw st pm scale x0 y0 width y (a.width - width) height
           end
         end;
         if y > 0 then begin
           assert (y + height >= a.height);
           redraw st pm scale x0 y0 0 0 a.width y;
         end else begin
           assert (y = 0);
           if a.height > height then
             redraw st pm scale x0 y0 0 height a.width (a.height - height)
         end;
         pm.valid_rect <- { x = x0; y = y0; width = a.width; height = a.height }
       end;
       let area = GdkEvent.Expose.area ev in
       let x = Gdk.Rectangle.x area in
       let y = Gdk.Rectangle.y area in
       let width = Gdk.Rectangle.width area in
       let height = Gdk.Rectangle.height area in
       let r = pm.valid_rect in
       if
         x < 0 || y < 0 ||
         x + width > r.width || y + height > r.height
       then begin
         redraw st pm scale x0 y0 0 0 a.width a.height;
         pm.valid_rect <- {x = x0; y = y0; width = a.width; height = a.height };
       end;
       let gwin = display#misc#window in
       let d = new GDraw.drawable gwin in
       d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height pm.pixmap#pixmap;
       true));

  let refresh () =
    pm.valid_rect <- empty_rectangle;
    GtkBase.Widget.queue_draw display#as_widget
  in
  ignore (hadj#connect#value_changed
    (fun () -> GtkBase.Widget.queue_draw display#as_widget));
  ignore (vadj#connect#value_changed
    (fun () -> GtkBase.Widget.queue_draw display#as_widget));
  let prev_scale = ref (get_scale ()) in
  let zoom_center = ref (0.5, 0.5) in
  ignore (sadj#connect#value_changed
    (fun () ->
       let scale = get_scale () in
       let r = (1. -. !prev_scale /. scale) in
Format.eprintf "update@.";
       hadj#set_value (hadj#value +. hadj#page_size *. r *. fst !zoom_center);
       vadj#set_value (vadj#value +. vadj#page_size *. r *. snd !zoom_center);
       prev_scale := scale;
       refresh ();
       update_scrollbars ()));

  let bump_scale x y v =
    let a = display#misc#allocation in
    let x = x /. float a.width in
    let y = y /. float a.height in
    if x >= 0. && x <= 1. && y >= 0. && y <= 1. then
      zoom_center := (x, y);
Format.eprintf "loc: %f %f@." x y;
    sadj#set_value (sadj#value +. v *. sadj#step_increment);
Format.eprintf "reset@.";
    zoom_center := (0.5, 0.5);
    true
  in
  (* Zoom using the mouse wheel *)
  ignore (display#event#connect#scroll
    (fun ev ->
       let x = GdkEvent.Scroll.x ev in
       let y = GdkEvent.Scroll.y ev in
       match GdkEvent.Scroll.direction ev with
         `UP   -> bump_scale x y 1.
       | `DOWN -> bump_scale x y (-1.)
       | _     -> false));
  display#event#add [`SCROLL];

  let pos = ref None in
  ignore (display#event#connect#button_press
    (fun ev ->
       display#misc#grab_focus ();
       if
         GdkEvent.get_type ev = `BUTTON_PRESS && GdkEvent.Button.button ev = 1
       then begin
         pos := Some (GdkEvent.Button.x ev, GdkEvent.Button.y ev);
       end;
       false));
  ignore (display#event#connect#button_release
    (fun ev ->
       if GdkEvent.Button.button ev = 1 then begin
         pos := None;
       end;
       false));
  ignore (display#event#connect#motion_notify
    (fun ev ->
       begin match !pos with
         Some (x, y) ->
           let (x', y') =
             if GdkEvent.Motion.is_hint ev then
               let (x', y') = display#misc#pointer in
               (float x', float y')
             else
               (GdkEvent.Motion.x ev, GdkEvent.Motion.y ev)
           in
           let offset a d =
             a#set_value (min (a#value +. d) (a#upper -. a#page_size)) in
           let scale = get_scale () in
           offset (hadj) ((x -. x') /. scale);
           offset (vadj) ((y -. y') /. scale);
           pos := Some (x', y')
       | None ->
           ()
       end;
       false));
  display#event#add
    [`BUTTON_PRESS; `BUTTON_RELEASE; `BUTTON1_MOTION; `POINTER_MOTION_HINT];

  ignore (display#event#connect#key_press
    (fun ev ->
       let keyval = GdkEvent.Key.keyval ev in
       if
         keyval = GdkKeysyms._Up
       then begin
         vadj#set_value (vadj#value -. vadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Down
       then begin
         vadj#set_value (vadj#value +. vadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Left
       then begin
         hadj#set_value (hadj#value -. hadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Right
       then begin
         hadj#set_value (hadj#value +. hadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._0 || keyval = GdkKeysyms._agrave
       then begin
         let a = table#misc#allocation in
Format.eprintf "alloc: %d %d@." a.width a.height;
         let zf =
           max (st.st_width /. float a.width) (st.st_height /. float a.height)
         in
         let v = ceil (log zf /. log 2. *. zoom_steps) in
Format.eprintf "ZOOM: %f %f %f@." zf v sadj#upper;
         sadj#set_value (min sadj#upper (max 0. (sadj#upper -. v)));
         true
       end else if
         keyval = GdkKeysyms._1 || keyval = GdkKeysyms._ampersand
       then begin
         sadj#set_value (sadj#upper);
         true
       end else if
         keyval = GdkKeysyms._plus ||
         keyval = GdkKeysyms._equal ||
         keyval = GdkKeysyms._KP_Add
       then begin
         let (x, y) = display#misc#pointer in
         bump_scale (float x) (float y) 1.
       end else if
         keyval = GdkKeysyms._minus ||
         keyval = GdkKeysyms._KP_Subtract
       then begin
         let (x, y) = display#misc#pointer in
         bump_scale (float x) (float y) (-1.)
       end else
      (*XXXX key bindings for zooming, ... *)
         false));
  display#event#add [`KEY_PRESS];

  object
    method display = display
    method scale_adjustment = sadj
    method scale = get_scale ()
    method offset =
Format.eprintf "%f %f %f %f@." hadj#value hadj#page_size (hadj#value +. hadj#page_size) hadj#upper;
(*XXX Center... *)
 (hadj#value, vadj#value)
  end

let create ?(full_screen=false) (x1, y1, x2, y2) scene =
  let st =
    { bboxes = [||]; scene = Scene.get scene; zoom_factor = 20.;
      st_x = x1; st_y = y1; st_width = x2 -. x1; st_height = y2 -. y1 } in

  let initial_size = 600 in
  let w = GWindow.window () in
  ignore (w#connect#destroy GMain.quit);
  let b = GPack.hbox (*~spacing:6*)
      ~packing:w#add () in
  let f =
    scroll_view ~width:initial_size ~height:initial_size
      ~packing:(b#pack ~expand:true) st in
  ignore
    (GRange.scale `VERTICAL ~inverted:true
       ~draw_value:false ~adjustment:(f#scale_adjustment)
       ~packing:b#pack ());

(*XXX Tooltips
area#misc#set_has_tooltip true;
ignore (area#misc#connect#query_tooltip (fun ~x ~y ~kbd tooltip ->
Format.eprintf "%d %d %b@." x y kbd; false));
*)

  (* Full screen mode *)
  let fullscreen = ref false in
  let toggle_fullscreen () =
    if !fullscreen then w#unfullscreen () else w#fullscreen ();
    fullscreen := not !fullscreen;
    true
  in
  if full_screen then ignore (toggle_fullscreen ());

  ignore (w#event#connect#key_press
    (fun ev ->
       let keyval = GdkEvent.Key.keyval ev in
Format.eprintf "%d@." keyval;
       if keyval = GdkKeysyms._q || keyval = GdkKeysyms._Q then
         exit 0
       else if
         keyval = GdkKeysyms._F11 ||
         keyval = GdkKeysyms._F5 ||
         (keyval = GdkKeysyms._Escape && !fullscreen) ||
         keyval = GdkKeysyms._f || keyval = GdkKeysyms._F
       then
         toggle_fullscreen ()
       else
         false));

  w#show ()

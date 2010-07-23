(*
- Zoom : widget + mouse wheel
*)

type rect = {x : int; y : int; width : int; height: int}

module Html = Dom_html

let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##width <- w; c##height <- h; c

module Common = Viewer_common.F (struct
  type font = Js.js_string Js.t
  type color = Js.js_string Js.t
  type text = Js.js_string Js.t
  let white = Js.string "white"

  type ctx = Html.canvasRenderingContext2D Js.t

  let save ctx = ctx##save ()
  let restore ctx = ctx##restore ()

  let scale ctx ~sx ~sy = ctx##scale (sx, sy)
  let translate ctx ~tx ~ty = ctx##translate (tx, ty)

  let begin_path ctx = ctx##beginPath ()
  let close_path ctx = ctx##closePath ()
  let move_to ctx ~x ~y = ctx##moveTo (x, y)
  let line_to ctx ~x ~y = ctx##lineTo (x, y)
  let curve_to ctx ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    ctx##bezierCurveTo (x1, y1, x2, y2, x3, y3)
  let arc ctx ~xc ~yc ~radius ~angle1 ~angle2 =
    ctx##arc (xc, yc, radius, angle1, angle2, Js._true)
  let rectangle ctx ~x ~y ~width ~height = ctx##rect (x, y, width, height)

  let fill ctx c = ctx##fillStyle <- c; ctx##fill ()
  let stroke ctx c = ctx##strokeStyle <- c; ctx##stroke ()
  let clip ctx = ctx##clip ()

  let draw_text (ctx:ctx) x y txt font fill_color stroke_color =
     ctx##font <- font;
     ctx##textAlign <- Js.string "center";
     begin match fill_color with
       Some c -> ctx##fillStyle <- c; ctx##fillText (txt, x, y, Js.null)
     | None   -> ()
     end;
     begin match stroke_color with
       Some c -> ctx##strokeStyle <- c; ctx##strokeText (txt, x, y, Js.null)
     | None   -> ()
     end

  type window = Html.canvasElement Js.t
  type drawable = window * ctx
  type pixmap = drawable
  let get_drawable w =
    let ctx = w##getContext(Html._2d_) in
    ctx##lineWidth <- 2.;
    (w, ctx)
  let make_pixmap _ width height =
    let c = Html.createCanvas Html.document in
    c##width <- width; c##height <- height;
    get_drawable c
  let drawable_of_pixmap p = p
  let get_context (p, c) = c
  let put_pixmap ~dst:((p, c) :drawable) ~x ~y ~xsrc ~ysrc ~width ~height ((p, _) : pixmap)=
    c##drawImage_fullFromCanvas (p, float xsrc, float ysrc, float width, float height, float x, float y, float width, float height)

  (****)

  type rectangle = rect = {x : int; y : int; width : int; height: int}
  let compute_extents _ = assert false
end)
open Common

let redraw st s h v (canvas : Html.canvasElement Js.t) =
  let width = canvas##width in
  let height = canvas##height in
Firebug.console##log_2 (h, v);
  redraw st s h v canvas
    {x = 0; y = 0; width = width; height = height} 0 0 width height

let (>>=) = Lwt.bind

let http_get url =
  XmlHttpRequest.send url >>= fun {XmlHttpRequest.code = cod; content = msg} ->
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let json : < parse : Js.js_string Js.t -> 'a> Js.t = Js.Unsafe.variable "JSON"

(*
  ?value:float ->
  ?lower:float ->
  ?upper:float ->
  ?step_incr:float ->
  ?page_incr:float -> ?page_size:float -> unit -> adjustment
*)

class adjustment
    ?(value=0.) ?(lower=0.) ?(upper=100.)
    ?(step_incr=1.) ?(page_incr = 10.) ?(page_size = 10.) () =
  object
    val mutable _value = value method value = _value
    val mutable _lower = lower method lower = _lower
    val mutable _upper = upper method upper = _upper
    val mutable _step_incr = step_incr method step_incr = _step_incr
    val mutable _page_incr = page_incr method page_incr = _page_incr
    val mutable _page_size = page_size method page_size = _page_size
    method set_value v = _value <- v
    method set_bounds ?lower ?upper ?step_incr ?page_incr ?page_size () =
      begin match lower with Some v -> _lower <- v | None -> () end;
      begin match upper with Some v -> _upper <- v | None -> () end;
      begin match step_incr with Some v -> _step_incr <- v | None -> () end;
      begin match page_incr with Some v -> _page_incr <- v | None -> () end;
      begin match page_size with Some v -> _page_size <- v | None -> () end
  end

let start _ =
  Html.document##documentElement##style##overflow <- Js.string "hidden";
  Html.document##body##style##overflow <- Js.string "hidden";
  Html.document##body##style##margin <- Js.string "0px";
  let canvas =
    create_canvas  (*XXX FIX inner#Width,Height*)
      ((Js.Unsafe.coerce Html.window)##innerWidth)
      ((Js.Unsafe.coerce Html.window)##innerHeight) in
  Dom.appendChild Html.document##body canvas;
  http_get "scene.json" >>= fun s ->
  let ((x1, y1, x2, y2), bboxes, scene) = json##parse (Js.string s) in
  let st =
    { bboxes = bboxes(*[|(0., 0., 600., 600.)|]*);
      scene = scene (*[|Scene.Ellipse (300., 300., 250., 250., Some (Js.string "red"), Some (Js.string "blue"))|]*);
      zoom_factor = 1. /. 20.;
      st_x = x1; st_y = y1; st_width = x2 -. x1; st_height = y2 -. y1;
      st_pixmap = Common.make_pixmap () }
  in
  let hadj = new adjustment () in
  let vadj = new adjustment () in

  let get_scale () = 0.5 in
  let allocation () =
    {x = 0; y = 0; width = canvas##width; height = canvas##height} in

  let update_view () =
Firebug.console##log("update");
    let a = allocation () in
    let scale = get_scale () in
    let aw = ceil (float a.width /. scale) in
    let ah = ceil (float a.height /. scale) in
    hadj#set_bounds ~step_incr:(aw /. 20.) ~page_incr:(aw /. 2.)
      ~page_size:(min aw st.st_width) ~upper:st.st_width ();
    let mv = st.st_width -. hadj#page_size in
    if hadj#value < 0. then hadj#set_value 0.;
    if hadj#value > mv then hadj#set_value mv;
    vadj#set_bounds ~step_incr:(ah /. 20.) ~page_incr:(ah /. 2.)
      ~page_size:(min ah st.st_height) ~upper:st.st_height ();
    let mv = st.st_height -. vadj#page_size in
    if vadj#value < 0. then vadj#set_value 0.;
    if vadj#value > mv then vadj#set_value mv;
Firebug.console##log_3(get_scale (), hadj#value, vadj#value);
    redraw st (get_scale ()) hadj#value vadj#value canvas;
  in
  update_view ();

  Html.window##onresize <- Html.handler
    (fun _ ->
       canvas##width <- (Js.Unsafe.coerce Html.window)##innerWidth;
       canvas##height <- (Js.Unsafe.coerce Html.window)##innerHeight;
       update_view ();
       Js._false);

  let mx = ref 0 in
  let my = ref 0 in
  canvas##onmousedown <- Html.handler
    (fun ev ->
       mx := ev##clientX; my := ev##clientY;
       let old_cursor = canvas##style##cursor in
       canvas##style##cursor <- Js.string "move";
       let c1 =
         Html.addEventListener Html.document Html.Event.mousemove
           (Html.handler
              (fun ev ->
                 let x = ev##clientX and y = ev##clientY in
                 let offset a d =
                   a#set_value (min (a#value +. d) (a#upper -. a#page_size)) in
                 let scale = get_scale () in
                 offset (hadj) (float (!mx - x) /. scale);
                 offset (vadj) (float (!my - y) /. scale);
                 mx := x; my := y;
                 update_view ();
                 Js._true))
           Js._true
       in
       let c2 = ref Js.null in
       c2 := Js.some
         (Html.addEventListener Html.document Html.Event.mouseup
            (Html.handler
               (fun _ ->
                  Html.removeEventListener c1;
                  Js.Opt.iter !c2 Html.removeEventListener;
                  canvas##style##cursor <- old_cursor;
                  Js._true))
            Js._true);
       Js._false);


  Lwt.return ()

let _ =
Html.window##onload <- Html.handler (fun _ -> ignore (start ()); Js._false)

(**********************)

(*
I believe I have found an elegant solution to this:

JavaScript

/* important! for alignment, you should make things
 * relative to the canvas' current width/height.
 */
function draw() {
  var ctx = (a canvas context);
  ctx.canvas.width  = window.innerWidth;
  ctx.canvas.height = window.innerHeight;
  //...drawing code...
}

CSS

html, body {
  width:  100%;
  height: 100%;
  margin: 0px;
}

Hasn't had any large negative performance impact for me, so far.
*)

(*
<script>
window.onload = window.onresize = function() {
    var C = 0.8;        // canvas width to viewport width ratio
    var W_TO_H = 2/1;   // canvas width to canvas height ratio
    var el = document.getElementById("a");

    // For IE compatibility http://www.google.com/search?q=get+viewport+size+js
    var viewportWidth = window.innerWidth;
    var viewportHeight = window.innerHeight;

    var canvasWidth = viewportWidth * C;
    var canvasHeight = canvasWidth / W_TO_H;
    el.style.position = "fixed";
    el.setAttribute("width", canvasWidth);
    el.setAttribute("height", canvasHeight);
    el.style.top = (viewportHeight - canvasHeight) / 2;
    el.style.left = (viewportWidth - canvasWidth) / 2;

    window.ctx = el.getContext("2d");
    ctx.clearRect(0,0,canvasWidth,canvasHeight);
    ctx.fillStyle = 'yellow';
    ctx.moveTo(0, canvasHeight/2);
    ctx.lineTo(canvasWidth/2, 0);
    ctx.lineTo(canvasWidth, canvasHeight/2);
    ctx.lineTo(canvasWidth/2, canvasHeight);
    ctx.lineTo(0, canvasHeight/2);
    ctx.fill()
}
</script>

<body>
<canvas id="a" style="background: black">
</canvas>
</body>
*)


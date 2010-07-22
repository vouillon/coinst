

module F (M : sig
  type ctx
  type color

  val save : ctx -> unit
  val restore : ctx -> unit

  val scale : ctx -> sx:float -> sy:float -> unit
  val translate : ctx -> tx:float -> ty:float -> unit

  val begin_path : ctx -> unit
  val close_path : ctx -> unit
  val move_to : ctx -> x:float -> y:float -> unit
  val line_to : ctx -> x:float -> y:float -> unit
  val curve_to :
    ctx ->
    x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float ->
    unit
  val arc :
    ctx ->
    xc:float -> yc:float -> radius:float -> angle1:float -> angle2:float ->
    unit
  val rectangle :
    ctx -> x:float -> y:float -> width:float -> height:float -> unit

  val fill : ctx -> color -> unit
  val stroke : ctx -> color -> unit
  val clip : ctx -> unit

  type window
  type drawable
  type pixmap
  val get_drawable : window -> drawable
  val make_pixmap : window -> int -> int -> pixmap
  val drawable_of_pixmap : pixmap -> drawable
  val get_context : pixmap -> ctx
  val put_pixmap :
    dst:drawable ->
    x:int -> y:int -> xsrc:int -> ysrc:int -> width:int -> height:int ->
    pixmap -> unit

  (****)

  type rectangle = {x : int; y : int; width : int; height: int}

  val compute_extent :
    ctx -> color Scene.element -> float * float * float * float
end) : sig

  type pixmap

  val make_pixmap : unit -> pixmap

  type st =
    { mutable bboxes : (float * float * float * float) array;
      scene : M.color Scene.element array;
      mutable zoom_factor : float;
      st_x : float; st_y : float; st_width : float; st_height : float;
      st_pixmap : pixmap }

  val redraw :
    st -> float -> float -> float ->
    M.window -> M.rectangle -> int -> int -> int -> int -> unit

end

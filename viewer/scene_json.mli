
val rect_stringify : Format.formatter -> float * float * float * float -> unit

val rect_array_stringify :
  Format.formatter -> (float * float * float * float) array -> unit

val stringify :
  Format.formatter ->
  (float * float * float, string * float, string) Scene.element array ->
  unit

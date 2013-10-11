
val read_data : string -> Deb_lib.pool

val f :
  Upgrade_common.ignored_sets -> ?popcon_file:string ->
  Deb_lib.pool -> Deb_lib.pool -> string -> unit


type t

val compute :
  Upgrade_common.ignored_sets -> ?popcon_file:string ->
  Deb_lib.pool -> Deb_lib.pool ->
  (Upgrade_common.Repository.Package.t ->
   Layout.outside_anchor Layout.phrasing Layout.t) ->
  t

val f :
  Upgrade_common.ignored_sets -> ?popcon_file:string ->
  Deb_lib.pool -> Deb_lib.pool -> string -> unit

val read_data : string -> Deb_lib.pool

val explanations : t -> _ Layout.flow Layout.t

val has_issues : t -> bool

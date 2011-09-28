
val filter : in_channel -> in_channel
val open_in : string -> in_channel
val open_in_multiple : string list -> in_channel

val has_magic : in_channel -> string -> bool


type (+'a) t

val (&) : 'a t -> 'a t -> 'a t
val emp : 'a t

(****)

type +'a flow
type +'a phras
type 'a phrasing = 'a phras flow

val s : string -> _ phrasing t
val i : int -> _ phrasing t
val format : (Format.formatter -> 'a -> unit) -> 'a -> _ phrasing t
val seq : string -> ('a -> _ phrasing t) -> 'a list -> _ phrasing t
val seq2 : string -> string -> ('a -> _ phrasing t) -> 'a list -> _ phrasing t
val code : _ phrasing t -> _ phrasing t

val raw_html : (unit -> string) -> _ phrasing t

type in_anchor
type outside_anchor
val anchor : string -> in_anchor phrasing t -> outside_anchor phrasing t

val p : _ flow t

val div : ?clss:string -> _ flow t -> _ flow t
val span : ?clss:string -> _ phrasing t -> _ flow t

val pre : ?clss:string -> _ phrasing t -> _ flow t

val heading : _ phrasing t -> _ flow t

val section : _ flow t -> _ flow t
val footer : _ flow t -> _ flow t

(****)

type +'a lst
val list : ('a -> 'b t) -> 'a list -> 'b t

type u
val ul : ?prefix:string -> u lst t -> _ flow t
val li : _ flow t -> u lst t

type d
val dl : ?clss:string -> d lst t -> _ flow t
val dt : ?clss:string -> _ phrasing t -> d lst t
val dd : _ flow t -> d lst t
val dli : ?id:string -> _ phrasing t -> _ flow t -> d lst t

(****)

class type printer = object
  method start_doc : unit -> unit
  method end_doc : unit -> unit
  method text : string -> unit
  method change_p : unit -> unit
  method start_code : unit -> unit
  method end_code : unit -> unit
  method start_ul : string -> unit
  method li : unit -> unit
  method end_ul : unit -> unit
  method start_a : string -> unit
  method end_a : unit -> unit
  method start_dl : ?clss:string -> unit -> unit
  method dt : ?clss:string -> string option -> unit
  method dd : unit -> unit
  method end_dl : unit -> unit
  method start_div : ?clss:string -> unit -> unit
  method end_div : unit -> unit
  method start_span : ?clss:string -> unit -> unit
  method end_span : unit -> unit
  method start_pre : ?clss:string -> unit -> unit
  method end_pre : unit -> unit
  method start_heading : unit -> unit
  method end_heading : unit -> unit
  method start_section : unit -> unit
  method end_section : unit -> unit
  method start_footer : unit -> unit
  method end_footer : unit -> unit
  method raw_html : (unit -> string) -> unit
end

class html_printer :
  out_channel -> ?stylesheet:string -> ?style:string ->
  ?scripts:(string list) -> string -> printer
class format_printer : Format.formatter -> printer

val print : printer -> _ flow t -> unit


class type printer = object
  method start_doc : unit -> unit
  method end_doc : unit -> unit
  method text : string -> unit
  method start_code : unit -> unit
  method end_code : unit -> unit
  method change_p : unit -> unit
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
  method start_section : ?clss:string -> unit -> unit
  method end_section : unit -> unit
  method start_footer : unit -> unit
  method end_footer : unit -> unit
  method raw_html : (unit -> string) -> unit
end

type +'a t = printer -> unit

let (&) f1 f2 p = f1 p; f2 p
let emp p = ()

(****)

type +'a flow
type +'a phras
type 'a phrasing = 'a phras flow

let s s p = p#text s
let i i p = p#text (string_of_int i)

let rec seq sep f l p =
  match l with
    []     -> ()
  | [v]    -> f v p
  | v :: r -> f v p; s sep p; seq sep f r p
let rec seq2 sep sep' f l p =
  match l with
    []      -> ()
  | [v]     -> f v p
  | [v; v'] -> f v p; s sep' p; f v' p
  | v :: r  -> f v p; s sep p; seq2 sep sep' f r p

let buf = Buffer.create 16
let formatter = Format.formatter_of_buffer buf
let format f v p =
  Buffer.clear buf; f formatter v; Format.pp_print_flush formatter ();
  p#text (Buffer.contents buf)

let code contents p = p#start_code (); contents p; p#end_code ()

type in_anchor
type outside_anchor

let anchor link contents p = p#start_a link; contents p; p#end_a ()

let p pr = pr#change_p ()

let div ?clss contents p = p#start_div ?clss (); contents p; p#end_div ()

let span ?clss contents p = p#start_span ?clss (); contents p; p#end_span ()

let pre ?clss contents p = p#start_pre ?clss (); contents p; p#end_pre ()

let heading contents p = p#start_heading (); contents p; p#end_heading ()

let section ?clss contents p =
  p#start_section ?clss (); contents p; p#end_section ()

let footer contents p = p#start_footer (); contents p; p#end_footer ()

let raw_html f pr = pr#raw_html f

(****)

type +'a lst
let rec list f l p = List.iter (fun v -> f v p) l

type u
let ul ?(prefix="* ") lst p = p#start_ul prefix; lst p; p#end_ul ()
let li contents p = p#li (); contents p

type d
let dl ?clss lst p = p#start_dl ?clss (); lst p; p#end_dl ()
let dli ?id key desc (p : #printer) = p#dt id; key p; p#dd (); desc p
let dt ?clss key p = p#dt ?clss None; key p
let dd desc p = p#dd (); desc p

(****)

let print p doc = p#start_doc (); doc p; p#end_doc ()

let html_escape s =
  let s = Bytes.of_string s in
  let l = Bytes.length s in
  let n = ref 0 in
  for i = 0 to l - 1 do
    match Bytes.unsafe_get s i with
      '<' | '>' -> n := !n + 3
    | '&'       -> n := !n + 4
    | '\''      -> n := !n + 5
    | _         -> ()
  done;
  if !n = 0 then Bytes.to_string s else
  let s' = Bytes.create (l + !n) in
  n := 0;
  for i = 0 to l - 1 do
    match Bytes.unsafe_get s i with
      '<' ->
        Bytes.unsafe_set s' !n '&'; incr n;
        Bytes.unsafe_set s' !n 'l'; incr n;
        Bytes.unsafe_set s' !n 't'; incr n;
        Bytes.unsafe_set s' !n ';'; incr n
    | '>' ->
        Bytes.unsafe_set s' !n '&'; incr n;
        Bytes.unsafe_set s' !n 'g'; incr n;
        Bytes.unsafe_set s' !n 't'; incr n;
        Bytes.unsafe_set s' !n ';'; incr n
    | '&' ->
        Bytes.unsafe_set s' !n '&'; incr n;
        Bytes.unsafe_set s' !n 'a'; incr n;
        Bytes.unsafe_set s' !n 'm'; incr n;
        Bytes.unsafe_set s' !n 'p'; incr n;
        Bytes.unsafe_set s' !n ';'; incr n
    | '\'' ->
        Bytes.unsafe_set s' !n '&'; incr n;
        Bytes.unsafe_set s' !n 'a'; incr n;
        Bytes.unsafe_set s' !n 'p'; incr n;
        Bytes.unsafe_set s' !n 'o'; incr n;
        Bytes.unsafe_set s' !n 's'; incr n;
        Bytes.unsafe_set s' !n ';'; incr n
    | c ->
        Bytes.unsafe_set s' !n c; incr n
  done;
  Bytes.to_string s'

class html_printer ch ?stylesheet ?(style="") ?(scripts=[]) title : printer =
object (self)
  val mutable in_p = false
  val mutable need_break = false
  val mutable at_list_start = None
  method private break () = if need_break then output_char ch '\n'
  method start_doc () =
    output_string ch
      "<!DOCTYPE html>\n<meta charset='utf-8'>\n<title>";
    output_string ch (html_escape title);
    output_string ch "</title>\n";
    begin match stylesheet with
      Some url ->
        output_string ch "<link rel='stylesheet' href='";
        output_string ch (html_escape url);
        output_string ch "'>\n";
    | None ->
        ()
    end;
    if style <> "" then begin
      output_string ch "<style type='text/css'>\n";
      output_string ch (html_escape style);
      output_string ch "</style>\n"
    end;
    List.iter
      (fun url ->
         output_string ch "<script src='";
         output_string ch (html_escape url);
         output_string ch "'></script>\n")
      scripts
  method end_doc () = ()
  method text s =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    output_string ch (html_escape s); need_break <- true
  method change_p () = in_p <- false
  method start_ul _ = at_list_start <- Some ""
  method li () =
    begin match at_list_start with
      Some clss ->
        self#break (); output_string ch "<ul";
        if clss <> "" then
          output_string ch (" class='" ^ html_escape clss ^ "'");
        output_string ch ">";
        at_list_start <- None
    | None ->
        ()
    end;
    self#break (); output_string ch "<li>"; in_p <- false
  method end_ul () =
    if at_list_start = None then begin
      self#break (); output_string ch "</ul>";
      self#break (); need_break <- false;
      in_p <- false
    end;
    at_list_start <- None
  method start_a l =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    output_string ch ("<a href='" ^ html_escape l ^ "'>")
  method end_a () = output_string ch "</a>"
  method start_code () =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    output_string ch ("<code>")
  method end_code () = output_string ch "</code>"
  method start_dl ?(clss = "") () = at_list_start <- Some clss
  method dt ?clss id =
    begin match at_list_start with
      Some clss ->
        self#break (); output_string ch "<dl";
        if clss <> "" then
          output_string ch (" class='" ^ html_escape clss ^ "'");
        output_string ch ">";
        self#break (); need_break <- false;
        at_list_start <- None
    | None ->
        ()
    end;
    self#break (); output_string ch "<dt";
    begin match clss with
      None      -> ()
    | Some clss -> output_string ch (" class='" ^ clss ^ "'")
    end;
    begin match id with
      None    -> ()
    | Some id -> output_string ch (" id='" ^ id ^ "'")
    end;
    output_string ch ">";
    in_p <- true
  method dd () =
    begin match at_list_start with
      Some clss ->
        self#break (); output_string ch "<dl";
        if clss <> "" then
          output_string ch (" class='" ^ html_escape clss ^ "'");
        output_string ch ">";
        self#break (); need_break <- false;
        at_list_start <- None
    | None ->
        ()
    end;
    self#break (); output_string ch "<dd>"; in_p <- false
  method end_dl () =
    if at_list_start = None then begin
      self#break (); output_string ch "</dl>";
      self#break (); need_break <- false;
      in_p <- false
    end;
    at_list_start <- None
  method start_div ?clss () =
    self#break ();
    begin match clss with
      Some clss -> output_string ch ("<div class='" ^ html_escape clss ^ "'>")
    | None      -> output_string ch "<div>"
    end;
    need_break <- true; in_p <- false
  method end_div () =
    self#break (); output_string ch "</div>"; need_break <- true; in_p <- false
  method start_span ?clss () =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    begin match clss with
      Some clss -> output_string ch ("<span class='" ^ html_escape clss ^ "'>")
    | None      -> output_string ch "<span>"
    end
  method end_span () = output_string ch "</span>";
  method start_pre ?clss () =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    begin match clss with
      Some clss -> output_string ch ("<pre class='" ^ html_escape clss ^ "'>")
    | None      -> output_string ch "<pre>"
    end
  method end_pre () = output_string ch "</pre>";
  method start_heading () =
    self#break ();
    output_string ch "<h1>";
    need_break <- true; in_p <- true
  method end_heading () =
    self#break (); output_string ch "</h1>"; need_break <- true; in_p <- false
  method start_section ?clss  () =
    self#break ();
    begin match clss with
      Some clss ->
        output_string ch ("<section class='" ^ html_escape clss ^ "'>")
    | None ->
        output_string ch "<section>"
    end;
    need_break <- true; in_p <- false
  method end_section () =
    self#break (); output_string ch "</section>";
    need_break <- true; in_p <- false
  method start_footer () =
    self#break ();
    output_string ch "<footer>";
    need_break <- true; in_p <- false
  method end_footer () =
    self#break (); output_string ch "</footer>";
    need_break <- true; in_p <- false
  method raw_html f =
    if not in_p then begin
      self#break (); output_string ch "<p>"; in_p <- true
    end;
    output_string ch (f ());
    need_break <- true
end

let space = Str.regexp " "

(*XXX recognize non-breaking spaces and replace them with spaces (?) *)
(*Unicode bullets? • *)

class format_printer f : printer = object
  val mutable at_flow_start = true
  val mutable in_p = false
  val mutable at_list_start = false
  val mutable ul_prefixes = []
  method start_doc () =
    Format.fprintf f "@[<v>";
    at_flow_start <- true; in_p <- false; ul_prefixes <- []
  method end_doc () =
    if in_p then Format.fprintf f "@]";
    Format.fprintf f "@]@."
  method text s =
    if not in_p then begin
      if not at_flow_start then Format.fprintf f "@ ";
      Format.fprintf f "@[";
    end;
    at_flow_start <- false; in_p <- true;
    List.iter
      (fun e ->
         match e with
           Str.Delim _ -> Format.fprintf f "@ "
         | Str.Text s  -> Format.fprintf f "%s" s)
      (Str.full_split space s)
  method change_p () =
    if in_p then Format.fprintf f "@]";
    in_p <- false
  method start_ul prefix =
    ul_prefixes <- prefix :: ul_prefixes;
    if in_p then Format.fprintf f "@]";
    at_list_start <- true; in_p <- false
  method li () =
    if at_list_start then begin
      if not at_flow_start then Format.fprintf f "@ ";
      Format.fprintf f "@[<v>";
    end else begin
      if in_p then Format.fprintf f "@]";
      Format.fprintf f "@]@ "
    end;
    Format.fprintf f "@[<v2>%s" (List.hd ul_prefixes);
    in_p <- false; at_list_start <- false; at_flow_start <- true
  method end_ul () =
    ul_prefixes <- List.tl ul_prefixes;
    if not at_list_start then begin
      if in_p then Format.fprintf f "@]";
      Format.fprintf f "@]@]";
      at_flow_start <- false
    end;
    at_list_start <- false; in_p <- false
  method start_a l = ()
  method end_a l = ()
  method start_code l = ()
  method end_code l = ()
  method start_dl ?clss () = at_list_start <- true; assert false
  method dt ?clss id =
    if at_list_start then begin
      Format.fprintf f "@[<v>"; at_list_start <- false
    end else
      Format.fprintf f "@]@]@ ";
    Format.fprintf f "@[<v2>* @["
  method dd () = Format.fprintf f "@]@ @["
  method end_dl () =
    if not at_list_start then Format.fprintf f "@]"
  method start_div ?clss () = ()
  method end_div () = ()
  method start_span ?clss () = ()
  method end_span () = ()
  method start_pre ?clss () = ()
  method end_pre () = ()
  method start_heading () = ()
  method end_heading () = ()
  method start_section ?clss () = ()
  method end_section () = ()
  method start_footer () = ()
  method end_footer () = ()
  method raw_html f = ()
end

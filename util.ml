
let enable_msgs = (* isatty is not available...*)
  (Unix.fstat Unix.stderr).Unix.st_kind = Unix.S_CHR

let cur_msg = ref ""

let hide_msg () =
  if !cur_msg <> "" then begin
    prerr_string "\r";
    prerr_string (String.make (String.length !cur_msg) ' ');
    prerr_string "\r";
    flush stderr;
  end

let show_msg () =
  if !cur_msg <> "" then begin prerr_string !cur_msg; flush stderr end

let set_msg s =
  if enable_msgs && s <> !cur_msg then begin
    hide_msg (); cur_msg := s; show_msg ()
  end

(****)

let warn_loc = ref None

let set_warning_location s = warn_loc := Some s
let reset_warning_location () = warn_loc := None

let print_warning s =
  hide_msg ();
  begin match !warn_loc with
    None    -> Format.eprintf "Warning: %s@." s
  | Some s' -> Format.eprintf "Warning (%s): %s@." s' s
  end;
  show_msg ()

let fail s =
  hide_msg ();
  Format.eprintf "Failure: %s@." s;
  exit 1

(****)

let title s = Format.printf "%s@.%s@." s (String.make (String.length s) '=')

(****)

module Timer = struct
  type t = float
  let start () = Unix.gettimeofday ()
  let stop t = Unix.gettimeofday () -. t
end

module IntSet =
  Set.Make (struct type t = int let compare x (y : int) = compare x y end)

(****)

module ListTbl = struct
  type ('a, 'b) t = ('a, 'b list ref) Hashtbl.t

  let create = Hashtbl.create

  let add h n p =
    try
      let l = Hashtbl.find h n in
      l := p :: !l
    with Not_found ->
      Hashtbl.add h n (ref [p])

  let find h n = try !(Hashtbl.find h n) with Not_found -> []

  let mem = Hashtbl.mem

  let iter f h = Hashtbl.iter (fun k l -> f k !l) h
end

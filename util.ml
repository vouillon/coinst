(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2005-2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * These programs are free software; you can redistribute them and/or
 * modify them under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

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

(****)

let print_list pr sep ch l =
  match l with
    []     -> ()
  | x :: r -> pr ch x; List.iter (fun x -> Format.fprintf ch "%s%a" sep pr x) r

(****)

let rec make_directories f =
  let f = Filename.dirname f in
  if not (Sys.file_exists f) then begin
    try
      Unix.mkdir f (0o755)
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      make_directories f;
      Unix.mkdir f (0o755)
  end

(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2011 Jérôme Vouillon
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

type t = { mutable state : bool; name : string; desc : string }

let debugs = ref []
let association = Hashtbl.create 11

let make s desc l =
  let d =
    try
      List.assoc s !debugs
    with Not_found ->
      let d = { state = false; name = s; desc = desc } in
      debugs := (s, d) :: !debugs;
      d
  in
  List.iter (fun s' -> Hashtbl.add association s' s) l;
  fun () -> d.state

let print () =
  Format.eprintf "Debug options:@.";
  List.iter
    (fun (_, d) -> Format.eprintf "    %s: %s@." d.name d.desc) !debugs;
  exit 1

let rec set s =
  if s = "help" || not (List.mem_assoc s !debugs) then
    print ()
  else
    try
      let d = List.assoc s !debugs in
      if not d.state then begin
        d.state <- true;
        List.iter set (Hashtbl.find_all association s)
      end
    with Not_found -> ()

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

(*
TODO (?)
- List broken new packages!
- Does it make sense to consider new packages as previously
  installable, and report issues for them in a uniform way?
- Print equivalence classes
*)

let broken_sets = Upgrade_common.empty_break_set ()

let (file1,file2) =
("snapshots/updates/stable", "snapshots/updates/testing")
(*
("snapshots/updates/oldstable", "snapshots/updates/stable")
("/tmp/last_month", "/tmp/new")
*)


let _ =
let output_file = ref "/tmp/upgrade.html" in
let l = ref [] in
let popcon_file = ref None in
let spec =
  Arg.align
  ["-o",
   Arg.String (fun d -> output_file := d),
   "FILE       Write output to file FILE";
   "--break",
   Arg.String (Upgrade_common.allow_broken_sets broken_sets),
   "SETS Ignore broken sets of packages of shape SETS";
   "--popcon",
   Arg.String (fun s -> popcon_file := Some s),
   "FILE Use popcon data from FILE";
   "--debug",
   Arg.String Debug.set,
   "NAME Activate debug option NAME"]
in
Arg.parse spec (fun f -> l := f :: !l)
  ("Usage: " ^ Sys.argv.(0) ^ " OPTIONS FILE1 FILE2\n\
    Takes two Debian binary package control files as input and computes\n\
    a core set of packages that were co-installable but are not anymore\n\
    after upgrade.\n\
    \n\
    Options:");
let (file1, file2) =
  match List.rev !l with
    [] -> (file1, file2)
  | [file1; file2] -> (file1, file2)
  | _ ->
    Format.eprintf
      "Exactly two Debian binary package control files \
       should be provided as input.@.";
    exit 1
in
if Sys.command "dot -V 2> /dev/null" <> 0 then begin
  Format.eprintf "Could not execute Graphviz 'dot' command.@.";
  exit 1
end;
let dist1 = Upgrade.read_data file1 in
let dist2 = Upgrade.read_data file2 in
Upgrade.f broken_sets dist1 dist2 ?popcon_file:!popcon_file !output_file

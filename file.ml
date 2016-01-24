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

let rec read_write ic oc =
  let bufsize = 4096 in
  let buf = Bytes.create bufsize in
  let rec read () =
    let n = input ic buf 0 bufsize in
    if n > 0 then begin
      output oc buf 0 n;
      read ()
    end
  in
  read ()

let pipe_to_command cmd input output =
  if Unix.fork () = 0 then begin
    Unix.dup2 input Unix.stdin; Unix.dup2 output Unix.stdout;
    Unix.close input; Unix.close output;
    Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  end

let spawn ?(sync=false) f =
  let (read_fd, write_fd) = Unix.pipe () in
  begin match Unix.fork () with
    0 ->
      Unix.close read_fd;
      f write_fd;
      exit 0
  | pid ->
      Unix.close write_fd;
      if sync then ignore (Unix.waitpid [] pid)
  end;
  read_fd

let pipe_gen feeder cmd =
  flush_all ();
  let in_read =
    (spawn ~sync:true
       (fun in_write ->
          let out_read =
            spawn ~sync:false
              (fun out_write ->
                 feeder (Unix.out_channel_of_descr out_write); exit 0)
          in
          pipe_to_command cmd out_read in_write;
          exit 0))
  in
  Unix.in_channel_of_descr in_read

let pipe_from_string s cmd = pipe_gen (fun ch -> output_string ch s) cmd

let pipe ic cmd =
  let in_read =
    (spawn ~sync:true
       (fun in_write ->
          let out_read =
            spawn ~sync:false
              (fun out_write ->
                 read_write ic (Unix.out_channel_of_descr out_write);
                 exit 0)
          in
          close_in ic;
          pipe_to_command cmd out_read in_write;
          exit 0))
  in
  close_in ic;
  Unix.in_channel_of_descr in_read

let has_magic ch s =
  let l = String.length s in
  let buf = Bytes.create l in
  let i = ref 0 in
  while
    !i < l && (let n = input ch buf !i (l - !i) in i := !i + n; n > 0)
  do () done;
  if !i > 0 then seek_in ch (pos_in ch - !i);
  !i = l && (Bytes.to_string buf) = s

let filter ch =
  if has_magic ch "\031\139" then pipe ch "exec gzip -cd" else
  if has_magic ch "BZh" then pipe ch "exec bzcat" else
  ch

let open_in file = filter (open_in file)

let open_in_multiple files =
  let ics = List.map Pervasives.open_in files in
  match ics with
    [ic] ->
      filter ic
  | _ ->
      let ic =
        Unix.in_channel_of_descr
          (spawn ~sync:true
             (fun write_fd ->
                if Unix.fork () = 0 then begin
                  let oc = Unix.out_channel_of_descr write_fd in
                  List.iter
                    (fun ic ->
                       let ic = filter ic in
                       read_write ic oc;
                       flush oc)
                    ics
                end;
                exit 0))
      in
      List.iter close_in ics;
      ic

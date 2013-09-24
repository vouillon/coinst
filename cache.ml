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

let make_uid () =
  let magic1 = 0xcab4ea850533f24dL in
  let magic2 = 0xb517d4f5440b7995L in
  Format.sprintf "%016Lx"
    (Int64.logxor
       (Int64.mul magic1 (Int64.of_float (1e6 *. Unix.gettimeofday ())))
       (Int64.mul magic2 (Int64.of_int (Unix.getpid ()))))

let cache_disabled = ref false

let set_disabled b = cache_disabled := b

let recompute cache magic f ch =
  begin match ch with Some ch -> close_in ch | None -> () end;
  let res = f () in
  let uid = make_uid () in
  if not !cache_disabled then begin
    let tmp = cache ^ ".tmp" in
    Util.make_directories tmp;
    let ch = open_out tmp in
    output_string ch magic;
    output_string ch uid;
    Marshal.to_channel ch res [];
    close_out ch;
    Sys.rename tmp cache
  end;
  (res, uid)

let cached ?(force=false) files cache magic ?(is_valid=fun _ -> true) f =
  let magic =
    Format.sprintf
      "This cache file can be safely removed at any time.\n%s\n%s\n\n"
      magic (String.concat "\n" files)
  in
  let ch = try Some (open_in cache) with Sys_error _ -> None in
  let should_compute =
    !cache_disabled || force ||
    match ch with
      None ->
        true
    | Some ch ->
        (try
           let cache_time =
             (Unix.fstat (Unix.descr_of_in_channel ch)).Unix.st_mtime in
             List.exists
               (fun file -> (Unix.stat file).Unix.st_mtime > cache_time)
               files
         with Unix.Unix_error (Unix.ENOENT, _, _) ->
           true)
          ||
        (try
           let l = String.length magic in
           let s = String.create l in
           really_input ch s 0 l;
           s <> magic
         with End_of_file ->
           true)
  in
  if should_compute then
    recompute cache magic f ch
  else begin
    match ch with
      Some ch ->
        let uid = String.create 16 in
        really_input ch uid 0 16;
        let res = Marshal.from_channel ch in
        close_in ch;
        if is_valid res then
          (res, uid)
        else
          recompute cache magic f None
    | None ->
        assert false
  end

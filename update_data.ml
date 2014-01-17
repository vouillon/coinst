(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2012 Jérôme Vouillon
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
- incremental update using Packages.diff?
*)

let src = "http://ftp.debian.org/debian/dists/"
let hint_src = "https://release.debian.org/britney/hints/"
let britney_src = "https://release.debian.org/britney/data-b2/"
let britney_files =
  [("Dates", `Testing, "Dates");
   ("Urgency", `Testing, "Urgency");
   ("testing_BugsV", `Testing, "BugsV");
   ("unstable_BugsV", `Unstable, "BugsV")]

let sects = ["main"; "contrib"; "non-free"]

let ext = "gz"
let decompress_tool = "zcat"

(****)

let download dst url =
  let tmp = dst ^ ".tmp" in
  begin try Sys.remove tmp with Sys_error _ -> () end;
  let cmd =
    if Sys.file_exists dst then
      Format.sprintf
        "curl -L -f -z %s --create-dirs -o %s %s 2>/dev/null" dst tmp url
    else
      Format.sprintf "curl -L -f --create-dirs -o %s %s 2>/dev/null" tmp url
  in
  Format.eprintf "Fetching %s...@." url;
(*
  Format.printf "> %s@." cmd;
*)
  if Sys.command cmd = 0 && Sys.file_exists tmp then
    ignore (Sys.rename tmp dst)

let rec make_directories f =
  let f = Filename.dirname f in
  if not (Sys.file_exists f) then begin
    try
      Unix.mkdir f (0o755)
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      make_directories f;
      Unix.mkdir f (0o755)
  end

let par_iter f l =
  let l = List.map (fun s -> (s, Task.spawn (fun () -> ()))) l in
  Task.iter l (fun (x, st) -> f st x) (fun () -> ());
  List.iter (fun (_, st) -> Task.kill st) l

(****)

let uncompress cache_dir target_dir suite arch_tmp arch_dst =
  let srcs =
    List.map
      (fun sect ->
         Filename.concat cache_dir (Format.sprintf "%s/%s" sect arch_tmp))
      sects
        @
    List.map
      (fun sect ->
         Filename.concat cache_dir
           (Format.sprintf "%s/debian-installer/%s" sect arch_tmp))
      sects
  in
  let dst = Filename.concat target_dir arch_dst in
  let should_update =
    try
      let t = (Unix.stat dst).Unix.st_mtime in
      List.exists
        (fun src ->
           try
             (Unix.stat src).Unix.st_mtime > t
           with Unix.Unix_error (Unix.ENOENT, _, _) ->
             false)
        srcs
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      true
  in
  if should_update then begin
    make_directories dst;
    let tmp = dst ^ ".tmp" in
    let cmd = decompress_tool ^ " " ^ String.concat " " srcs ^ " > " ^ tmp in
    Format.eprintf "> %s@." cmd;
    ignore (Sys.command cmd);
    ignore (Sys.rename tmp dst)
  end

let update_suite suite archs target_dir =
  let cache_dir = Filename.concat target_dir ".coinst_cache" in
  List.iter
    (fun (arch_tmp, arch_dst) ->
       List.iter
         (fun sect ->
            let file1 = Format.sprintf "%s/%s" sect arch_tmp in
            let url = Format.sprintf "%s%s/%s" src suite file1 in
            let dst = Filename.concat cache_dir file1 in
            download dst url;
            let file2 =
              Format.sprintf "%s/debian-installer/%s" sect arch_tmp in
            let url = Format.sprintf "%s%s/%s" src suite file2 in
            let dst = Filename.concat cache_dir file2 in
            download dst url)
         sects;
       uncompress cache_dir target_dir suite arch_tmp arch_dst)
    ((Format.sprintf "source/Sources.%s" ext, "Sources") ::
      List.map
        (fun s -> (Format.sprintf "binary-%s/Packages.%s" s ext,
                   Format.sprintf "Packages_%s" s))
        archs)

let update_suites suites archs =
  let f =
    Task.funct
      (fun _ (suite, archs, target_dir) -> update_suite suite archs target_dir)
  in
  par_iter (fun st (suite, dir) -> f st (suite, archs, dir))
    suites

(****)

let update_britney_files testing_dir unstable_dir =
  let f =
    Task.funct
      (fun _ (src_file, suite, dst_file) ->
         let dir =
           match suite with
             `Testing  -> testing_dir
           | `Unstable -> unstable_dir
         in
         let url = Format.sprintf "%s%s" britney_src src_file in
         let dst = Filename.concat dir dst_file in
         make_directories dst;
         ignore (download dst url))
  in
  par_iter f britney_files

(****)

let update_hints hint_dir hint_files =
  let f =
    Task.funct
      (fun _ file ->
         let url = Format.sprintf "%s%s" hint_src file in
         let dst = Filename.concat hint_dir file in
         ignore (download dst url))
  in
  par_iter f hint_files

(****)

let f testing_dir unstable_dir archs hint_dir hint_files =
  if Sys.command "curl -L -V > /dev/null" <> 0 then begin
    Format.eprintf "Could not execute 'curl' command.@.";
    exit 1
  end;
  update_suites [("testing", testing_dir); ("unstable", unstable_dir)] archs;
  update_britney_files testing_dir unstable_dir;
  update_hints hint_dir hint_files

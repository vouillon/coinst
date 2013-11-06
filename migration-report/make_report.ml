#!/usr/bin/ocaml unix.cma
(*
ocaml ./make_report.ml ~/coinst-report/britney.conf ~/coinst-report/report

cd ~/misc/coinst/migration-report && ./make_report.ml ~/coinst-report/britney.conf ~/coinst-report/report && rsync -a --delete ~/coinst-report/report/ vouillon@www.pps.univ-paris-diderot.fr:public_html/coinst/report/ 2>&1 > ~/coinst-report/log

*)

let conf = Sys.argv.(1)
let dir = Sys.argv.(2)

let files =
  ["index.html"; "arrows.png"; "jquery.js"; "jquery-ui.js";
   "jquery-ui.css"; "script.js"; "style.css"]

let break =
  ["libqt4-phonon"; "liboss4-salsa-asound2";
   "libboost1.49-dev"; "libboost-chrono1.49-dev"; "libboost-date-time1.49-dev";
   "libboost1.49-dbg"; "libboost-filesystem1.49-dev"; "libboost-graph1.49-dev";
   "libboost-graph-parallel1.49-dev"; "libboost-iostreams1.49-dev";
   "libboost-locale1.49-dev"; "libboost-math1.49-dev"; "libboost-mpi1.49-dev";
   "libboost-mpi-python1.49-dev"; "libboost-program-options1.49-dev";
   "libboost-python1.49-dev"; "libboost-random1.49-dev";
   "libboost-regex1.49-dev"; "libboost-serialization1.49-dev";
   "libboost-signals1.49-dev"; "libboost-system1.49-dev";
   "libboost-test1.49-dev"; "libboost-thread1.49-dev";
   "libboost-timer1.49-dev"; "libboost-wave1.49-dev"; "libboost1.49-all-dev";
   "libboost-mpi-python1.49.0";  "libboost-mpi-python1.49-dev";
   "libboost1.49-all-dev"; "libboost1.49-doc"]

let trace = true

(****)

let temp_files = ref []

let do_exit n =
  List.iter (fun nm -> try Sys.remove nm with Sys_error _ -> ()) !temp_files;
  exit n

let _ = Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> do_exit 0))

let cmd spec =
  Format.ksprintf
    (fun cmd ->
       if trace then Format.eprintf "+ %s@." cmd;
       if Sys.command cmd <> 0 then begin
         Format.eprintf "Command failed: %s@." cmd;
         do_exit 1
       end)
    spec

let temp prefix =
  let nm = Filename.temp_file prefix "" in
  temp_files := nm :: !temp_files;
  nm

let pipe_in_line spec =
  Format.ksprintf
    (fun cmd ->
       let ch = Unix.open_process_in cmd in
       let l =
         try
           input_line ch
         with End_of_file ->
           Format.eprintf "Command failed: %s@." cmd;
           do_exit 1
       in
       if Unix.close_process_in ch <> Unix.WEXITED 0 then begin
         Format.eprintf "Command failed: %s@." cmd;
         do_exit 1
       end;
       l)
    spec

(****)

let testing = pipe_in_line "grep TESTING %s | sed 's/.*= *//'" conf
let last_week = pipe_in_line "date -u +%%Y%%m%%dT%%H%%M%%SZ -d '1 week ago'"
let last_month = pipe_in_line "date -u +%%Y%%m%%dT%%H%%M%%SZ -d '1 month ago'"

let break_args =
  String.concat " " (List.map (fun p -> "--break " ^ p ^ ",_") break)

let popcon = temp "popcon"

let recent_issues date output =
  let old = temp date in
  cmd "curl -f -o %s \
    http://snapshot.debian.org/archive/debian/%s/\
    dists/testing/main/binary-i386/Packages.bz2" old date;
  cmd "../coinst-upgrades %s %s/Packages_i386 -o %s/%s --popcon %s %s"
    old testing dir output popcon break_args

let compare_to_stable output =
  let old = temp "stable" in
  cmd "curl -f -o %s \
    ftp://ftp.fr.debian.org/debian/dists/stable/main/binary-i386/Packages.bz2"
    old;
  cmd "../coinst-upgrades %s %s/Packages_i386 -o %s/%s --popcon %s %s"
    old testing dir output popcon break_args

let _ =
cmd "mkdir -p %s" dir;
cmd "rm -f %s/p/*.html" dir;
List.iter (fun f -> cmd "cp %s %s/" f dir) files;
cmd "curl -f -o %s http://popcon.debian.org/by_inst.gz" popcon;
cmd "../comigrate -c %s --update" conf;
recent_issues last_week "issues_week.html";
recent_issues last_month "issues_month.html";
compare_to_stable "issues_stable.html";
cmd "../comigrate -c %s --explain %s --popcon %s %s" conf dir popcon break_args

let _ =
do_exit 0

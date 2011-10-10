#! /usr/bin/ocaml unix.cma
let src = "http://ftp.debian.org/debian/dists/"
let dest = Filename.concat (Sys.getenv "HOME") "debian-dists"

(****)

let suites = ["oldstable"; "stable"; "testing"; "unstable"]
let archs = ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let sects = ["main"; "contrib"; "non-free"]
let ext = "bz2"

(****)

let download dst url =
  let cmd =
    if Sys.file_exists dst then
      Format.sprintf "curl -# -z %s --create-dirs -o %s %s" dst dst url
    else
      Format.sprintf "curl -# --create-dirs -o %s %s" dst url
  in
  Format.printf "> %s@." cmd;
  ignore (Sys.command cmd)

let _ =
List.iter
  (fun suite ->
     List.iter
       (fun arch_file ->
          List.iter
            (fun sect ->
               let file1 = Format.sprintf "%s/%s/%s" suite sect arch_file in
               let url = Format.sprintf "%s%s" src file1 in
               let dst = Filename.concat dest file1 in
               download dst url;
               let file2 =
                 Format.sprintf "%s/%s/debian-installer/%s"
                   suite sect arch_file
               in
               let url = Format.sprintf "%s%s" src file2 in
               let dst = Filename.concat dest file2 in
               download dst url)
            sects)
       (Format.sprintf "source/Sources.%s" ext ::
        List.map (fun s -> Format.sprintf "binary-%s/Packages.%s" s ext)
          archs))
  suites

(****)

let britney_src = "http://release.debian.org/britney/data-b2/"
let britney_dir = Filename.concat dest "britney"
let files =
  ["Dates"; "FauxPackages"; "Urgency"; "testing_BugsV"; "unstable_BugsV"]

let _ =
List.iter
  (fun file ->
     let url = Format.sprintf "%s%s" britney_src file in
     let dst = Filename.concat britney_dir file in
     download dst url)
  files

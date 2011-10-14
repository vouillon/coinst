#! /usr/bin/ocaml unix.cma
let src = "http://ftp.debian.org/debian/dists/"
let dest = Filename.concat (Sys.getenv "HOME") "debian-dists"
let britney_dir = Filename.concat dest "britney"

(****)

let suites = ["oldstable"; "stable"; "testing"; "unstable"]
let archs = ["i386"; "sparc"; "powerpc"; "armel"; "ia64"; "mips"; "mipsel"; "s390"; "amd64"; "kfreebsd-i386"; "kfreebsd-amd64"]
let sects = ["main"; "contrib"; "non-free"]
let ext = "bz2"

let download dst url =
  let cmd =
    if Sys.file_exists dst then
      Format.sprintf "curl -# -z %s --create-dirs -o %s %s" dst dst url
    else
      Format.sprintf "curl -# --create-dirs -o %s %s" dst url
  in
  Format.printf "> %s@." cmd;
  ignore (Sys.command cmd)

let rec make_directories f =
  let f = Filename.dirname f in
  if not (Sys.file_exists f) then begin
    try
      Unix.mkdir f (0o755)
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      make_directories f;
      Unix.mkdir f (0o755)
  end

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

let _ =
  let uncompress suite arch_src arch_dst =
    let srcs =
      List.map
        (fun sect ->
           Filename.concat dest
             (Format.sprintf "%s/%s/%s" suite sect arch_src))
        sects
          @
      List.map
        (fun sect ->
           Filename.concat dest
             (Format.sprintf "%s/%s/debian-installer/%s" suite sect arch_src))
        sects
    in
    let dst = Filename.concat britney_dir (Filename.concat suite arch_dst) in
    make_directories dst;
    let cmd = "bzcat " ^ String.concat " " srcs ^ " > " ^ dst in
    Format.printf "> %s@." cmd;
    ignore (Sys.command cmd)
  in
  List.iter
    (fun suite ->
       List.iter (fun (srcs, dst) -> uncompress suite srcs dst)
         ((Format.sprintf "source/Sources.%s" ext, "Sources") ::
          List.map
            (fun s -> (Format.sprintf "binary-%s/Packages.%s" s ext,
                       Format.sprintf "Packages_%s" s))
            archs))
    ["testing"; "unstable"]

(****)

let britney_src = "http://release.debian.org/britney/data-b2/"
let files =
  [("Dates", "testing/Dates");
   ("Urgency", "testing/Urgency");
   ("testing_BugsV", "testing/BugsV");
   ("unstable_BugsV", "unstable/BugsV")]

let _ =
List.iter
  (fun (src_file, dst_file) ->
     let url = Format.sprintf "%s%s" britney_src src_file in
     let dst = Filename.concat britney_dir dst_file in
     make_directories dst;
     download dst url)
  files

(****)

let hint_files =
  ["aba"; "adsb"; "faw"; "he"; "jcristau"; "luk"; "mehdi";
   "neilm"; "nthykier"; "pkern"; "vorlon"; "zobel";
   "freeze"; "freeze-exception"]

let hint_src = "http://release.debian.org/britney/hints/"
let hint_dir = Filename.concat britney_dir "unstable/Hints"

let _ =
List.iter
  (fun file ->
     let url = Format.sprintf "%s%s" hint_src file in
     let dst = Filename.concat hint_dir file in
     download dst url)
  hint_files

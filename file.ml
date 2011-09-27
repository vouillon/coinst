
let pipe_to_command cmd input output =
  if Unix.fork () = 0 then begin
    Unix.dup2 input Unix.stdin; Unix.dup2 output Unix.stdout;
    Unix.close input; Unix.close output;
    Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
  end

let forward ic oc =
  if Unix.fork () = 0 then begin
    let bufsize = 4096 in
    let buf = String.create bufsize in
    let rec read () =
      let n = input ic buf 0 bufsize in
      if n = 0 then exit 0;
      output oc buf 0 n;
      read ()
    in
    read ()
  end

let pipe cmd ic =
  let (in_read, in_write) = Unix.pipe () in
  let inchan = Unix.in_channel_of_descr in_read in
  begin match Unix.fork () with
    0 ->
      let (out_read, out_write) = Unix.pipe () in
      let outchan = Unix.out_channel_of_descr out_write in
      forward ic outchan;
      Unix.close out_write;
      pipe_to_command cmd out_read in_write;
      exit 0
  | pid ->
      ignore (Unix.waitpid [] pid)
  end;
  close_in ic; Unix.close in_write;
  inchan

let has_magic ch s =
  let l = String.length s in
  let buf = String.create l in
  really_input ch buf 0 l;
  seek_in ch (pos_in ch - l);
  buf = s


let filter ch =
  let buf = "abc" in
  really_input ch buf 0 3;
  seek_in ch 0;
  if has_magic ch "\031\139" then pipe "exec gzip -cd" ch else
  if has_magic ch "BZh" then pipe "exec bzcat" ch else
  ch

let open_in file = filter (open_in file)

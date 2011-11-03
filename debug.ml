
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

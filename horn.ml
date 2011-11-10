(*
- Retract one of these assumptions
- Get all reasons...
*)

let debug = Debug.make "horn" "Debug Horn clause solver." []

(****)

let string_extend s n c =
  let s' = String.make n c in
  String.blit s 0 s' 0 (String.length s);
  s'

let array_extend a n v =
  let a' = Array.make n v in
  Array.blit a 0 a' 0 (Array.length a);
  a'

(****)

module BitVect = struct
  type t = string
  let make n v = String.make n (if v then 'T' else 'F')
  let test vect x = Char.code vect.[x] <> Char.code 'F'
  let set vect x = vect.[x] <- 'T'
  let clear vect x = vect.[x] <- 'F'
  let extend vect n v = string_extend vect n (if v then 'T' else 'F')
  let sub = String.sub
end

(****)

module type S = sig
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int

  val initialize : ?signal_assign:(var array -> reason -> unit) -> int -> state
  val extend : state -> int -> unit

  val assignment : state -> BitVect.t
  val direct_reasons : state -> var -> (var array * reason) list

  val add_rule : state -> var array -> reason -> unit

  val assume : state -> var -> reason -> unit
end

module F (X : S) : SOLVER  with type reason = X.reason = struct

(* Variables *)
type var = int

type reason = X.reason

type clause =
  { lits : var array;
    reason : reason }

type explanation = Assumption of reason | Clause of clause | Unconstrained

type state =
  { (* Indexed by var *)
    mutable st_assign : BitVect.t;
    mutable st_reason : explanation array;
    mutable st_forward : clause list array;
    mutable st_backward : clause list array;
    mutable st_assumptions : clause list array;
    (* Queues *)
    st_prop_queue : var Queue.t;
    (* Misc *)
    st_signal_assign : (var array -> reason -> unit) option }

let print_clause f r =
  let r = r.lits in
  if Array.length r > 1 then begin
    for i = 1 to Array.length r - 1 do
      Format.fprintf f "%d " r.(i)
    done;
    Format.fprintf f " => "
  end;
  Format.fprintf f "%d" r.(0)

let is_unit st r =
  let lits = r.lits in
  let l = Array.length lits in
  let i = ref 1 in
  while !i < l && BitVect.test st.st_assign lits.(!i) do
    incr i
  done;
  !i = l

let rec enqueue st p reason =
  if not (BitVect.test st.st_assign p) then begin
    BitVect.set st.st_assign p;
    st.st_reason.(p) <- reason;
    begin match st.st_signal_assign, reason with
      Some f, Assumption reason -> f [|p|] reason
    | Some f, Clause r          -> f r.lits r.reason
    | Some _, Unconstrained     -> assert false
    | None,   _                 -> ()
    end;
    List.iter (fun r -> ignore (propagate_in_clause st r)) st.st_forward.(p)
  end

and propagate_in_clause st r =
  if debug () then Format.eprintf "Trying rule %a@." print_clause r;
  if is_unit st r then begin
    enqueue st r.lits.(0) (Clause r); true
  end else
    false

let add_rule st lits reason =
  let r = { lits = lits; reason = reason } in
  if debug () then Format.eprintf "Adding rule %a@." print_clause r;
  let l = Array.length lits in
  for i = 1 to l - 1 do
    let p = lits.(i) in
    st.st_forward.(p) <- r :: st.st_forward.(p)
  done;
  let p = lits.(0) in
  st.st_backward.(p) <- r :: st.st_backward.(p);
  ignore (propagate_in_clause st r)

let assume st p reason =
  let r = { lits = [|p|]; reason = reason } in
  st.st_assumptions.(p) <- r :: st.st_assumptions.(p);
  enqueue st p (Assumption reason)

(*
let retract st p =
  st.st_assumptions.(p) <- [];
  if st.st_reason.(p) = Assumption then begin
    st.st_reason.(p) <- Unconstrained;
    (*XXX Recursively cancel all consequences... *)
    ignore (List.exists (fun r -> propagate_in_clause st r) st.st_backward.(p))
  end
*)

let initialize ?signal_assign n =
  { st_assign = BitVect.make n false;
    st_reason = Array.make n Unconstrained;
    st_forward = Array.make n [];
    st_backward = Array.make n [];
    st_assumptions = Array.make n [];
    st_prop_queue = Queue.create ();
    st_signal_assign = signal_assign }

let extend st n =
  let n = max n (Array.length st.st_reason) in
  st.st_assign <- BitVect.extend st.st_assign n false;
  st.st_reason <- array_extend st.st_reason n Unconstrained;
  st.st_forward <- array_extend st.st_forward n [];
  st.st_backward <- array_extend st.st_backward n [];
  st.st_assumptions <- array_extend st.st_assumptions n []

let assignment st = st.st_assign

let direct_reasons st p =
  List.map (fun r -> (r.lits, r.reason))
    (List.filter (fun p -> is_unit st p) st.st_backward.(p) @
     List.filter (fun p -> is_unit st p) st.st_assumptions.(p))

end

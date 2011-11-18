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

let debug = Debug.make "horn" "Debug Horn clause solver." []
let stats =
  Debug.make "horn_stats" "Output stats regarding Horn clause solver." ["horn"]
let debug_retract =
  Debug.make "horn_retract" "Debug Horn assumption retraction." ["horn"]

let n1 = ref 0
let n2 = ref 0
let d = Array.make 10 0
let _ = at_exit (fun _ ->
if stats () && (!n1 > 0 || !n2 > 0) then begin
Format.eprintf "%d rules / %d assumptions@." !n1 !n2;
Array.iter (fun n -> Format.eprintf " %d" n) d;
Format.eprintf "@."
end)

(****)

module BitVect = Util.BitVect

module type S = sig
  type reason
end

module type SOLVER = sig
  type state

  type reason

  type var = int

  type id

  val initialize : ?signal_assign:(var array -> reason -> unit) -> int -> state
  val extend : state -> int -> unit
  val set_var_printer : state -> (Format.formatter -> var -> unit) -> unit

  val assignment : state -> BitVect.t
  val direct_reasons : state -> var -> (var array * reason) list
  val reason : state -> var -> (var array * reason) option
  val assumptions : state -> var -> reason list

  val add_rule : state -> var array -> reason -> id
  val assume : state -> var -> reason -> unit
  val retract_rule : state -> id -> unit
  val retract_assumptions : state -> var -> unit
end

module F (X : S) : SOLVER with type reason = X.reason = struct

(* Variables *)
type var = int

type reason = X.reason

type clause =
  { lits : var array;
    reason : reason }

type id = clause

type explanation = Assumption of reason | Clause of clause | Unconstrained

type state =
  { (* Indexed by var *)
    mutable st_assign : BitVect.t;
    mutable st_reason : explanation array;
    mutable st_forward : clause list array;
    mutable st_backward : clause list array;
    mutable st_assumptions : reason list array;
    (* Queues *)
    st_prop_queue : var Queue.t;
    (* Misc *)
    st_signal_assign : (var array -> reason -> unit) option;
    mutable st_var_printer : (Format.formatter -> var -> unit) option }

let set_var_printer st pr = st.st_var_printer <- Some pr

let print_var st f x =
  match st.st_var_printer with
    None    -> Format.fprintf f "%d" x
  | Some pr -> Format.fprintf f "%d (%a)" x pr x

let print_clause st f r =
  let r = r.lits in
  if Array.length r > 1 then begin
    for i = 1 to Array.length r - 1 do
      Format.fprintf f "%a " (print_var st) r.(i)
    done;
    Format.fprintf f " => "
  end;
  Format.fprintf f "%a" (print_var st) r.(0)

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
    List.iter (fun r -> propagate_in_clause st r) st.st_forward.(p)
  end

and propagate_in_clause st r =
  if debug () then Format.eprintf "Trying rule %a@." (print_clause st) r;
  if is_unit st r then enqueue st r.lits.(0) (Clause r)

let add_rule st lits reason =
incr n1;
let len = Array.length lits in
if len <= 10 then d.(len - 1) <- d.(len - 1) + 1;
  let r = { lits = lits; reason = reason } in
  if debug () then Format.eprintf "Adding rule %a@." (print_clause st) r;
  let l = Array.length lits in
  for i = 1 to l - 1 do
    let p = lits.(i) in
    st.st_forward.(p) <- r :: st.st_forward.(p)
  done;
  let p = lits.(0) in
  st.st_backward.(p) <- r :: st.st_backward.(p);
  propagate_in_clause st r;
  r

let assume st p reason =
incr n2;
  st.st_assumptions.(p) <- reason :: st.st_assumptions.(p);
  enqueue st p (Assumption reason)

let rec propagate_retraction st l p =
  if debug_retract () then
    Format.eprintf "Retracting assignment to variable %a@." (print_var st) p;
  st.st_reason.(p) <- Unconstrained;
  BitVect.clear st.st_assign p;
  p ::
  List.fold_left
    (fun l r ->
       if debug_retract () then
         Format.eprintf "Considering rule %a:@." (print_clause st) r;
       match st.st_reason.(r.lits.(0)) with
         Assumption _ | Unconstrained ->
           if debug_retract () then Format.eprintf "  does not apply.@.";
           l
       | Clause r' when r' != r ->
           if debug_retract () then Format.eprintf "  does not apply.@.";
           l
       | Clause _ ->
           if debug_retract () then Format.eprintf "  the rule was applied.@.";
           propagate_retraction st l r.lits.(0))
    l st.st_forward.(p)

let update_after_retraction st p =
  let l = propagate_retraction st [] p in
  (* Then, we see whether other rules apply instead. *)
  List.iter
    (fun q ->
       if st.st_reason.(q) = Unconstrained then
         List.iter (fun r -> propagate_in_clause st r) st.st_backward.(q))
    l;
  if debug_retract () then
    List.iter
      (fun q ->
         match st.st_reason.(q) with
           Unconstrained ->
             ()
         | Assumption _ ->
             Format.eprintf
               "Variable %a constrained for another reason (assumption).@."
               (print_var st) q
         | Clause r ->
             Format.eprintf
               "Variable %a constrained for another reason (%a).@."
               (print_var st) q (print_clause st) r)
      l

let retract_assumptions st p =
  (* We remove all the assumptions associated to variable p *)
  st.st_assumptions.(p) <- [];
  match st.st_reason.(p) with
    Assumption _ ->
      (* If variable p were directly constrained by an assumption,
         we recursively cancel the consequences of the this
         assumption. *)
      update_after_retraction st p
  | _ ->
      ()

let retract_rule st r =
  let lits = r.lits in
  let l = Array.length lits in
  for i = 1 to l - 1 do
    let p = lits.(i) in
    st.st_forward.(p) <- List.filter (fun r' -> r' != r) st.st_forward.(p)
  done;
  let p = lits.(0) in
  st.st_backward.(p) <- List.filter (fun r' -> r' != r) st.st_backward.(p);
  match st.st_reason.(p) with
    Clause r' when r == r' ->
      update_after_retraction st p
  | _ ->
      ()

let initialize ?signal_assign n =
  { st_assign = BitVect.make n false;
    st_reason = Array.make n Unconstrained;
    st_forward = Array.make n [];
    st_backward = Array.make n [];
    st_assumptions = Array.make n [];
    st_prop_queue = Queue.create ();
    st_signal_assign = signal_assign;
    st_var_printer = None }

let extend st n =
  let n = max n (Array.length st.st_reason) in
  st.st_assign <- BitVect.extend st.st_assign n false;
  st.st_reason <- Util.array_extend st.st_reason n Unconstrained;
  st.st_forward <- Util.array_extend st.st_forward n [];
  st.st_backward <- Util.array_extend st.st_backward n [];
  st.st_assumptions <- Util.array_extend st.st_assumptions n []

let assignment st = st.st_assign

let direct_reasons st p =
  List.map (fun r -> (r.lits, r.reason))
    (List.filter (fun p -> is_unit st p) st.st_backward.(p)) @
  List.map (fun reason -> ([|p|], reason)) st.st_assumptions.(p)

let reason st p =
  match st.st_reason.(p) with
    Clause r -> Some (r.lits, r.reason)
  | _        -> None

let assumptions st p = st.st_assumptions.(p)

end

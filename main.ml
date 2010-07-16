(*
XXX Perform checks: stable under composition, ...
XXX Simplifies p -> q \/ d  when p ## q (during flattening)
*)

module F (M : Api.S) = struct

  module Repository = Repository.F(M)
  open Repository

let simplify_formula confl f =
  Formula.filter
    (fun d ->
       Disj.for_all
         (fun p ->
            Conflict.exists confl (fun q -> not (Disj.implies1 q d)) p) d)
    f

let filter_conflicts confl p f =
  Formula.map
    (fun d -> Disj.filter (fun q -> not (Conflict.check confl p q)) d) f

let rec flatten_deps tbl dist deps conflicts visited l =
  Formula.fold
    (fun d (l, r) ->
       let (l', r') =
         Disj.fold
           (fun i (l, r) ->
              let (l', r') = flatten_dep tbl dist deps conflicts visited i in
              (Formula.disj l' l, PSet.union r r')) d (Formula._false, r)
       in
       (Formula.conj l' l, r'))
    l (Formula._true, PSet.empty)

and flatten_dep tbl dist deps conflicts visited i =
  try
    (Hashtbl.find tbl i, PSet.empty)
  with Not_found ->
    let res =
      if List.mem i visited then
        (Formula._true, PSet.singleton i)
      else begin
        let (l, r) =
          flatten_deps tbl dist deps conflicts (i :: visited) (PTbl.get deps i) in
(*
Format.eprintf "< %a@." (Formula.print dist) l;
*)
        let l = simplify_formula conflicts l in
(*???
        let l = filter_conflicts conflicts i l in
*)
Format.eprintf "%a > %a@." (Package.print dist) i (Formula.print dist) l;
(*
*)
        let r = PSet.remove i r in
        if Conflict.has conflicts i then
          (Formula.conj (Formula.lit i) l, r)
        else
          (l, r)
      end
    in
(*
Format.printf "%a: %a (%d)@."
(print_pack dist) i (pr_depends dist) (fst res) (PSet.cardinal (snd res));
*)
    (* Only cache the result if it is unconditionally true *)
    if PSet.is_empty (snd res) then Hashtbl.add tbl i (fst res);
    res


  let quotient dist confl deps =
    let classes_by_dep = Hashtbl.create 17 in
    let class_count = ref 0 in
    PTbl.iteri
      (fun p f ->
         let f = Formula.normalize f in
         let l =
           try
             let (l, _, _) = Hashtbl.find classes_by_dep f in l
           with Not_found ->
             incr class_count;
             let l = ref PSet.empty in
             Hashtbl.add classes_by_dep f (l, f, confl);
             l
         in
         l := PSet.add p !l)
      deps;


  (* Compute good representatives *)
  let repr = PTbl.create dist (Package.of_index (-1)) in
  Hashtbl.iter
    (fun _ (l, deps, confl) ->
       let ps =
         Formula.fold (fun d s -> PSet.union (Disj.to_lits d) s)
           deps PSet.empty
       in
       let l' = PSet.inter ps !l in
       let i = try PSet.choose l' with Not_found -> PSet.choose !l in
       PSet.iter (fun j -> PTbl.set repr j i) !l)
    classes_by_dep;

  (* Normalize dependencies and conflicts *)
  let classes = Hashtbl.create 101 in
  let normalize l =
    Formula.fold
      (fun d f ->
         Formula.conj
           (Formula.of_disj
              (Disj.fold (fun p d -> Disj.disj (Disj.lit (PTbl.get repr p)) d)
                 d Disj._false))
           f)
      l Formula._true
  in
  Hashtbl.iter
    (fun _ (l, deps, confl) ->
       let i = PTbl.get repr (PSet.choose !l) in
       let deps = normalize deps in
       let confl = normalize confl in

       Hashtbl.add classes i (!l, deps, confl))
    classes_by_dep;
  ()

  let class_size classes i =
    let (l, _, _) = Hashtbl.find classes i in List.length l

  let f ignored_packages ic =
    let dist = M.new_pool () in
    M.parse_packages dist ignored_packages ic;
    let confl = Conflict.create dist in
    let c = M.compute_conflicts dist in
    Array.iteri
      (fun p1 l ->
         List.iter
           (fun p2 ->
              Conflict.add confl
                (Package.of_index p1) (Package.of_index p2))
           l)
      c;

    let deps =
      let d = M.compute_deps dist in
      PTbl.init dist
        (fun p ->
           Formula.conjl
             (List.map (fun l' -> Formula.lit_disj (Package.of_index_list l'))
             d.(Package.index p)))
    in

    (****)
(*
  for i = 0 to M.pool_size dist - 1 do
    let p = Package.of_index i in
    let f = PTbl.get deps p in
    Format.eprintf "%a => %a@." (Package.print dist) p (Formula.print dist) f
  done;
*)

 let t1 = Unix.gettimeofday () in
  let tbl = Hashtbl.create 17 in

  let flatten_deps =
    PTbl.init dist (fun p -> fst (flatten_dep tbl dist deps confl [] p))
  in
Format.eprintf "==============================@.";

    let t2 = Unix.gettimeofday () in
Format.eprintf "%f@." (t2 -. t1);

  let conj_deps p =
    let f = PTbl.get flatten_deps p in
    Formula.fold
      (fun d s -> match Disj.to_lit d with Some p -> PSet.add p s | None -> s)
      f PSet.empty
  in
  Conflict.iter confl
    (fun p1 p2 ->
       let d1 = conj_deps p1 in
       let d2 = conj_deps p2 in
       if PSet.exists (fun q1 -> PSet.exists (fun q2 -> (p1 <> q1 || q2 <> p2) && Conflict.check confl q1 q2) d2) d1 then begin
         Format.eprintf "%a ## %a@."
           (Package.print dist) p1 (Package.print dist) p2;
         Conflict.remove confl p1 p2
       end);
Format.eprintf "==============================@.";

  let fd2 = PTbl.map (simplify_formula confl) flatten_deps in
Format.eprintf "==============================@.";

(*
  for i = 0 to M.pool_size dist - 1 do
    let p = Package.of_index i in
    let f = PTbl.get fd2 p in
    Format.eprintf "%a => %a@." (Package.print dist) p (Formula.print dist) f
  done;
*)
(*
  for i = 0 to M.pool_size dist - 1 do
    let p = Package.of_index i in
    let f1 = PTbl.get flatten_deps p in
    let f2 = PTbl.get fd2 p in
    if not (Formula.equiv f1 f2) then begin
      Format.eprintf "%a =>@." (Package.print dist) p;
      Format.eprintf "  %a@." (Formula.print dist) f1;
      Format.eprintf "  %a@." (Formula.print dist) f2
    end
  done;
*)

  let maybe_remove p f d =
    Disj.exists (fun q ->
      Conflict.for_all confl (fun r ->
        Formula.exists (fun d' -> Disj.implies d' d && not (Disj.implies1 q d')) (PTbl.get fd2 r)) q

&& (
Format.eprintf "%a =>(%a) %a@." (Package.print dist) p (Package.print dist) q (Disj.print dist) d;
true)

) d
  in
  let is_composition p f d =
  Formula.exists (fun d' ->
    not (Disj.equiv d d') && not (Disj.equiv (Disj.lit p) d') &&
    Disj.exists (fun q ->
      Formula.exists (fun d'' ->
        Disj.implies d (Disj.cut d' q d'') && (
Format.eprintf "XXX %a / %a -> %a@." (Disj.print dist) d' (Package.print dist) q (Disj.print dist) d''; true
)) (PTbl.get fd2 q)) d') f
in

PTbl.iteri (fun p f ->
Formula.iter f (fun d -> if Conflict.exists confl (fun q -> Disj.implies1 q d) p then Format.eprintf "YYY %a ==> %a@." (Package.print dist) p (Disj.print dist) d
)) fd2;


  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
  let fd2 =
  PTbl.mapi
    (fun p f ->
Formula.filter (fun d ->
  not (maybe_remove p f d) || is_composition p f d) f) fd2 
  in
Format.eprintf "==============================@.";
ignore fd2;

(*then
    begin
Format.eprintf "%a => %a@." (Package.print dist) p (Disj.print dist) d;
Format.eprintf "  %a@." (Formula.print dist) f;

if is_composition p f d then
Format.eprintf "XXX@."

(*
XXX check "composite"
*)
    end)
    ) fd2;
*)



(*
    let fd2 =
    Solver.f (Dgraph.invert (dep_dgraph dist deps))
      (fun m p ->
         let f = PTbl.get deps p in
         let res =
         Formula.fold
           (fun d f ->
              Formula.conj
                (Disj.fold
                   (fun p f ->
 Formula.disj (PMap.find p m) f) d Formula._false)
                f)
           f
           (if Conflict.has confl p then Formula.lit p else Formula._true)
         in
        let res = simplify_formula confl res in
res)
    in
    let t3 = Unix.gettimeofday () in
Format.eprintf "%f@." (t3 -. t2);
*)





(*===============================================*)
exit 1;
    let reversed_deps = PTbl.create dist [] in
    PTbl.iteri
      (fun p f ->
         Formula.iter f
           (fun d ->
              Disj.iter d
                (fun q ->
                   PTbl.set reversed_deps q
                     ((p, d) :: PTbl.get reversed_deps q))))
      deps;

    let dep_closure = PTbl.create dist Formula._true in
    let queue = Queue.create () in
    PTbl.iteri
      (fun p _ -> if Conflict.has confl p then Queue.add (p, Disj.lit p) queue)
      dep_closure;
    while not (Queue.is_empty queue) do
      let (p, d) = Queue.take queue in
      let f = PTbl.get dep_closure p in
      if not (Formula.implies1 f d) then begin
        PTbl.set dep_closure p (Formula.conj f (Formula.of_disj d));
Format.eprintf "%a => %a@." (Package.print dist) p (Formula.print dist) (Formula.conj f (Formula.of_disj d));
        let l = PTbl.get reversed_deps p in
        List.iter
          (fun (q, d') ->
             let f =
               Disj.fold
                 (fun p' f ->
                    Formula.disj f
                      (if p' = p then Formula.of_disj d else
                       PTbl.get dep_closure p'))
                 d' Formula._false
             in
             Formula.iter f (fun d -> Queue.add (q, d) queue))
          l
(*
        List.iter (fun (q, d') -> Queue.add (q, Disj.cut d' p d) queue) l
*)
      end
else
Format.eprintf "---@."
    done;
(*
XXX
- compute dependency closure
  ==> mapping package from disj that depends on it
  ==> start from packages with conflicts
      propagate (using a queue)

- simplifications...
*)
()

end

type kind = Cudf | Deb | Rpm

let _ =
let ignored_packages = ref [] in
let kind = ref Deb in
let file = ref None in
Arg.parse
  ["-cudf",
   Arg.Unit (fun () -> kind := Cudf),
   "  Parse CUDF files";
   "-rpm",
   Arg.Unit (fun () -> kind := Rpm),
   "  Parse hdlist.cz (RPM) files";
   "-deb",
   Arg.Unit (fun () -> kind := Deb),
   "  Parse (Debian) binary package control files (default)";
   "-ignore",
   Arg.String (fun p -> ignored_packages := p :: !ignored_packages),
   "PACKAGE   Ignore package of name PACKAGE";
(*
   "-max",
   Arg.Int (fun n -> max_size := n),
   "N   Limit to the size of non-coinstallable sets searched (default: 2)";
   "-graph",
   Arg.String (fun f -> graph_file := Some f),
   "FILE   Output coinstallability graph to file FILE";
   "-all",
   Arg.Unit (fun () -> mark_all := true),
   "  Include all packages in the coinstallability graph";
   "-explain",
   Arg.Unit (fun () -> explain := true),
   " Explain the results";
   "-debug",
   Arg.Unit (fun () -> debug := true),
   "  Output debugging informations";
*)
  ]
  (fun p -> file := Some p)
  ("Usage: " ^ Sys.argv.(0) ^ " [OPTION]...\n\
    Analyze package coinstallability.  The package information are read\n\
    from the standard input.\n\
    \n\
    Options:");

let ic = match !file with None -> stdin | Some f -> open_in f in
match !kind with
  Cudf -> let module M = F(Cudf_lib) in M.f !ignored_packages ic
| Deb  -> let module M = F(Deb_lib) in M.f !ignored_packages ic
| Rpm  -> let module M = F(Rpm_lib) in M.f !ignored_packages ic

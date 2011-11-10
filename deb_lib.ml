(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2005-2011 Jérôme Vouillon
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

let len = 4096
type st =
  { buf : string;
    mutable pos : int;
    mutable last : int;
    mutable start : int;
    mutable eof : bool;
    input : string -> int -> int -> int }

let start_token st = st.start <- st.pos

let get_token st ofs =
  let t = String.sub st.buf st.start (st.pos - st.start - ofs) in
  st.start <- -1;
  t

let from_channel ch =
  let buf = String.create len in
  { buf = buf; pos = 0; last = 0; eof = false; start = -1;
    input = fun buf pos len -> input ch buf pos len }

let from_string s =
  { buf = s; pos = 0; last = String.length s; eof = false; start = -1;
    input = fun _ _ _ -> 0 }

let refill st =
  if st.start <> -1 then begin
    st.pos <- st.pos - st.start;
    String.blit st.buf st.start st.buf 0 st.pos;
    st.start <- 0
  end else
    st.pos <- 0;
assert (len > st.pos);  (*FIX: error message...*)
  st.last <- st.pos + st.input st.buf st.pos (len - st.pos);
  if st.last = st.pos then st.eof <- true

let rec next st =
  let pos = st.pos in
  if pos < st.last then begin
    st.pos <- st.pos + 1;
    st.buf.[pos]
  end else if st.eof then '\n' else begin
    refill st;
    next st
  end

let unread st =
  if not st.eof then begin
    assert (st.pos > st.start && st.start <> -1);
    st.pos <- st.pos - 1
  end

let rec accept st c =
  let pos = st.pos in
  if pos < st.last then begin
    if st.buf.[pos] = c then begin
      st.pos <- st.pos + 1;
      true
    end else
      false
  end else if st.eof then
    false
  else begin
    refill st;
    accept st c
  end

let rec find st c =
  let pos = st.pos in
  if pos < st.last then begin
    let c' = st.buf.[pos] in
    st.pos <- st.pos + 1;
    c' = c
      ||
    (c' <> '\n' &&
     find st c)
  end else if st.eof then
    false
  else begin
    refill st;
    find st c
  end

let at_eof st =
  if st.pos = st.last then refill st;
  st.eof

(****)

let rec skip_blank_lines st = if accept st '\n' then skip_blank_lines st
let ignore_line st = ignore (find st '\n')

let accept_whitespace st = accept st ' ' || accept st '\t'
let skip_whitespaces st = while accept_whitespace st do () done

let parse_field ~field s st =
  start_token st;
  if not (find st ':') then failwith "Incorrect field: missing ':'";
  let name = get_token st 1 in
  skip_whitespaces st;
  if not (field s name st) then begin
    ignore_line st;
    while accept_whitespace st do
      ignore_line st
    done
  end

let parse_stanza ~start ~field ~finish st =
  skip_blank_lines st;
  if not (at_eof st) then begin
    let s = start () in
    while
      not (at_eof st || accept st '\n')
    do
      parse_field ~field s st
    done;
    finish s
  end

let parse_stanzas ~start ~field ~finish st =
  skip_blank_lines st;
  while not (at_eof st) do
    parse_stanza ~start ~field ~finish st
  done

let parse_field_end st =
  skip_whitespaces st;
  if not (accept st '\n' || at_eof st) then failwith "Garbage at end of field"

let parse_simple_field_content st =
  start_token st;
  let rec skip n =
    match next st with 
      ' ' | '\t' -> skip (n + 1)
    | '\n'       -> unread st; n
    | _          -> skip 0
  in
  let s = get_token st (skip 0) in
  parse_field_end st;
  s

(****)

let strings = Hashtbl.create 101
let common_string s =
  try Hashtbl.find strings s with Not_found -> Hashtbl.add strings s s; s

(****)

type rel = SE | E | EQ | L | SL
type version = int * string * string option
type dep = (string * (rel * version) option) list list
type p =
  { mutable num : int;
    mutable package : string;
    mutable version : version;
    mutable source : string * version;
    mutable section : string;
    mutable architecture : string;
    mutable depends : dep;
    mutable recommends : dep;
    mutable suggests : dep;
    mutable enhances : dep;
    mutable pre_depends : dep;
    mutable provides : dep;
    mutable conflicts : dep;
    mutable breaks : dep;
    mutable replaces : dep }

let dummy_version = (-1, "", None)

(****)

let package_names : (string, string) Hashtbl.t = Hashtbl.create 32768

let parse_package st =
  start_token st;
  let bad = ref false in
  begin match next st with
    'a'..'z' | '0'..'9' -> ()
  | 'A'..'Z' ->
      bad := true
  | _ ->
      failwith "Missing package name"
  end;
  while
    match next st with
      'a'..'z' | '0'..'9' | '.' | '+' | '-' ->
        true
    | 'A'..'Z' | '_' ->
        bad := true; true
    | _ ->
        unread st; false
  do () done;
  let s = get_token st 0 in
  if !bad || String.length s < 2 then
    Util.print_warning (Format.sprintf "bad package name '%s'" s);
  try
    Hashtbl.find package_names s
  with Not_found ->
    Hashtbl.add package_names s s;
    s

let parse_version_end st epoch n bad hyphen =
  unread st;
  if n = 0 then
    failwith (Format.sprintf "Bad version %d:%s" epoch (get_token st 0));
  let s = get_token st 0 in
  if bad then
    Util.print_warning (Format.sprintf "bad version '%d:%s'" epoch s);
  if hyphen = -1 then
    (epoch, s, None)
  else
    (epoch, String.sub s 0 hyphen,
     Some (String.sub s (hyphen + 1) (String.length s - hyphen - 1)))

let rec parse_upstream_version st epoch n bad hyphen =
  match next st with
    '0'..'9' ->
      parse_upstream_version st epoch (n + 1) bad hyphen
  | '-' ->
      parse_upstream_version st epoch (n + 1) (bad || n = 0) n
  | 'A'..'Z' | 'a'..'z' | '.' | '_' | '+' | '~' | ':' ->
      parse_upstream_version st epoch (n + 1) (bad || n = 0) hyphen
  | _ ->
      parse_version_end st epoch n bad hyphen

let rec parse_epoch st n =
  match next st with
    '0'..'9' ->
      parse_epoch st (n + 1)
  | ':' when n > 0 ->
      let epoch = int_of_string (get_token st 1) in
      start_token st;
      parse_upstream_version st epoch 0 false (-1)
  | 'A'..'Z' | 'a'..'z' | '.' | '_' | '+' | '~' | ':' ->
      parse_upstream_version st 0 (n + 1) (n = 0) (-1)
  | '-' ->
      parse_upstream_version st 0 (n + 1) (n = 0) n
  | _ ->
      parse_version_end st 0 n false (-1)

let parse_version st =
  start_token st;
  parse_epoch st 0

let versions = Hashtbl.create 32768

let parse_version st =
  let v = parse_version st in
  try Hashtbl.find versions v with Not_found -> Hashtbl.add versions v v; v

let parse_relation st =
  match next st with
    '<' ->
      begin match next st with
        '<' -> SE
      | '=' -> E
      | _   -> unread st; E
      end
  | '=' ->
      EQ
  | '>' ->
      begin match next st with
        '>' -> SL
      | '=' -> L
      | _   -> unread st; L
      end
  | c ->
      failwith (Format.sprintf "Bad relation '%c'" c)

let parse_package_dep f vers st =
  let name = parse_package st in
  skip_whitespaces st;
  if accept st '(' then begin
    if not vers then
      failwith (Format.sprintf "Package version not allowed in '%s'" f);
    skip_whitespaces st;
    let comp = parse_relation st in
    skip_whitespaces st;
    let version = parse_version st in
    skip_whitespaces st;
    if not (accept st ')') then assert false;
    skip_whitespaces st;
    (name, Some (comp, version))
  end else
    (name, None)

let rec parse_package_disj f vers disj st =
  let nm = parse_package_dep f vers st in
  if not (accept st '|') then
    [nm]
  else begin
    if not disj then begin
      if f = "Enhances" then
(*XXX Turn disjunction into conjunction? *)
        Util.print_warning
          (Format.sprintf "package disjunction not allowed in field '%s'" f)
      else
        failwith (Format.sprintf "Package disjunction not allowed in '%s'" f)
    end;
    skip_whitespaces st;
    nm :: parse_package_disj f vers disj st
  end

let rec parse_package_conj f vers disj st =
  let nm = parse_package_disj f vers disj st in
  if accept st '\n' || at_eof st then
    [nm]
  else if accept st ',' then begin
    skip_whitespaces st;
    nm :: parse_package_conj f vers disj st
  end else
    failwith (Format.sprintf "Bad character '%c'" (next st))

let parse_rel f vers disj st = parse_package_conj f vers disj st

let parse_package_source st =
  let name = parse_package st in
  skip_whitespaces st;
  if accept st '(' then begin
    skip_whitespaces st;
    let version = parse_version st in
    skip_whitespaces st;
    if not (accept st ')') then assert false;
    skip_whitespaces st;
    if not (accept st '\n' || at_eof st) then assert false;
    (name, version)
  end else begin
    if not (accept st '\n' || at_eof st) then assert false;
    (name, dummy_version)
  end

(****)

let print_version ch v =
  let (epoch, upstream_version, debian_revision) = v in
  if epoch <> 0 then Format.fprintf ch "%d:" epoch;
  Format.fprintf ch "%s" upstream_version;
  match debian_revision with
    None   -> ()
  | Some r -> Format.fprintf ch "-%s" r

(****)

module ListTbl = Util.ListTbl

type deb_pool =
  { mutable size : int;
    packages : (string * version, p) Hashtbl.t;
    packages_by_name : (string, p) ListTbl.t;
    packages_by_num : (int, p) Hashtbl.t;
    provided_packages : (string, p) ListTbl.t }

type pool = deb_pool

let new_pool () =
  { size = 0;
    packages = Hashtbl.create 32768;
    packages_by_name = ListTbl.create 32768;
    packages_by_num = Hashtbl.create 32768;
    provided_packages = ListTbl.create 32768 }

let insert_package pool p =
  if not (Hashtbl.mem pool.packages (p.package, p.version)) then begin
    p.num <- pool.size;
    pool.size <- pool.size + 1;
    Hashtbl.add pool.packages (p.package, p.version) p;
    Hashtbl.add pool.packages_by_num p.num p;
    ListTbl.add pool.packages_by_name p.package p;
    ListTbl.add pool.provided_packages p.package p;
    List.iter
      (fun l ->
         match l with
           [(n, None)] -> ListTbl.add pool.provided_packages n p
         | _           -> assert false)
      p.provides
  end

let parse_packages pool ignored_packages ch =
  let info = Common.start_parsing true ch in
  let st = from_channel ch in
  let start () =
    Common.parsing_tick info;
    { num = 0; package = " "; version = dummy_version;
      source = (" ", dummy_version); section = ""; architecture = "";
      depends = []; recommends = []; suggests = []; enhances = [];
      pre_depends = []; provides = []; conflicts = []; breaks = [];
      replaces = [] }
  in
  let finish q =
    assert (q.package <> " "); assert (q.version <> dummy_version);
    if fst q.source = " " then q.source <- (q.package, q.version);
    if snd q.source = dummy_version then q.source <- (fst q.source, q.version);

    if not (List.mem q.package ignored_packages) then insert_package pool q
  in
  let field q f st =
    match f with
      "Package"      -> q.package <- parse_package st; parse_field_end st; true
    | "Version"      -> q.version <- parse_version st; parse_field_end st; true
    | "Source"       -> q.source <- parse_package_source st; true
    | "Section"      -> q.section <-
                          common_string (parse_simple_field_content st);
                        true
    | "Architecture" -> q.architecture <-
                          common_string (parse_simple_field_content st);
                        true
    | "Depends"      -> q.depends <- parse_rel f true true st; true
    | "Recommends"   -> q.recommends <- parse_rel f true true st; true
    | "Suggests"     -> q.suggests <- parse_rel f true true st; true
    | "Enhances"     -> q.enhances <- parse_rel f true false st; true
    | "Pre-Depends"  -> q.pre_depends <- parse_rel f true true st; true
    | "Provides"     -> q.provides <- parse_rel f false false st; true
    | "Conflicts"    -> q.conflicts <- parse_rel f true false st; true
    | "Breaks"       -> q.breaks <- parse_rel f true false st; true
    | "Replaces"     -> q.replaces <- parse_rel f true false st; true
    | _              -> false
  in
  parse_stanzas ~start ~field ~finish st;
  Common.stop_parsing info

(****)

type s =
  { mutable s_name : string;
    mutable s_version : version;
    mutable s_section : string }

type s_pool =
  { mutable s_size : int;
    s_packages : (string, s) Hashtbl.t }

let new_src_pool () =
  { s_size = 0; s_packages = Hashtbl.create 16384 }

let parse_src_packages pool ch =
  let info = Common.start_parsing true ch in
  let st = from_channel ch in
  let start () =
    Common.parsing_tick info;
    { s_name = " "; s_version = dummy_version; s_section = "unknown" }
  in
  let field q f st =
    match f with
      "Package" -> q.s_name <- parse_package st; parse_field_end st; true
    | "Version" -> q.s_version <- parse_version st; parse_field_end st; true
    | "Section" -> q.s_section <-
                     common_string (parse_simple_field_content st);
                   true
    | _         -> false
  in
  let finish q =
    assert (q.s_name <> " "); assert (q.s_version <> dummy_version);
    Hashtbl.add pool.s_packages q.s_name q;
    pool.s_size <- pool.s_size + 1
  in
  parse_stanzas ~start ~field ~finish st;
  Common.stop_parsing info

(****)

let is_letter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_num c = c >= '0' && c <= '9'

let char_map =
  Array.init 256
    (fun c ->
      if Char.chr c = '~' then c - 256 else
	if is_letter (Char.chr c) then c else
          c + 256)

let compare_ver_char c1 c2 =
  compare (char_map.(Char.code c1)) (char_map.(Char.code c2))

let compare_ver_str s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let p1 = ref 0 in
  let p2 = ref 0 in
  while !p1 < l1 && !p2 < l2 && s1.[!p1] = s2.[!p2] do
    incr p1; incr p2
  done;
    if !p1 = l1 
    then
      if !p2 = l2 
      then
	0
      else
	if s2.[!p2] = '~' then 1 else -1
    else
      if !p2 = l2 
      then
	if s1.[!p1] = '~' then -1 else 1
      else
	compare_ver_char s1.[!p1] s2.[!p2]


let first_num s p l =
  let p = ref p in
  while !p < l && (s.[!p] < '0' || s.[!p] > '9') do incr p done;
  !p

let last_num s p l =
  let p = ref p in
  while !p < l && (s.[!p] >= '0' && s.[!p] <= '9') do incr p done;
  !p

let rec compare_rev_rec s1 p1 l1 s2 p2 l2 =
  let p1' = first_num s1 p1 l1 in
  let p2' = first_num s2 p2 l2 in
  let s1' = String.sub s1 p1 (p1' - p1) in
  let s2' = String.sub s2 p2 (p2' - p2) in
  let c = compare_ver_str s1' s2' in
  if c <> 0 then c else
  let p1'' = last_num s1 p1' l1 in
  let p2'' = last_num s2 p2' l2 in
  let s1' = String.sub s1 p1' (p1'' - p1') in
  let s2' = String.sub s2 p2' (p2'' - p2') in
  let i1 = if s1' = "" then 0. else float_of_string s1' in
  let i2 = if s2' = "" then 0. else float_of_string s2' in
  let c = compare i1 i2 in
  if c <> 0 then c else
  if p1'' = l1 && p2'' = l2 then 0 else
  compare_rev_rec s1 p1'' l1 s2 p2'' l2

let compare_rev s1 s2 =
  if s1 = s2 then 0 else
  compare_rev_rec s1 0 (String.length s1) s2 0 (String.length s2)

let compare_version (v1 : version) (v2 : version) =
  let (epoch1, upstream_version1, debian_revision1) = v1 in
  let (epoch2, upstream_version2, debian_revision2) = v2 in
  let c = compare epoch1 epoch2 in
  if c <> 0 then c else
  let c = compare_rev upstream_version1 upstream_version2 in
  if c <> 0 then c else
  match debian_revision1, debian_revision2 with
    None, None       -> 0
  | None, _          -> -1
  | _, None          -> 1
  | Some r1, Some r2 -> compare_rev r1 r2

(****)

let package_name pool n =
  let p = Hashtbl.find pool.packages_by_num n in
  p.package

let print_pack pool ch n =
  let p = Hashtbl.find pool.packages_by_num n in
  Format.fprintf ch "%s (= %a)" p.package print_version p.version

let print_pack_name pool ch n =
  Format.fprintf ch "%s" (package_name pool n)

(****)

let rec remove_duplicates_rec x (l : int list) =
  match l with
    []     ->
      [x]
  | y :: r ->
      if x = y then
        remove_duplicates_rec x r
      else
        x :: remove_duplicates_rec y r

let remove_duplicates l =
  match l with
    []     -> []
  | x :: r -> remove_duplicates_rec x r

let normalize_set (l : int list) =
  match l with
    [] | [_] -> l
  | _        -> remove_duplicates (List.sort (fun x y -> compare x y) l)

(****)

type deb_reason =
    R_conflict
    of int * int * (int * (string * (rel * version) option) list) option
  | R_depends
    of int * (string * (rel * (int * string * string option)) option) list

type reason = deb_reason

(****)

module Solver = Solver.F (struct type t = reason type reason = t end)

let print_rules = ref false

let add_conflict st confl l =
  let l = normalize_set l in
  if List.length l > 1 then begin
    if !print_rules then begin
      Format.printf "conflict (";
      List.iter (fun c -> Format.printf " %d" c) l;
      Format.printf ")@."
    end;
    let a = Array.of_list l in
    let len = Array.length a in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        let p = Solver.lit_of_var a.(i) false in
        let p' = Solver.lit_of_var a.(j) false in
        Solver.add_rule st [|p; p'|] [R_conflict (a.(i), a.(j), confl)]
      done
    done
  end

let add_depend st deps n l =
  let l = normalize_set l in
  (* Some packages depend on themselves... *)
  if not (List.memq n l) then begin
    if !print_rules then begin
      Format.printf "%d -> any-of (" n;
      List.iter (fun c -> Format.printf " %d" c) l;
      Format.printf ")@."
    end;
    Solver.add_rule st
      (Array.of_list
         (Solver.lit_of_var n false ::
          List.map (fun n' -> Solver.lit_of_var n' true) l))
      [R_depends (n, deps)];
    match l with
      [] | [_] -> ()
    | _        -> Solver.associate_vars st (Solver.lit_of_var n true) l
  end

(****)

let filter_rel rel c =
  match rel with
    SE -> c < 0
  | E  -> c <= 0
  | EQ -> c = 0
  | L  -> c >= 0
  | SL -> c > 0

let resolve_package_dep_raw pool (n, cstr) =
  match cstr with
    None ->
      ListTbl.find pool.provided_packages n
  | Some (rel, vers) ->
      List.filter
        (fun p -> filter_rel rel (compare_version p.version vers))
        (ListTbl.find pool.packages_by_name n)

let resolve_package_dep pool d =
  List.map (fun p -> p.num) (resolve_package_dep_raw pool d)

let single l =
  match l with
    [x] -> x
  | _   -> assert false

let generate_rules pool =
  let st = Common.start_generate (not !print_rules) pool.size in
  let pr = Solver.initialize_problem ~print_var:(print_pack pool) pool.size in
  (* Cannot install two packages with the same name *)
  ListTbl.iter
    (fun _ l ->
       add_conflict pr None (List.map (fun p -> p.num) l))
    pool.packages_by_name;
  Hashtbl.iter
    (fun _ p ->
       Common.generate_next st;
       if !print_rules then
         Format.eprintf "%s %a@." p.package print_version p.version;
       (* Dependences *)
       List.iter
         (fun l ->
            add_depend pr l p.num
              (List.flatten
                 (List.map (fun p -> resolve_package_dep pool p) l)))
         p.depends;
       List.iter
         (fun l ->
            add_depend pr l p.num
              (List.flatten
                 (List.map (fun p -> resolve_package_dep pool p) l)))
         p.pre_depends;
       (* Conflicts *)
       let c = List.map (fun p -> single p) p.conflicts in
       List.iter
         (fun n -> add_conflict pr (Some (p.num, c)) [p.num; n])
         (normalize_set
             (List.flatten
                (List.map (fun p -> resolve_package_dep pool p) c)));
       let c = List.map (fun p -> single p) p.breaks in
       List.iter
         (fun n -> add_conflict pr (Some (p.num, c)) [p.num; n])
         (normalize_set
             (List.flatten
                (List.map (fun p -> resolve_package_dep pool p) c))))
    pool.packages;
  Common.stop_generate st;
  Solver.propagate pr;
  pr

module IntSet = Util.IntSet

let generate_rules_restricted pool s =
  let pr = Solver.initialize_problem ~print_var:(print_pack pool) pool.size in
  let visited = ref IntSet.empty in
  let s = ref s in
  while not (IntSet.is_empty !s) do
    let n = IntSet.choose !s in
    s := IntSet.remove n !s;
    visited := IntSet.add n !visited;
    let p = Hashtbl.find pool.packages_by_num n in
    (* Dependences *)
    let add_deps l =
      let l' =
        List.flatten
          (List.map (fun p -> resolve_package_dep pool p) l)
      in
      List.iter
        (fun n -> if not (IntSet.mem n !visited) then s := IntSet.add n !s) l';
      add_depend pr l p.num l'
    in
    List.iter add_deps p.depends;
    List.iter add_deps p.pre_depends;
    (* Conflicts *)
    let c = List.map (fun p -> single p) p.conflicts in
    List.iter
      (fun n -> add_conflict pr (Some (p.num, c)) [p.num; n])
      (normalize_set
          (List.flatten
             (List.map (fun p -> resolve_package_dep pool p) c)));
    let c = List.map (fun p -> single p) p.breaks in
    List.iter
      (fun n -> add_conflict pr (Some (p.num, c)) [p.num; n])
      (normalize_set
          (List.flatten
             (List.map (fun p -> resolve_package_dep pool p) c)))
  done;
  (* Cannot install two packages with the same name *)
  ListTbl.iter
    (fun _ l ->
       add_conflict pr None (List.map (fun p -> p.num) l))
    pool.packages_by_name;
  Solver.propagate pr;
  pr

(****)

let parse_package_dependency pool s =
  let st = from_string s in
  let d = parse_package_dep "" true st in
  if not (at_eof st) then
    failwith (Format.sprintf "Bad character '%c'" (next st));
  resolve_package_dep pool d

let parse_package_name pool s =
  List.map (fun p -> p.num) (ListTbl.find pool.packages_by_name s)

let parse_version s =
  let st = from_string s in
  skip_whitespaces st;
  let v = parse_version st in
  skip_whitespaces st;
  if not (at_eof st) then
    failwith (Format.sprintf "Bad character '%c'" (next st));
  v

(****)

let print_rel ch rel =
  Format.fprintf ch "%s"
    (match rel with
       SE -> "<<"
     | E  -> "<="
     | EQ -> "="
     | L  -> ">="
     | SL -> ">>")

let print_package_ref ch (p, v) =
  Format.fprintf ch "%s" p;
  match v with
    None ->
      ()
  | Some (rel, vers) ->
      Format.fprintf ch " (%a %a)" print_rel rel print_version vers

let rec print_package_disj ch l =
  match l with
    []     -> ()
  | [p]    -> print_package_ref ch p
  | p :: r -> print_package_ref ch p; Format.fprintf ch " | ";
              print_package_disj ch r

let print_package_dependency ch l =
  Util.print_list print_package_disj ", " ch l

let check pool st =
  let assign = Solver.assignment st in
  Array.iteri
    (fun i v ->
       if v = Solver.True then begin
         let p = Hashtbl.find pool.packages_by_num i in
         Format.printf "Package: %a@." (print_pack pool) i;
         (* XXX No other package of the same name *)
         List.iter
           (fun p ->
              if p.num <> i && assign.(p.num) = Solver.True then begin
                Format.eprintf "PACKAGE %a ALSO INSTALLED!@."
                  (print_pack pool) p.num;
                exit 1
              end)
           (ListTbl.find pool.packages_by_name p.package);
         if p.depends <> [] then begin
           Format.printf "Depends: ";
           List.iter
             (fun l ->
                Format.printf "%a " print_package_disj l;
                try
                  let n =
                    List.find (fun n -> assign.(n) = Solver.True)
                      (List.flatten (List.map (resolve_package_dep pool) l))
                  in
                  Format.printf "{%a}, " (print_pack pool) n
                with Not_found ->
                  Format.printf "{UNSATISFIED}@.";
                  exit 1)
             p.depends;
           Format.printf "@."
         end;
         if p.pre_depends <> [] then begin
           Format.printf "Pre-Depends: ";
           List.iter
             (fun l ->
                Format.printf "%a " print_package_disj l;
                try
                  let n =
                    List.find (fun n -> assign.(n) = Solver.True)
                      (List.flatten (List.map (resolve_package_dep pool) l))
                  in
                  Format.printf "{%a}, " (print_pack pool) n
                with Not_found ->
                  Format.printf "{UNSATISFIED}@.";
                  exit 1)
             p.pre_depends;
           Format.printf "@."
         end;
         if p.conflicts <> [] then begin
           Format.printf "Conflicts: ";
           List.iter
             (fun l ->
                Format.printf "%a " print_package_disj l;
                try
                  let n =
                    List.find
                      (fun n -> n <> i && assign.(n) = Solver.True)
                      (resolve_package_dep pool (single l))
                  in
                  Format.printf "{CONFLICT: %a}" (print_pack pool) n;
                  exit 1
                with Not_found ->
                  Format.printf "{ok}, ")
             p.conflicts;
           Format.printf "@."
         end;
         if p.breaks <> [] then begin
           Format.printf "Breaks: ";
           List.iter
             (fun l ->
                Format.printf "%a " print_package_disj l;
                try
                  let n =
                    List.find
                      (fun n -> n <> i && assign.(n) = Solver.True)
                      (resolve_package_dep pool (single l))
                  in
                  Format.printf "{CONFLICT: %a}" (print_pack pool) n;
                  exit 1
                with Not_found ->
                  Format.printf "{ok}, ")
             p.breaks;
           Format.printf "@."
         end
       end)
    assign

let rec print_package_list_rec pool ch l =
  match l with
    []     -> Format.fprintf ch "NOT AVAILABLE"
  | [x]    -> print_pack pool ch x
  | x :: r -> Format.fprintf ch "%a, %a"
                (print_pack pool) x (print_package_list_rec pool) r

let print_package_list pool ch l =
  Format.fprintf ch "{%a}" (print_package_list_rec pool) l

let show_reasons pool l =
  if l <> [] then begin
    Format.printf "The following constraints cannot be satisfied:@.";
    List.iter
      (fun r ->
         match r with
           R_conflict (n1, n2, _) ->
             Format.printf "  %a conflicts with %a@."
               (print_pack pool) n1 (print_pack pool) n2
         | R_depends (n, l) ->
             Format.printf "  %a depends on %a %a@."
               (print_pack pool) n print_package_disj l
               (print_package_list pool)
               (List.flatten (List.map (resolve_package_dep pool) l)))
      l
  end

let conflicts_in_reasons rl =
  List.fold_left
    (fun cl ->
       function R_conflict (i,j,_) -> (min i j, max i j)::cl | _ -> cl) [] rl

(****)

(*XXX Build the array directly *)
let compute_conflicts pool =
  let conflict_pairs = Hashtbl.create 1000 in
  let conflicts = ListTbl.create 1000 in
  Hashtbl.iter
    (fun _ p ->
       List.iter
         (fun n ->
            let pair = (min n p.num, max n p.num) in
            if n <> p.num && not (Hashtbl.mem conflict_pairs pair) then begin
              Hashtbl.add conflict_pairs pair ();
              ListTbl.add conflicts p.num n;
              ListTbl.add conflicts n p.num
            end)
         (normalize_set
            (List.flatten
               (List.map (fun p -> resolve_package_dep pool (single p))
                   (p.breaks @ p.conflicts)))))
    pool.packages;
  Array.init pool.size (fun i -> ListTbl.find conflicts i)

let compute_deps dist =
  Array.init dist.size (fun i ->
    let p = Hashtbl.find dist.packages_by_num i in
    List.map
      (fun l ->
         match l with
           [p] ->
             normalize_set (resolve_package_dep dist p)
         | _ ->
             normalize_set
               (List.flatten
                  (List.map (fun p -> resolve_package_dep dist p) l)))
      (p.pre_depends @ p.depends))

(****)

let pool_size p = p.size

(****)

let only_latest pool' =
  let pool = new_pool () in
  ListTbl.iter
    (fun _ l ->
       let l =
         List.sort (fun p1 p2 -> - compare_version p1.version p2.version) l in
       insert_package pool {(List.hd l) with num = pool.size})
    pool'.packages_by_name;
  pool

let copy pool =
  { size = pool.size;
    packages = Hashtbl.copy pool.packages;
    packages_by_name = ListTbl.copy pool.packages_by_name;
    packages_by_num = Hashtbl.copy pool.packages_by_num;
    provided_packages = ListTbl.copy pool.provided_packages }

let merge pool filter pool' =
  Hashtbl.iter
    (fun _ p ->
       if filter p.num then insert_package pool {p with num = pool.size})
    pool'.packages

let merge2 pool filter pool' =
  Hashtbl.iter
    (fun _ p -> if filter p then insert_package pool {p with num = pool.size})
    pool'.packages

let add_package pool p =
  let num = pool.size in
  insert_package pool {p with num = num};
  (Hashtbl.find pool.packages (p.package, p.version)).num

let src_only_latest h =
  let h' = new_src_pool () in
  Hashtbl.iter
    (fun nm s ->
       try
         let s' = Hashtbl.find h'.s_packages nm in
         if compare_version s.s_version s'.s_version > 0 then
           Hashtbl.replace h'.s_packages nm s
       with Not_found ->
         Hashtbl.add h'.s_packages nm s;
         h'.s_size <- h'.s_size + 1)
    h.s_packages;
  h'

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

module ListTbl = Util.ListTbl
module StringTbl = Util.StringTbl
module PkgTbl = Util.IntTbl
module PkgSet = Util.IntSet

module Extarray = struct
  type 'a t =
    { def : 'a; mutable a : 'a array; mutable b : bool array }

  let create def =
    { def = def;
      a = Array.create 16000 def;
      b = Array.create 16000 false }

  let get a i =
    a.a.(i)

  let get_list a i =
    if i >= Array.length a.a then [] else a.a.(i)

  let find a i =
    if i >= Array.length a.a || not a.b.(i) then
      raise Not_found
    else
      a.a.(i)

  let iter f a =
    for i = 0 to Array.length a.a - 1 do
      if a.b.(i) then f a.a.(i)
    done

  let iteri f a =
    for i = 0 to Array.length a.a - 1 do
      if a.b.(i) then f i a.a.(i)
    done

  let resize a i =
    let l = Array.length a.a in
    let a' = Array.create (2 * l) a.def in
    Array.blit a.a 0 a' 0 l;
    a.a <- a';
    let b' = Array.create (2 * l) false in
    Array.blit a.b 0 b' 0 l;
    a.b <- b'
    
  let add a i v =
    while i >= Array.length a.a do resize a i done;
    assert (not a.b.(i));
    a.a.(i) <- v;
    a.b.(i) <- true

  let add_to_list a i v =
    while i >= Array.length a.a do resize a i done;
    a.a.(i) <- v :: a.a.(i);
    a.b.(i) <- true

  let replace a i v =
    while i >= Array.length a.a do resize a i done;
    a.a.(i) <- v;
    a.b.(i) <- true

  let remove a i =
    assert (a.b.(i));
    a.a.(i) <- a.def;
    a.b.(i) <- false

  let remove_from_list a i p =
    let l = List.filter (fun v -> not (p v)) a.a.(i) in
    a.a.(i) <- l;
    a.b.(i) <- l <> []

  let copy a = {a with a = Array.copy a.a; b = Array.copy a.b}

  let mem a i = i < Array.length a.a && a.b.(i)

  let is_prefix eq a a' =
    let l = Array.length a.a in
    let l' = Array.length a'.a in
    let res = ref true in
    for i = 0 to min l l' - 1 do
      if a.b.(i) && not (a'.b.(i) && eq a.a.(i) a'.a.(i)) then res := false
    done;
    for i = l' to l - 1 do
      if a.b.(i) then res := false
    done;
    !res
end

module PkgDenseTbl = Extarray

module Dict = struct
  type t =
    { mutable next : int; to_id : int StringTbl.t; of_id : string Extarray.t }
  let create () =
    { next = 0;
      to_id = StringTbl.create 32768; of_id = Extarray.create "" }
  let to_id d s = StringTbl.find d.to_id s
  let add d s =
    try
      to_id d s
    with Not_found ->
      let n = d.next in
      d.next <- n + 1;
      StringTbl.add d.to_id s n;
      Extarray.add d.of_id n s;
      n
  let of_id d n = Extarray.get d.of_id n
  let exists d s = StringTbl.mem d.to_id s
  let is_extended d d' = Extarray.is_prefix (fun x y -> x = y) d.of_id d'.of_id
end

(****)

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

let get_token_suffixed st ofs suffix =
  let len = st.pos - st.start - ofs in
  let len' = String.length suffix in
  let t = String.create (len + len') in
  String.blit st.buf st.start t 0 len;
  String.blit suffix 0 t len len';
  st.start <- -1;
  t

let ignore_token st = st.start <- -1

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
    assert (st.pos > st.start);
    assert (st.start <> -1);
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

let rec find_rec st c buf last =
  let pos = st.pos in
  if pos < last then begin
    let c' = String.unsafe_get buf pos in
    st.pos <- pos + 1;
    c' = c
      ||
    (c' <> '\n' &&
     find_rec st c buf last)
  end else if st.eof then
    false
  else begin
    refill st;
    find_rec st c st.buf st.last
  end

let find st c = find_rec st c st.buf st.last

let at_eof st =
  if st.pos = st.last then refill st;
  st.eof

let print_location st =
  let i = ref (max (st.pos - 2) 0) in
  while !i > 0 && (st.buf.[!i] <> '\n' || st.buf.[!i + 1] <> '\n') do
    decr i
  done;
  if st.buf.[!i] = '\n' && st.buf.[!i + 1] = '\n' then i := !i + 2;
  Format.eprintf "%s<HERE>%s@."
    (String.sub st.buf !i (st.pos - !i))
    (String.sub st.buf st.pos (st.last - st.pos))

let fail st s =
  print_location st;
  Format.eprintf "Parsing error: %s@." s;
  exit 1

(****)

let rec skip_blank_lines st = if accept st '\n' then skip_blank_lines st
let ignore_line st = ignore (find st '\n')

let accept_whitespace st = accept st ' ' || accept st '\t'
let skip_whitespaces st = while accept_whitespace st do () done

let parse_field ~field s st =
  start_token st;
  if not (find st ':') then fail st "incorrect field (missing ':')";
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
  if not (accept st '\n' || at_eof st) then
    fail st "garbage at end of field"

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

let strings = StringTbl.create 101
let common_string s =
  try StringTbl.find strings s with Not_found -> StringTbl.add strings s s; s

(****)

type rel = SE | E | EQ | L | SL
type package_name = int
type version = string
type 'a dep = ('a * (rel * version) option) list
type deps = package_name dep list
type p =
  { mutable num : int;
    mutable package : package_name;
    mutable version : version;
    mutable source : package_name * version;
    mutable section : string;
    mutable architecture : string;
    mutable depends : deps;
    mutable recommends : deps;
    mutable suggests : deps;
    mutable enhances : deps;
    mutable pre_depends : deps;
    mutable provides : deps;
    mutable conflicts : deps;
    mutable breaks : deps;
    mutable replaces : deps }

let dummy_package =
  { num = -1; package = -1; version = ""; source = (-1, ""); section = "";
    architecture = ""; depends = []; recommends = []; suggests = [];
    enhances = []; pre_depends = []; provides = []; conflicts = [];
    breaks = []; replaces = [] }

type dict = Dict.t
let dict = ref (Dict.create ())
let current_dict () = !dict
let valid_directory d = Dict.is_extended !dict d
let set_dict d =
 assert (valid_directory d);
 dict := d
let name_of_id id = Dict.of_id !dict id
let id_of_name nm = Dict.to_id !dict nm
let add_name nm = Dict.add !dict nm
let name_exists nm = Dict.exists !dict nm

(****)

module Version = struct
  let rec epoch s i =
    let c = String.unsafe_get s i in
    match c  with
      '0' .. '9' -> epoch s (i + 1)
    | _          -> if c = ':' then i else -1

  let rec compare_substring s i s' i' n =
    if n = 0 then 0 else begin
      let c = Char.code (String.unsafe_get s i) in
      let c' = Char.code (String.unsafe_get s' i') in
      if c < c' then -1 else
      if c > c' then 1 else
      compare_substring s (i + 1) s' (i' + 1) (n - 1)
    end

  let rec skip_zeroes s i =
    if String.unsafe_get s i = '0' then skip_zeroes s (i + 1) else i

  let rec skip_digits s i =
    match String.unsafe_get s i with
      '0' .. '9' -> skip_digits s (i + 1)
    | _          -> i

  let rec digit_start s i =
    if i = 0 then 0 else
    let i' = i - 1 in
    match String.unsafe_get s i' with
      '0'..'9' -> digit_start s (i - 1)
    | _        -> i

  let is_letter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  let is_digit c = c >= '0' && c <= '9'

  let char_map =
    Array.init 256
      (fun c ->
         if Char.chr c = '~' then c - 512 else
         if Char.chr c = ' ' || is_digit (Char.chr c) then c - 256 else
         if is_letter (Char.chr c) then c else
         c + 256)

  let rec compare_versions_rec s s' i i' c'' =
    let c = String.unsafe_get s i in
    let c' = String.unsafe_get s' i' in
    if c = c' then
      compare_versions_rec s s' (i + 1) (i' + 1) c
    else
      match c, c', c'' with
        '0'..'9', _, '0'..'9'
      | _, '0'..'9', '0'..'9'
      | '0'..'9', '0'..'9', _ ->
          let j = digit_start s i in
          let j' = i' - i + j in
          let j = skip_zeroes s j in
          let j' = skip_zeroes s' j' in
          let l = skip_digits s i in
          let l' = skip_digits s' i' in
          let n = l - j in
          let n' = l' - j' in
          if n < n' then -1 else
          if n > n' then 1 else
          let c = compare_substring s j s' j' n in
          if c <> 0 then c else
          compare_versions_rec s s' l l' ' '
      | _ ->
          let c = char_map.(Char.code c) in
          let c' = char_map.(Char.code c') in
          if c < c' then -1 else
          if c > c' then 1 else
          0

  let compare s s' =
    if s = s' then 0 else
    let i = epoch s 0 in
    let i' = epoch s' 0 in
    if i >= 0 || i' >= 0 then begin
      let j = skip_zeroes s 0 in
      let j' = skip_zeroes s' 0 in
      let l = max 0 i in
      let l' = max 0 i' in
      let n = l - j in
      let n' = l' - j' in
      if n < n' then -1 else
      if n > n' then 1 else
      let c = compare_substring s j s' j' n in
      if c <> 0 then c else
      compare_versions_rec s s' (i + 1) (i' + 1) ' '
    end else
      compare_versions_rec s s' 0 0 ' '

  let print ch v =
    let len = String.length v - 2 in
    let s = String.sub v 0 len in
    for i = 0 to len - 1 do
      if s.[i] = ' ' then s.[i] <- '-'
    done;
    Format.fprintf ch "%s" s

  let to_string v =
    let len = String.length v - 2 in
    let s = String.sub v 0 len in
    for i = 0 to len - 1 do
      if s.[i] = ' ' then s.[i] <- '-'
    done;
    s

  let dummy = ""

  let get st =
    let v = get_token_suffixed st 0 "  " in
    try
      let i = String.rindex v '-' in
      v.[i] <- ' ';
      v
    with Not_found ->
      v
end

let print_version = Version.print
let compare_version = Version.compare
let dummy_version = Version.dummy
let string_of_version = Version.to_string

(****)

let parse_package st =
  start_token st;
  let bad = ref false in
  begin match next st with
    'a'..'z' | '0'..'9' -> ()
  | 'A'..'Z' ->
      bad := true
  | _ ->
      fail st "missing package name"
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
  s

let parse_arch st =
  start_token st;
  while
    match next st with
      'a'..'z' | '0'..'9' | '.' | '+' | '-' ->
        true
    | _ ->
        unread st; false
  do () done;
  get_token st 0

let debug_versions = Debug.make "versions" "Print bad version warnings" []

let parse_version_end st n bad hyphen =
  unread st;
  if n = 0 then
    fail st (Format.sprintf "bad version %s" (get_token st 0));
  let s = Version.get st in
  if bad && debug_versions () then
    Util.print_warning (Format.sprintf "bad version '%s'" s);
  s

let rec parse_upstream_version st n bad hyphen =
  match next st with
    '0'..'9' ->
      parse_upstream_version st (n + 1) bad hyphen
  | '-' ->
      parse_upstream_version st (n + 1) (bad || n = 0) n
  | 'A'..'Z' | 'a'..'z' | '.' | '_' | '+' | '~' | ':' ->
      parse_upstream_version st (n + 1) (bad || n = 0) hyphen
  | _ ->
      parse_version_end st n bad hyphen

let rec parse_epoch st n =
  match next st with
    '0'..'9' ->
      parse_epoch st (n + 1)
  | ':' when n > 0 ->
      parse_upstream_version st 0 false (-1)
  | 'A'..'Z' | 'a'..'z' | '.' | '_' | '+' | '~' | ':' ->
      parse_upstream_version st (n + 1) (n = 0) (-1)
  | '-' ->
      parse_upstream_version st (n + 1) (n = 0) n
  | _ ->
      parse_version_end st n false (-1)

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
      start_token st;
      let rel =
        match next st with
          '<' -> SE
        | '=' -> E
        | _   -> unread st; E
      in
      ignore_token st;
      rel
  | '=' ->
      EQ
  | '>' ->
      start_token st;
      let rel =
        match next st with
          '>' -> SL
        | '=' -> L
        | _   -> unread st; L
      in
      ignore_token st;
      rel
  | c ->
      fail st (Format.sprintf "bad relation '%c'" c)

let parse_package_dep f vers st =
  let name = parse_package st in
  skip_whitespaces st;
  let name =
    if accept st ':' then begin
      let arch = parse_arch st in
      skip_whitespaces st;
      if arch = "any" then name else name ^ ":" ^ arch
    end else
      name
  in
  let name = Dict.add !dict name in
  if accept st '(' then begin
    if not vers then
      fail st (Format.sprintf "package version not allowed in '%s'" f);
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
        fail st (Format.sprintf "package disjunction not allowed in '%s'" f)
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
    (* Hack to parse multiline Binary fields... *)
    if accept st '\n' then skip_whitespaces st;
    nm :: parse_package_conj f vers disj st
  end else
    fail st (Format.sprintf "bad character '%c'" (next st))

let parse_rel f vers disj st = parse_package_conj f vers disj st

let parse_package_source st =
  let name = Dict.add !dict (parse_package st) in
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

type deb_pool =
  { mutable size : int;
    packages_by_name : p list Extarray.t;
    packages_by_num : p Extarray.t;
    provided_packages : p list Extarray.t }

type pool = deb_pool

let new_pool () =
  { size = 0;
    packages_by_name = Extarray.create [];
    packages_by_num = Extarray.create dummy_package;
    provided_packages = Extarray.create [] }

let find_package_by_num pool n = Extarray.get pool.packages_by_num n
let find_packages_by_name pool nm = Extarray.get_list pool.packages_by_name nm
let has_package_of_name pool nm =
  Extarray.get_list pool.packages_by_name nm <> []
let iter_packages_by_name pool f = Extarray.iteri f pool.packages_by_name
let iter_packages pool f =
  (*iter_packages_by_name pool (fun _ l -> List.iter f l)*)
  Extarray.iter f pool.packages_by_num
let pool_size pool = pool.size
let find_provided_packages pool nm =
  find_packages_by_name pool nm @ Extarray.get_list pool.provided_packages nm
let package_is_provided pool nm =
  has_package_of_name pool nm ||
  Extarray.get_list pool.provided_packages nm <> []

let has_package pool nm v =
  List.exists (fun p -> compare_version p.version v = 0)
    (find_packages_by_name pool nm)

let insert_package pool p =
  if not (has_package pool p.package p.version) then begin
    p.num <- pool.size;
    pool.size <- pool.size + 1;
    Extarray.add pool.packages_by_num p.num p;
    Extarray.add_to_list pool.packages_by_name p.package p;
    List.iter
      (fun l ->
         match l with
           [n,_] -> Extarray.add_to_list pool.provided_packages n p
         | _   -> assert false)
      p.provides
  end

let remove_package pool p =
  Extarray.remove pool.packages_by_num p.num;
  Extarray.remove_from_list
    pool.packages_by_name p.package (fun q -> q.num = p.num);
  List.iter
    (fun l ->
       match l with
         [n,_] -> Extarray.remove_from_list pool.provided_packages n
                  (fun q -> q.num = p.num)
       | _   -> assert false)
    p.provides

let replace_package pool q p =
  let p = {p with num = q.num} in
  remove_package pool q;
  assert (not (has_package pool p.package p.version));
  Extarray.add pool.packages_by_num p.num p;
  Extarray.add_to_list pool.packages_by_name p.package p;
  List.iter
    (fun l ->
       match l with
         [n,_] -> Extarray.add_to_list pool.provided_packages n p
       | _   -> assert false)
    p.provides

let parse_packages pool ignored_packages ch =
  let ignored_packages = List.map (Dict.add !dict) ignored_packages in
  let info = Common.start_parsing true ch in
  let st = from_channel ch in
  let start () =
    Common.parsing_tick info;
    { num = 0; package = -1; version = dummy_version;
      source = (-1, dummy_version); section = ""; architecture = "";
      depends = []; recommends = []; suggests = []; enhances = [];
      pre_depends = []; provides = []; conflicts = []; breaks = [];
      replaces = [] }
  in
  let finish q =
    assert (q.package <> -1); assert (q.version <> dummy_version);
    if fst q.source = -1 then q.source <- (q.package, q.version);
    if snd q.source = dummy_version then q.source <- (fst q.source, q.version);

    if not (List.mem q.package ignored_packages) then insert_package pool q
  in
  let field q f st =
    match f with
      "Package"      -> q.package <- Dict.add !dict (parse_package st);
                        parse_field_end st; true
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
    | "Provides"     -> q.provides <- parse_rel f true false st; true
    | "Conflicts"    -> q.conflicts <- parse_rel f true false st; true
    | "Breaks"       -> q.breaks <- parse_rel f true false st; true
    | "Replaces"     -> q.replaces <- parse_rel f true false st; true
    | _              -> false
  in
  parse_stanzas ~start ~field ~finish st;
  Common.stop_parsing info

(****)

type s =
  { mutable s_name : package_name;
    mutable s_version : version;
    mutable s_section : string;
    mutable s_binary : package_name list;
    mutable s_extra_source : bool}

type s_pool =
  { mutable s_size : int;
    s_packages : s list Extarray.t }

let new_src_pool () = { s_size = 0; s_packages = Extarray.create [] }

let find_source_by_name pool nm =
  match Extarray.get_list pool.s_packages nm with
    []  -> raise Not_found
  | [s] -> s
  | _   -> assert false

let has_source pool nm = Extarray.get_list pool.s_packages nm <> []

let remove_source pool nm = Extarray.remove pool.s_packages nm

let add_source pool s = Extarray.add_to_list pool.s_packages s.s_name s

let iter_sources f pool =
  Extarray.iter
    (fun l -> match l with [] -> () | [s] -> f s | _ -> List.iter f l)
    pool.s_packages

let parse_src_packages pool ch =
  let info = Common.start_parsing true ch in
  let st = from_channel ch in
  let start () =
    Common.parsing_tick info;
    { s_name = -1; s_version = dummy_version; s_section = "unknown";
      s_binary = []; s_extra_source = false }
  in
  let field q f st =
    match f with
      "Package" -> q.s_name <- Dict.add !dict (parse_package st);
                   parse_field_end st; true
    | "Version" -> q.s_version <- parse_version st; parse_field_end st; true
    | "Section" -> q.s_section <-
                     common_string (parse_simple_field_content st);
                   true
    | "Binary"  -> q.s_binary <-
                     List.map
                       (function d ->
                          match d with [(nm, None)] -> nm | _ -> assert false)
                       (parse_rel f false false st);
                   true
    | "Extra-Source-Only" ->
                   q.s_extra_source <- parse_simple_field_content st = "yes";
                   true
    | _         -> false
  in
  let finish q =
    assert (q.s_name <> -1); assert (q.s_version <> dummy_version);
    Extarray.add_to_list pool.s_packages q.s_name q;
    pool.s_size <- pool.s_size + 1
  in
  parse_stanzas ~start ~field ~finish st;
  Common.stop_parsing info

(****)

let package_name pool n =
  let p = Extarray.get pool.packages_by_num n in
  Dict.of_id !dict p.package

let print_pack pool ch n =
  let p = Extarray.get pool.packages_by_num n in
  Format.fprintf ch "%s (= %a)"
    (Dict.of_id !dict p.package) print_version p.version

let print_pack_name pool ch n = Format.fprintf ch "%s" (package_name pool n)

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
    R_conflict of int * int * (int * package_name dep) option
  | R_depends of int * package_name dep

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
      find_provided_packages pool n
  | Some (rel, vers) ->
      List.filter
        (fun p -> filter_rel rel (compare_version p.version vers))
        (Extarray.get_list pool.packages_by_name n)

let resolve_package_dep pool d =
  List.map (fun p -> p.num) (resolve_package_dep_raw pool d)

let dep_can_be_satisfied pool (n, cstr) =
  match cstr with
    None ->
      package_is_provided pool n
  | Some (rel, vers) ->
      List.exists
        (fun p -> filter_rel rel (compare_version p.version vers))
        (Extarray.get_list pool.packages_by_name n)

let single l =
  match l with
    [x] -> x
  | _   -> assert false

let generate_rules pool =
  let st = Common.start_generate (not !print_rules) pool.size in
  let pr = Solver.initialize_problem ~print_var:(print_pack pool) pool.size in
  (* Cannot install two packages with the same name *)
  Extarray.iter
    (fun l -> add_conflict pr None (List.map (fun p -> p.num) l))
    pool.packages_by_name;
  iter_packages pool
    (fun p ->
       Common.generate_next st;
       if !print_rules then
         Format.eprintf "%s %a@."
           (Dict.of_id !dict p.package) print_version p.version;
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
         (fun cstr ->
            List.iter
              (fun n -> add_conflict pr (Some (p.num, [cstr])) [p.num; n])
              (normalize_set (resolve_package_dep pool cstr)))
         c;
       let c = List.map (fun p -> single p) p.breaks in
       List.iter
         (fun cstr ->
            List.iter
              (fun n -> add_conflict pr (Some (p.num, [cstr])) [p.num; n])
              (normalize_set (resolve_package_dep pool cstr)))
         c);
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
    let p = Extarray.get pool.packages_by_num n in
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
      (fun cstr ->
         List.iter
           (fun n -> add_conflict pr (Some (p.num, [cstr])) [p.num; n])
           (normalize_set (resolve_package_dep pool cstr)))
      c;
    let c = List.map (fun p -> single p) p.breaks in
    List.iter
      (fun cstr ->
         List.iter
           (fun n -> add_conflict pr (Some (p.num, [cstr])) [p.num; n])
           (normalize_set (resolve_package_dep pool cstr)))
      c
  done;
  (* Cannot install two packages with the same name *)
  Extarray.iter (fun l -> add_conflict pr None (List.map (fun p -> p.num) l))
    pool.packages_by_name;
  Solver.propagate pr;
  pr

(****)

let parse_package_dependency pool s =
  let st = from_string s in
  let d = parse_package_dep "" true st in
  if not (at_eof st) then
    fail st (Format.sprintf "bad character '%c'" (next st));
  resolve_package_dep pool d

let parse_package_name pool s =
  List.map (fun p -> p.num)
    (Extarray.get_list pool.packages_by_name (Dict.add !dict s))

let parse_version s =
  let st = from_string s in
  skip_whitespaces st;
  let v = parse_version st in
  skip_whitespaces st;
  if not (at_eof st) then
    fail st (Format.sprintf "bad character '%c'" (next st));
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

let print_package_ref pr ch (p, v) =
  pr ch p;
  match v with
    None ->
      ()
  | Some (rel, vers) ->
      Format.fprintf ch " (%a %a)" print_rel rel print_version vers

let rec print_package_disj pr ch l =
  match l with
    []     -> ()
  | [p]    -> print_package_ref pr ch p
  | p :: r -> print_package_ref pr ch p; Format.fprintf ch " | ";
              print_package_disj pr ch r

let print_package_dependency ch l =
  let pr ch nm = Format.fprintf ch "%s" nm in
  Util.print_list (print_package_disj pr) ", " ch l

let check pool st =
  let assign = Solver.assignment st in
  Array.iteri
    (fun i v ->
       if v = Solver.True then begin
         let p = Extarray.get pool.packages_by_num i in
         Format.printf "Package: %a@." (print_pack pool) i;
         (* XXX No other package of the same name *)
         List.iter
           (fun p ->
              if p.num <> i && assign.(p.num) = Solver.True then begin
                Format.eprintf "PACKAGE %a ALSO INSTALLED!@."
                  (print_pack pool) p.num;
                exit 1
              end)
           (Extarray.get_list pool.packages_by_name p.package);
         let pr_pkg ch nm = Format.fprintf ch "%s" (Dict.of_id !dict nm) in
         if p.depends <> [] then begin
           Format.printf "Depends: ";
           List.iter
             (fun l ->
                Format.printf "%a " (print_package_disj pr_pkg) l;
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
                Format.printf "%a " (print_package_disj pr_pkg) l;
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
                Format.printf "%a " (print_package_disj pr_pkg) l;
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
                Format.printf "%a " (print_package_disj pr_pkg) l;
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
    let pr_pkg ch nm = Format.fprintf ch "%s" (Dict.of_id !dict nm) in
    List.iter
      (fun r ->
         match r with
           R_conflict (n1, n2, _) ->
             Format.printf "  %a conflicts with %a@."
               (print_pack pool) n1 (print_pack pool) n2
         | R_depends (n, l) ->
             Format.printf "  %a depends on %a %a@."
               (print_pack pool) n (print_package_disj pr_pkg) l
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
  iter_packages pool
    (fun p ->
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
                   (p.breaks @ p.conflicts)))));
  Array.init pool.size (fun i -> ListTbl.find conflicts i)

let compute_deps dist =
  Array.init dist.size (fun i ->
    let p = Extarray.get dist.packages_by_num i in
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
  Extarray.iter
    (fun l ->
       let l =
         List.sort (fun p1 p2 -> - compare_version p1.version p2.version) l in
       insert_package pool {(List.hd l) with num = pool.size})
    pool'.packages_by_name;
  pool

let copy pool =
  { size = pool.size;
    packages_by_name = Extarray.copy pool.packages_by_name;
    packages_by_num = Extarray.copy pool.packages_by_num;
    provided_packages = Extarray.copy pool.provided_packages }

let merge pool filter pool' =
  iter_packages pool'
    (fun p -> if filter p then insert_package pool {p with num = pool.size})

let add_package pool p =
  insert_package pool {p with num = pool.size};
  (List.find (fun q -> compare_version q.version p.version = 0)
    (find_packages_by_name pool p.package)).num

let src_only_latest h =
  let h' = new_src_pool () in
  Extarray.iter
    (fun l ->
       let l = List.filter (fun s -> not s.s_extra_source) l in
       let l =
         List.sort (fun s1 s2 -> - compare_version s1.s_version s2.s_version) l
       in
       match l with
         []     -> ()
       | s :: _ -> Extarray.add_to_list h'.s_packages s.s_name s)
    h.s_packages;
  h'

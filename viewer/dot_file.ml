(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type kind = [`Graph | `Digraph]

type attr_type = [`Graph | `Edge | `Node]

type attributes = (string * string) list

type node = { name : string; port : string option }

type graph = { graph_name : string option; body : statement list }

and compound = [`Node of node | `Graph of graph] list * attributes

and statement =
  [`Attributes of attr_type * attributes | `Compound of compound]

type t = { kind : kind; strict : bool; graph : graph }

(****)

let graph' ?name body = { graph_name = name; body = body }
let graph kind ?(strict = false) name body =
   { kind = kind; strict = strict; graph = graph' ~name body }
let subgraph ?name body = `Graph (graph' ?name body)
let node ?port name = `Node { name = name; port = port }

(****)

let name_or_number_re =
  Str.regexp
    "^\\([a-zA-Z_][a-zA-Z_0-9]*\\|-?[0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)$"
let keywords = ["node"; "edge"; "graph"; "digraph"; "subgraph"; "strict"]
let backslash_re = Str.regexp "\\\\"
let quote_re = Str.regexp "\""
let escape_string s =
  Str.global_replace quote_re "\\\""
    (Str.global_replace backslash_re "\\\\\\\\" s)

let need_quotes s =
  not (Str.string_match name_or_number_re s 0) ||
  List.mem (String.lowercase s) keywords

let print_atom f atom =
  if need_quotes atom then
    Format.fprintf f "\"%s\"" atom
  else
    Format.fprintf f "%s" atom 

let print_node f node =
  match node.port with
    None      -> print_atom f node.name
  | Some port -> Format.fprintf f "%a:%s" print_atom node.name port

let print_attribute f (key, value) =
  Format.fprintf f "@[<2>%a=@,%a@]" print_atom key print_atom value

let print_list pr f l =
  match l with
    []     -> ()
  | x :: r -> pr f x; List.iter (fun x -> Format.fprintf f "@ %a" pr x) r

let print_attributes f (typ, l) =
  Format.fprintf f "@[%s@ [@;<0 2>@["
    (match typ with `Graph -> "graph" | `Edge -> "edge" | `Node -> "node");
  print_list print_attribute f l;
  Format.fprintf f "@]@,]@]"

let rec print_simple kind f s =
  match s with
    `Node node   -> print_node f node
  | `Graph graph -> print_subgraph kind f graph

and print_subgraph kind f graph =
  Format.fprintf f "@[@[";
  begin match graph.graph_name with
    None -> ()
  | Some n -> Format.fprintf f "subgraph@ %a@ " print_atom n
  end;
  Format.fprintf f "{@]@;<1 2>@[";
  print_list (print_statement kind) f graph.body;
  Format.fprintf f "@]@ }@]"

and print_statement kind f st =
  match st with
    `Attributes attrs -> print_attributes f attrs
  | `Compound c       -> print_compound kind f c

and print_compound kind f (c, l) =
  let sep = match kind with `Graph -> "--" | `Digraph -> "->" in
  Format.fprintf f "@[@[";
  begin match c with
    [] ->
      assert false
  | s :: r ->
      print_simple kind f s;
      List.iter
        (fun s -> Format.fprintf f "@ %s@ %a" sep (print_simple kind) s) r
  end;
  Format.fprintf f "@]";
  if l <> [] then begin
    Format.fprintf f "@ [@;<0 2>@[";
    print_list print_attribute f l;
    Format.fprintf f "@]@,]"
  end;
  Format.fprintf f "@]"

let print f g =
  Format.fprintf f "@[@[%s%s@ %a@ {@]@;<1 2>@["
    (if g.strict then "strict " else "")
    (if g.kind = `Digraph then "digraph" else "graph")
    print_atom (match g.graph.graph_name with Some nm -> nm | None -> "graph");
  print_list (print_statement g.kind) f g.graph.body;
  Format.fprintf f "@]@ }@]@."

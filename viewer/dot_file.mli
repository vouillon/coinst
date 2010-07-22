
type kind = [`Graph | `Digraph]

type attr_type = [`Graph | `Edge | `Node]

type attributes = (string * string) list

type node = { name : string; port : string option }

type graph = { graph_name : string option; body : statement list }

and compound = [`Node of node | `Graph of graph] list * attributes

and statement =
  [`Attributes of attr_type * attributes | `Compound of compound]

type t = { kind : kind; strict : bool; graph : graph }

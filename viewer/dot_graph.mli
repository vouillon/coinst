
module StringMap : Map.S with type key = string
module IntMap : Map.S with type key = int

type id

type 'a sequence =
  { mutable count : int;
    mutable seq : 'a IntMap.t;
    id : (id, 'a) Hashtbl.t }

type node =
  { name : string;
    id : id;
    mutable node_attr : string StringMap.t }

type edge =
  { head : node;
    tail : node;
    edge_id : id;
    mutable edge_attr : string StringMap.t }

type graph =
  { graph_id : id;
    graph_name : string option;
    mutable graph_attr : string StringMap.t;
    subgraphs : graph sequence;
    nodes : node sequence;
    edges : edge sequence;
    parents : (id, graph) Hashtbl.t }

type info =
  { kind : [`Graph | `Digraph];
    strict : bool }

val of_file_spec : Dot_file.t -> info * graph

val of_channel : in_channel -> info * graph

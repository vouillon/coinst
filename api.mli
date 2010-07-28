
module type SOLVER = sig

  type state
  type var = int

  val reset : state -> unit

  val solve : state -> var -> bool
  val solve_lst : state -> var list -> bool

  type reason
  val collect_reasons : state -> var -> reason list
  val collect_reasons_lst : state -> var list -> reason list

end

module type S = sig

  type pool
  type reason

  module Solver : SOLVER with type reason = reason

  val new_pool : unit -> pool

  val parse_packages : pool -> string list -> in_channel -> unit

  val parse_package_dependency : pool -> string -> int list
  val parse_package_name : pool -> string -> int list

  val print_pack : pool -> Format.formatter -> int -> unit
  val print_pack_name : pool -> Format.formatter -> int -> unit

  val show_reasons : pool -> reason list -> unit
  val conflicts_in_reasons : reason list -> (int * int) list

  val generate_rules : pool -> Solver.state

  val compute_conflicts : pool -> int list array
  val compute_deps : pool -> int list list array

  val pool_size : pool -> int

end

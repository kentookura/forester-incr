(* Nothing of note in here yet.

   Eventual goal is code that understand how a change in one tree propagates
   through the forest.
*)

open Forester_frontend
open Forester_core
open Query
module Incr = Current_incr
module Gph = Graph.Imperative.Digraph.Concrete (Addr)

let reachable addr = rel Paths Incoming Rel.transclusion addr
let get_tree = Addr_map.find_opt

let get_sorted_trees addrs (forest : Forest.forest) : Sem.tree list =
  let find addr =
    match get_tree addr forest.trees with None -> [] | Some doc -> [ doc ]
  in
  Sem.Util.sort @@ List.concat_map find @@ Addr_set.elements addrs

module Ev = Eval.Make ()

let get_trees_from_query query = get_sorted_trees @@ Ev.run_query query

let need_rebuild (updated : addr) forest =
  get_trees_from_query (reachable updated) forest

type changeable_forest = Sem.tree Incr.t Addr_map.t

let to_changeable = Addr_map.map (fun t -> Incr.of_var @@ Incr.var t)

let f addr (forest : Forest.forest) =
  let rebuild = need_rebuild addr forest in
  let c_forest = to_changeable forest.trees in
  let trees = Addr_map.map (fun t -> Incr.observe t) c_forest in
  let new_forest = { forest with trees } in
  ()

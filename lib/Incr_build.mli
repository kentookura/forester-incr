open Forester_core
open Forester_frontend

type t

val const : t -> t Current_incr.t

module Incr = Current_incr

val get_tree : Addr_map.key -> 'a Addr_map.t -> 'a option
val get_sorted_trees : Addr_set.t -> Forest.forest -> Sem.tree list

module Renderer : sig
  val eval_tree :
    addr:addr ->
    source_path:string option ->
    Syn.tree ->
    Sem.tree * Sem.tree list

  val run_query : addr Query.t -> Addr_set.t
end

(* val get_trees_from_query : addr Query.t -> Forest.forest -> Sem.tree list *)

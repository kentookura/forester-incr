(* Nothing of note in here yet. *)

open Forester_frontend
open Forester_core
open Query
module Incr = Current_incr

type t = Syn.tree Incr.t

let const : t -> t Incr.t = Incr.const

module Query_graph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (Addr)
    (struct
      type t = addr Query.t

      let compare = compare
      let default = union []
    end)

module Renderer : sig
  val eval_tree :
    addr:addr ->
    source_path:string option ->
    Syn.tree ->
    Sem.tree * Sem.tree list

  val run_query : addr Query.t -> Addr_set.t
end = struct
  include Eval.Make ()
  open Forest

  let initial_plant : raw_forest -> forest = plant_forest

  let build_query_graph (trees : Sem.tree list) =
    let open Sem in
    let query_graph = Query_graph.create () in
    let analyse_tree roots (tree : Sem.tree) =
      let roots = List.map (fun tree -> tree.fm.addr) trees :: roots in
      tree.fm.addr |> Query_graph.add_vertex query_graph;

      tree.body
      |> List.iter @@ fun node ->
         match Asai.Range.(node.value) with
         | Query_tree (opts, q) ->
             let matches = run_query q in
             matches
             |> Addr_set.iter (fun a ->
                    let edge : Query_graph.edge = (tree.fm.addr, q, a) in
                    Query_graph.add_edge_e query_graph edge)
         | _ -> ()
    in
    trees |> List.iter (analyse_tree []);
    query_graph

  let build_query_trie (trees : Sem.tree list) : Addr_set.t Query_trie.t =
    let open Sem in
    let query_trie = Query_trie.empty in
    let analyse_tree (tree : Sem.tree) =
      ( List.fold_right @@ fun node map ->
        match Asai.Range.(node.value) with
        | Query_tree (opts, q) ->
            let matches = run_query q in
            Query_trie.update q (Option.map (fun x -> x)) map
        | _ -> map )
        tree.body query_trie
    in
    List.fold_right
      (fun tree trie ->
        let trie' = analyse_tree tree in
        Query_trie.union_with Addr_set.union trie' trie)
      trees query_trie

  (* NOTE:
     To compute the set of nodes affected by a change to t, take a union of
     succs and preds of all relations.
  *)
end

(* let dirties : addr -> Addr_set.t Query_trie.t -> Addr_set.t = fun _ -> () *)
let get_tree = Addr_map.find_opt

let get_sorted_trees addrs (forest : Forest.forest) : Sem.tree list =
  let find addr =
    match get_tree addr forest.trees with None -> [] | Some doc -> [ doc ]
  in
  Sem.Util.sort @@ List.concat_map find @@ Addr_set.elements addrs

(* let get_trees_from_query query = get_sorted_trees @@ Ev.run_query query *)

(* let need_rebuild (updated : addr) forest = *)
(*   get_trees_from_query (reachable updated) forest *)

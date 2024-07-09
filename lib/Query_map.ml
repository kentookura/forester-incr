open Forester_core
open Query

(* I was inspired to play around with the ideas in
    https://arxiv.org/pdf/2302.08775.

   I was thinking about incrementally rendering forests. The question is: Which
   changes affect which trees?

   Let S_addr be the set of trees changeable by modifying tree at `addr`. Then
   { tree with transclusion path to `addr` } ⊂ S_addr, since S would need to be
   rerendered.

   However, trees which query a forest are more tricky. How can we tell if an
   edit action on a tree causes it to be matched/dropped by a query? If I
   change the taxon from t_1 to t_2, all trees which query for t_1 and t_2 must
   be rerendered.

   I thought that it might be convenient to have a datastructure for storing
   query results that is indexed by the type of queries itself. While I
   succeeded in creating this datastructure (so far with just a lookup
   function), it did open up a new issue, which I now illustrate:

   The constructors Isect and Union store a list of queries. The search result
   is independent of the list order. That's why I used a map indexed by
   query-sets instead:

   lookup (Isect [q1; q2]) == lookup (Isect [q2; q1])

   What if we wish to take equivalence of queries more seriously? Normal form?
   Confluence? Use some theorems about boolean algebra? Some stuff along these
   lines is discussed in the paper in the pattern matching section.

   Leaving these questions aside for a moment, it is easy to see the appeal of
   this:

   * First, we could write algorithms to incrementally render forests,
   combining the structures and functions in this file with the query engine..
   In a hypermedia environment, we could just rerender the currently viewed
   tree, but if we incorporate incrementality into static rendering, we need to
   make sure everything that needs rerendering actually gets rerendered.

   * We could build a pretty sophisticated search engine using this. When
   computing new query results, we could combine existing cached results with
   fresh search results, or even avoid searching the forest by simply combining
   existing results.
*)

(* Boilerplate, skip this. *)

type query = addr Query.t

module Query_map = Map.Make (struct
  type t = query

  let compare = compare
end)

module Rel_query_map = Map.Make (struct
  type t = Query.rel_query

  let compare = compare
end)

module Query_set = Set.Make (struct
  type t = query

  let compare = compare
end)

module Relation_set = Set.Make (struct
  type t = rel

  let compare = compare
end)

(* TODO: *)
module Query_set_map = Map.Make (struct
  type t = Query_set.t

  let compare = compare
end)

type 'a tf = 'a option -> 'a option

(* The core type. *)
module Query_trie : sig
  type key
  type 'a t

  val empty : 'a t
  val lookup : key -> 'a t -> 'a option
  val update : key -> 'a tf -> 'a t -> 'a t
  val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val cardinal : 'a t -> int
end = struct
  type key = query

  type 'a t =
    | Empty
    | QM of {
        rel : 'a Addr_map.t Rel_query_map.t;
        isect : 'a Query_set_map.t;
        union : 'a Query_set_map.t;
        complement : 'a t;
        isect_fam : 'a Rel_query_map.t Query_map.t;
        union_fam : 'a Rel_query_map.t Query_map.t;
      }

  let empty = Empty

  let rec lookup : type a. query -> a t -> a option =
   fun q qm ->
    match qm with
    | Empty -> None
    | QM { rel; isect; union; complement; isect_fam; union_fam } -> (
        match Forester_core.Query.view q with
        | Rel (q, a) ->
            Option.bind (Rel_query_map.find_opt q rel) (Addr_map.find_opt a)
        (* TODO: isect and union should not be sensitive to list order. That's
           why we store with key type of set*)
        | Isect qs -> Query_set_map.find_opt (Query_set.of_list qs) isect
        | Union qs -> Query_set_map.find_opt (Query_set.of_list qs) union
        | Complement q -> lookup q complement
        | Isect_fam (q, v) ->
            Option.bind
              (Query_map.find_opt q isect_fam)
              (Rel_query_map.find_opt v)
        | Union_fam (q, v) ->
            Option.bind
              (Query_map.find_opt q isect_fam)
              (Rel_query_map.find_opt v))

  type 'a tf = 'a option -> 'a option

  let lift_tf f tf =
    match tf with Some m -> Some (f m) | None -> Some (f Empty)

  let rec update : type a. query -> a tf -> a t -> a t =
   fun q tf qm ->
    match qm with
    | Empty -> Empty
    | QM ({ rel; isect; union; complement; isect_fam; union_fam } as m) -> (
        match view q with
        | Rel (relq, addr) ->
            QM
              {
                m with
                rel =
                  Rel_query_map.update relq
                    (Option.map (Addr_map.update addr tf))
                    rel;
              }
        | Isect qs ->
            QM
              {
                m with
                isect = Query_set_map.update (Query_set.of_list qs) tf isect;
              }
        | Union qs ->
            QM
              {
                m with
                union = Query_set_map.update (Query_set.of_list qs) tf union;
              }
        | Complement query -> update query tf complement
        | Isect_fam (query, rel_query) ->
            QM
              {
                m with
                isect_fam =
                  Query_map.update query
                    (Option.map (Rel_query_map.update rel_query tf))
                    isect_fam;
              }
        | Union_fam (query, rel_query) ->
            QM
              {
                m with
                union_fam =
                  Query_map.update query
                    (Option.map (Rel_query_map.update rel_query tf))
                    union_fam;
              })

  let insert : key -> 'a -> 'a t -> 'a t = fun k v -> update k (fun _ -> Some v)
  let delete : key -> 'a t -> 'a t = fun k -> update k (fun _ -> None)

  let rec fold : 'a 'acc. ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc =
   fun f m acc ->
    match m with
    | Empty -> acc
    | QM { rel; isect; union; complement; isect_fam; union_fam } ->
        (* The idea is to fold each map, calling `fold` and `M.fold` with `M`
           in {Query_set_map, Rel_query_map, Query_map}. I don't know yet if
           this implementation is correct.
        *)
        let acc =
          (* Ignoring first argument because of mismatch of signature of
             M.fold : (key -> 'a -> 'acc -> 'acc) -> 'a M.t -> 'acc -> 'acc
             with the fold in the paper
             fold : ('a -> 'acc -> 'acc) -> 'a M.t -> 'acc -> 'acc
          *)
          Rel_query_map.fold
            (fun _ adrm acc' -> Addr_map.fold (fun _ -> f) adrm acc')
            rel acc
        in
        let acc = Query_set_map.fold (fun _ -> f) isect acc in
        let acc = Query_set_map.fold (fun _ -> f) union acc in
        let acc = fold f complement acc in
        let acc =
          Query_map.fold
            (fun _ rqm acc' -> Rel_query_map.fold (fun _ -> f) rqm acc')
            isect_fam acc
        in
        let acc =
          Query_map.fold
            (fun _ rqm acc' -> Rel_query_map.fold (fun _ -> f) rqm acc')
            union_fam acc
        in
        acc

  let cardinal : 'a t -> int = fun m -> fold (fun _ n -> n + 1) m 0
  let elems : 'a t -> 'a list = fun m -> fold List.cons m []
end

module S = Relation_set

(* The set of relations in a query. We might want something more fine grained*)
let rec relations (q : query) : S.t =
  match view q with
  | Rel ((_, _, rel), addr) -> S.singleton rel
  | Isect qs | Union qs ->
      List.fold_left S.union S.empty (List.map relations qs)
  | Complement q -> relations q
  | Isect_fam (query, (_, _, rel)) | Union_fam (query, (_, _, rel)) ->
      S.union (S.singleton rel) (relations query)

(* One for each builtin relation. Expand on this if you want to create a
   tree-editor.*)
type edit_actions =
  | Links
  | Transclusion
  | Authors
  | Contributors
  | Taxa
  | Tags

(* If a tree is matched by `q` and is edited, but the edit action can't change
   the relations in q, then the trees that query only along only this relation
   will not need to be rerendered.*)

let can_change_relations edit_actions rels =
  match edit_actions with
  | Links -> S.mem Rel.links rels
  | Transclusion -> S.mem Rel.transclusion rels
  | Authors -> S.mem Rel.authors rels
  | Contributors -> S.mem Rel.contributors rels
  | Taxa -> S.mem Rel.taxa rels
  | Tags -> S.mem Rel.tags rels

let can_change_query_result edit_actions query =
  can_change_relations edit_actions (relations query)

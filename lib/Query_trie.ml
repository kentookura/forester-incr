open Forester_core
(** This module is broken since the addition of closures. I need to think about how to proceed.
    Should I just use Eval.inst_qclo?
 *)

module Query_map = Map.Make (struct
  type t = Sem.query

  let compare = compare
end)

module Rel_query_map = Map.Make (struct
  type t = Query.rel_query

  let compare = compare
end)

module Query_set = Set.Make (struct
  type t = Sem.query

  let compare = compare
end)

(*  FIXME: Is a set map fine? or should we make another trie? *)
module Query_set_map = Map.Make (struct
  type t = Query_set.t

  let compare = compare
end)

(* type 'a tf = 'a option -> 'a option *)
(* type key = Sem.query *)

(* Each constructor becomes a field.
   Each field becomes a nested map.
   Maybe it is possible to make this into a library using repr.
*)

(*
module Closure_map = struct
  type 'a t = Empty | CM of { clo : Query_map.t; rel : Query_map.t }
end

type 'a t =
  | Empty
  | QM of {
      rel : 'a Addr_map.t Rel_query_map.t;
      isect : 'a Query_set_map.t;
      union : 'a Query_set_map.t;
      complement : 'a t;
      isect_fam : 'a Closure_map.t Query_map.t;
      union_fam : 'a Closure_map.t Query_map.t;
    }

let empty = Empty

let rec lookup : type a. Sem.query -> a t -> a option =
 fun q qm ->
  match qm with
  | Empty -> None
  | QM { rel; isect; union; complement; isect_fam; union_fam } -> (
      match q with
      | Rel (q, a) ->
          Option.bind (Rel_query_map.find_opt q rel) (Addr_map.find_opt a)
      (* NOTE: isect and union should not be sensitive to list order. That's
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

let lift_tf f tf = match tf with Some m -> Some (f m) | None -> Some (f Empty)

let rec update : type a. query -> a tf -> a t -> a t =
 fun q tf qm ->
  match qm with
  | Empty -> Empty
  | QM ({ rel; isect; union; complement; isect_fam; union_fam } as m) -> (
      match q with
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
      (* NOTE: The idea is to fold each map, calling `fold` and `M.fold` with
         `M` in {Query_set_map, Rel_query_map, Query_map}. I don't know yet
         if this implementation is correct.
      *)
      let acc =
        (* NOTE: Ignoring first argument because of mismatch of signature of
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

let rec union_with : 'a. ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t =
 fun f m1 m2 ->
  match (m1, m2) with
  | QM r1, QM r2 ->
      let rel =
        Rel_query_map.union
          (fun _ m1 m2 ->
            Some (Addr_map.union (fun _ s t -> Some (f s t)) m1 m2))
          r1.rel r2.rel
      in
      let isect =
        Query_set_map.union (fun _ m1 m2 -> Some (f m1 m2)) r1.isect r2.isect
      in
      let union =
        Query_set_map.union (fun _ m1 m2 -> Some (f m1 m2)) r1.union r2.union
      in
      let complement = union_with f r1.complement r2.complement in
      let isect_fam =
        Query_map.union
          (fun _ m1 m2 ->
            Some (Rel_query_map.union (fun _ s t -> Some (f s t)) m1 m2))
          r1.isect_fam r2.isect_fam
      in
      let union_fam =
        Query_map.union
          (fun _ m1 m2 ->
            Some (Rel_query_map.union (fun _ s t -> Some (f s t)) m1 m2))
          r1.union_fam r2.union_fam
      in
      QM { rel; isect; union; complement; isect_fam; union_fam }
  | qm, Empty -> qm
  | Empty, qm -> qm
*)

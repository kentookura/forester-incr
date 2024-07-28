(* In this module I'll try to actually compute the set of trees affected by a
   change. *)

open Forester_core
open Query

module S = Set.Make (struct
  type t = rel

  let compare = compare
end)

(* The set of relations in a query. We might want something more fine grained *)
let rec relations (q : Sem.query) : S.t =
  match q with
  | Rel ((_, _, rel), addr) -> S.singleton rel
  | Isect qs | Union qs ->
      List.fold_left S.union S.empty (List.map relations qs)
  | Complement q -> relations q
  | Isect_fam (query, QClo_rel (_, _, rel))
  | Union_fam (query, QClo_rel (_, _, rel)) ->
      S.union (S.singleton rel) (relations query)
  (* TODO: I put S.empty to compile, I have to come back to this*)
  | Isect_fam (query, Sem.QClo (_, _, _)) -> S.empty
  | Union_fam (query, Sem.QClo (_, _, _)) -> S.empty

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

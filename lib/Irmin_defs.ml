(* https://github.com/mirage/irmin/issues/909 *)
(* https://github.com/ocaml/dune/blob/a3a5bfcecae994509b2cb5e283e9569956244785/doc/dev/cache.md *)
type ('a, 'b) content = Artifact of 'a | Value of 'b

type forest_content =
  (Forester_core.Xml_tree.tree_, Forester_core.Code.tree) content

(* Although we store the xml format, we won't be able to use this module for
   remote forestry, as the non-git filesystem backend does not support
   syncing.*)

module Content : Irmin.Contents.S with type t = forest_content = struct
  type t = forest_content

  let t : t Repr.ty =
    let open Repr in
    variant "forest_content" (fun artifact value -> function
      | Artifact x -> artifact x | Value x -> value x)
    |~ case1 "Artifact" Forester_core.Xml_tree.tree__t (fun x -> Artifact x)
    |~ case1 "Value" Forester_irmin.Code.tree (fun x -> Value x)
    |> sealv

  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end

module Cache = Irmin_fs_unix.KV.Make (Content)
module Cache_info = Irmin_unix.Info (Cache.Info)

let info message = Cache_info.v "%s" message

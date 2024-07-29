open Forester_core
open Forester_frontend
open Irmin_defs

module S = Algaeff.Sequencer.Make (struct
  type t = Eio.Fs.dir_ty Eio.Path.t
end)

module Cache_handle = Algaeff.Reader.Make (struct
  type t = Cache.t
end)

module Oper = Graph.Oper.Make (Graph.Builder.I (Import_graph.Gph))

let rec process_file fp =
  if Eio.Path.is_directory fp then process_dir fp
  else
    Eio.Path.split fp
    |> Option.iter @@ fun (_, basename) ->
       if
         Filename.extension basename = ".tree"
         && (not @@ String.starts_with ~prefix:"." basename)
       then S.yield fp

and process_dir dir =
  try
    Eio.Path.read_dir dir
    |> List.iter @@ fun fp -> process_file Eio.Path.(dir / fp)
  with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let scan_directories dirs =
  S.run @@ fun () -> dirs |> List.iter @@ fun fp -> process_dir fp

let addr_of_fs_path : Eio.Fs.dir_ty Eio.Path.t -> addr =
 fun path ->
  match Eio.Path.split path with
  | None ->
      Reporter.fatalf Internal_error
        "could not make addres from path %a because splitting the path failed."
        Eio.Path.pp path
  | Some (_, base) -> User_addr (Filename.chop_extension base)

let store_path_of_addr : addr -> Cache.Path.t =
 fun addr -> [ Format.asprintf "%a" pp_addr addr ]

let store_path_of_content : forest_content -> Cache.Path.t = function
  | Value tree -> (
      match tree.addr with
      | Some addr -> [ "content"; addr ]
      | None -> Reporter.fatalf Internal_error "tree has no address")
  | Artifact xml -> (
      match xml with
      | Xml_tree.Tree { frontmatter = { addr = Some a; _ }; _ } ->
          [ "value"; Format.asprintf "%a" pp_addr a ]
      | _ -> Reporter.fatalf Internal_error "tree has no address")

let store_path_of_path p = p |> addr_of_fs_path |> store_path_of_addr
let user_addr addr = User_addr addr

let pp_store_path ppf segments =
  let pp_sep ppf () = Format.fprintf ppf "/@" in
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep pp_print_string)
    segments

let pp_path = Eio.Path.pp

type 'a status = Changed of 'a | Unchanged of 'a

let partition_by_status p l =
  let rec part left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l -> (
        match p x with
        | Changed v -> part (v :: left) right l
        | Unchanged v -> part left (v :: right) l)
  in
  part [] [] l

let changed_content path =
  let cache = Cache_handle.read () in
  let store_path = path |> addr_of_fs_path |> store_path_of_addr in
  let code =
    match Parse.parse_file path with
    | Error _ ->
        Reporter.fatalf Parse_error "failed to parse tree at %a" pp_path path
    | Ok code -> code
  in
  let source_path = Option.map Unix.realpath @@ Eio.Path.native path in
  let addr =
    match Eio.Path.split path with
    | None -> None
    | Some (_, base) -> Some (Filename.chop_extension base)
  in
  let tree = Code.{ source_path; addr; code } in
  let fresh_hash = Cache.Backend.Contents.Hash.hash @@ Value tree in
  match Cache.hash cache store_path with
  | Some stored_hash ->
      if stored_hash = fresh_hash then Unchanged tree else Changed tree
  | None -> Changed tree

let update_content path =
  let cache = Cache_handle.read () in
  match changed_content path with
  | Unchanged _ -> Ok ()
  | Changed tree ->
      Reporter.tracef "updating %a" pp_store_path (store_path_of_path path)
      @@ fun () ->
      let v = Value tree in
      let path = store_path_of_content v in
      Cache.set ~info:(info "foo") cache path v

let get_value name =
  let cache = Cache_handle.read () in
  let path = [ "values"; name ] in
  Cache.get cache path |> function
  | Value v -> v
  | Artifact _ ->
      Reporter.fatalf Internal_error
        "got artifact when looking up store path %a. This should never happen! \
         There is a bug in the code which stores artifacts!"
        pp_store_path path

let get_artifact name =
  let cache = Cache_handle.read () in
  let path = [ "artifacts"; name ] in
  Cache.get cache path |> function
  | Artifact a -> a
  | Value _ ->
      Reporter.fatalf Internal_error
        "got value when looking up store path %a. This should never happen! \
         There is a bug in the code which stores values!"
        pp_store_path path

let update_cache =
  let to_either = function
    | Ok a -> Either.Right a
    | Error e -> Either.Left e
  in
  fun dirs ->
    Reporter.tracef "when updating cache" @@ fun () ->
    scan_directories dirs |> List.of_seq
    |> List.map (fun p -> update_content p)
    |> List.partition_map to_either
    |> fun (errs, _) -> match errs with [] -> Ok () | _ :: _ -> Error errs

let changed_trees dirs =
  scan_directories dirs |> List.of_seq
  |> List.filter_map (fun path ->
         match changed_content path with
         | Changed tree -> Some tree
         | Unchanged _ -> None)

let trees_to_reevaluate dirs =
  let changed_trees, unchanged_trees =
    scan_directories dirs |> List.of_seq
    |> partition_by_status (fun path -> changed_content path)
  in
  let dependency_graph =
    Import_graph.build_import_graph (changed_trees @ unchanged_trees)
    |> Oper.transitive_closure
  in
  let dependents addr =
    Import_graph.Gph.fold_succ Addr_set.add dependency_graph addr Addr_set.empty
  in
  let changed_addrs =
    List.filter_map
      (fun tree -> Option.map user_addr Code.(tree.addr))
      changed_trees
  in
  List.fold_left
    (fun trees addr -> Addr_set.union (dependents addr) trees)
    Addr_set.empty changed_addrs

(* TODO: Here I would need to look up the set of trees with queries that match
   an addr in `reval`.
*)
let trees_to_rerender dirs =
  let reval = trees_to_reevaluate dirs in
  Addr_set.empty

open Forester_core
open Forester_frontend
open Irmin_defs

(* NOTE:
   Not every irmin store supports the Sync API. This means we can only use the
   store module defined in this file for the build cache. For remote forestry,
   we should fetch the compiled format.
*)

module S = Algaeff.Sequencer.Make (struct
  type t = Eio.Fs.dir_ty Eio.Path.t
end)

module Config = Algaeff.Reader.Make (struct
  type t = Forester_frontend.Config.Forest_config.t
end)

module Cache' = Algaeff.Reader.Make (struct
  type t = Cache.t
end)

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
  | Some (path, base) -> User_addr (Filename.chop_extension base)

let store_path_of_addr : addr -> Cache.Path.t =
 fun addr -> [ Format.asprintf "%a" pp_addr addr ]

let store_path_of_path p = p |> addr_of_fs_path |> store_path_of_addr
let user_addr addr = User_addr addr

let pp_store_path ppf segments =
  let pp_sep ppf () = Format.fprintf ppf "/@" in
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep pp_print_string)
    segments

let pp_path = Eio.Path.pp

open Code

module Status = struct
  type 'a t = Changed of 'a | Unchanged of 'a

  let partition_map p l =
    let rec part left right = function
      | [] -> (List.rev left, List.rev right)
      | x :: l -> (
          match p x with
          | Changed v -> part (v :: left) right l
          | Unchanged v -> part left (v :: right) l)
    in
    part [] [] l
end

open Status

let unchanged a = Status.Unchanged a
let changed a = Status.Changed a

(* Right if changed, Left if unchanged. We're still returning the unchanged tree to avoid reparsing*)
let changed_content path =
  let cache = Cache'.read () in
  let store_path = path |> addr_of_fs_path |> store_path_of_addr in
  let code =
    match Parse.parse_file path with
    | Error _ ->
        Reporter.fatalf Parse_error "failed to parse tree at %a" Eio.Path.pp
          path
    | Ok code -> code
  in
  let source_path = Option.map Unix.realpath @@ Eio.Path.native path in
  let addr =
    match Eio.Path.split path with
    | None -> None
    | Some (_, base) -> Some (Filename.chop_extension base)
  in
  let tree = Code.{ source_path; addr; code } in
  let fresh_hash = Cache.Backend.Contents.Hash.hash @@ value tree in
  match Cache.find cache store_path with
  | Some stored_code -> (
      match Cache.hash cache store_path with
      | Some stored_hash ->
          if stored_hash = fresh_hash then unchanged tree else changed tree
      | None -> changed tree)
  | None -> changed tree

let update_content path =
  let cache = Cache'.read () in
  match changed_content path with
  | Unchanged _ -> Ok ()
  | Changed tree ->
      Cache.set ~info:(info "foo") cache (store_path_of_path path) (value tree)

let update_changed cache = function
  | Unchanged tree -> Ok tree
  | Changed tree -> (
      match tree.addr with
      | None -> Reporter.fatalf Internal_error "tree has no address"
      | Some a ->
          Result.map (Fun.const tree)
          @@ Cache.set ~info:(info "foo") cache
               (store_path_of_addr (User_addr a))
               (value tree))

let update_and_return path =
  let cache = Cache'.read () in
  match changed_content path with
  | Changed tree -> tree
  | Unchanged tree ->
      let _ =
        Cache.set ~info:(info "foo") cache (store_path_of_path path)
          (value tree)
      in
      tree

let update_cache dirs =
  Reporter.emitf Profiling "updating cache";
  scan_directories dirs |> List.of_seq |> List.map @@ fun p -> update_content p

let get_updated dirs =
  scan_directories dirs |> List.of_seq
  |> List.map @@ fun p -> update_and_return p

let changed_trees dirs =
  scan_directories dirs |> List.of_seq
  |> List.filter_map (fun path ->
         match changed_content path with
         | Changed tree -> Some tree
         | Unchanged _ -> None)

let trees_to_reevaluate dirs =
  let changed_trees, unchanged_trees =
    scan_directories dirs |> List.of_seq
    |> Status.partition_map (fun path -> changed_content path)
  in
  let import_graph =
    Import_graph.build_import_graph (changed_trees @ unchanged_trees)
  in
  let deps addr =
    Import_graph.Gph.fold_pred Addr_set.add import_graph addr Addr_set.empty
  in
  let changed_addrs =
    List.filter_map (fun tree -> Option.map user_addr tree.addr) changed_trees
  in
  List.fold_left
    (fun trees addr -> Addr_set.union (deps addr) trees)
    (Addr_set.of_list changed_addrs)
    []

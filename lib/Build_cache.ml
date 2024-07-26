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

let addr_of_fs_path : Eio.Fs.dir_ty Eio.Path.t -> (addr, unit) Result.t =
 fun path ->
  match Eio.Path.split path with
  | None -> Error ()
  | Some (path, base) -> Ok (User_addr (Filename.chop_extension base))

let store_path_of_addr : addr -> Cache.Path.t =
 fun addr -> [ Format.asprintf "%a" pp_addr addr ]

let pp_store_path ppf segments =
  let pp_sep ppf () = Format.fprintf ppf "/@" in
  Format.fprintf ppf "%a"
    Format.(pp_print_list ~pp_sep pp_print_string)
    segments

let update_content_if_changed path cache =
  let store_path =
    match path |> addr_of_fs_path with
    | Ok addr -> addr |> store_path_of_addr
    | Error _ -> Reporter.fatalf Internal_error "failed to construct store path"
  in
  let fresh_hash, code =
    match Parse.parse_file path with
    | Error _ ->
        Reporter.fatalf Parse_error "failed to parse tree at %a" Eio.Path.pp
          path
    | Ok code -> (Cache.Backend.Contents.Hash.hash code, code)
  in
  match Cache.find cache store_path with
  | Some stored_code -> (
      match Cache.hash cache store_path with
      | Some stored_hash ->
          if stored_hash = fresh_hash then
            Reporter.emitf Profiling "tree at addr %a is up to date"
              pp_store_path store_path
          else
            Reporter.emitf Profiling "tree at addr %a is not up to date"
              pp_store_path store_path;
          let _ = Cache.set ~info:(info "foo") cache store_path code in
          ()
      | None ->
          Reporter.emitf Profiling "new tree at addr %a" pp_store_path
            store_path;
          let _ = Cache.set ~info:(info "foo") cache store_path code in
          ())
  | None ->
      Reporter.emitf Profiling "new tree read from %a" Eio.Path.pp path;
      let _ = Cache.set ~info:(info "foo") cache store_path code in
      ()

let iter_dirs dirs (f : Eio.Fs.dir_ty Eio.Path.t list -> 'a) =
  scan_directories dirs |> List.of_seq |> f

let update_cache dirs cache =
  iter_dirs dirs
    ( List.iter @@ fun p ->
      Reporter.tracef "when checking %a" Eio.Path.pp p @@ fun () ->
      update_content_if_changed p cache )

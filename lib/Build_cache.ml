open Lwt.Syntax

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

module Content : Irmin.Contents.S with type t = Forester_core.Code.t = struct
  open Irmin.Type

  type t = Forester_core.Code.t

  let t = Forester_irmin.Code.t
  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end

module Cache = Irmin_fs_unix.KV.Make (Content)

(* NOTE:
   Not every irmin store supports the Sync API. This means we can only use the
   store module defined in this file for the build cache. For remote forestry,
   we should fetch the compiled format.
*)

type foo = {
  contents : Forester_core.Code.t;
  last_changed : float;
  path : string option;
  addr : string;
}

let read_trees_in_dirs ~dev ?(_ignore_malformed = false) dirs
    (build_cache : Cache.t) =
  scan_directories dirs |> List.of_seq
  |> ( List.filter_map @@ fun fp ->
       match Eio.Path.split fp with
       | Some (_, basename) -> (
           let addr = Filename.chop_extension basename in
           let source_path =
             if dev then Option.map Unix.realpath @@ Eio.Path.native fp
             else None
           in
           let mtime = Eio.Path.(stat ~follow:false fp).mtime in
           match Forester_frontend.Parse.parse_file fp with
           | Ok code ->
               Some
                 {
                   contents = code;
                   last_changed = mtime;
                   path = source_path;
                   addr;
                 }
           | Error _ -> None)
       | None -> None )
  |> Lwt_list.iter_p (fun { contents; last_changed; path; addr } ->
         let hash = Cache.Backend.Contents.Hash.hash contents in
         let key = Cache.Contents.of_hash (Cache.repo build_cache) hash in
         let info () = Cache.Info.v 0L in
         Cache.set_exn build_cache [ "sources"; addr ] ~info contents)

let _make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)

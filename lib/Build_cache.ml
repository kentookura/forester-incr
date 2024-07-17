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

module Content : Irmin.Contents.S with type t = string = struct
  open Irmin.Type

  type t = string

  let t = string
  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end

module Store = Irmin_fs_unix.KV.Make (Content)

type foo = { contents : string; last_changed : float; path : string option }

let _read_trees_in_dirs ~dev ?(_ignore_malformed = false) dirs build_cache =
  scan_directories dirs |> List.of_seq
  |> ( List.filter_map @@ fun fp ->
       match Eio.Path.split fp with
       | Some (_, basename) ->
           let addr = Filename.chop_extension basename in
           let source_path =
             if dev then Option.map Unix.realpath @@ Eio.Path.native fp
             else None
           in
           let mtime = Eio.Path.(stat ~follow:false fp).mtime in
           Some
             {
               contents = Eio.Path.load fp;
               last_changed = mtime;
               path = source_path;
             }
       | None -> None )
  |> Lwt_list.iter_p (fun { contents; last_changed; path } ->
         let _ = Store.Backend.Contents.Hash.hash contents in
         let info () = Store.Info.v 0L in
         Store.set_exn build_cache [ "sources"; addr ] ~info tree)

let _make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)

let main _env =
  let config = Irmin_fs.config "build/cache" in
  let* repo = Store.Repo.v config in
  let* main = Store.main repo in
  let info () = Store.Info.v 0L in
  let key = "Hello" in
  let* () = Store.set_exn main [ key ] ~info "world!" in
  let* v = Store.get main [ key ] in
  print_endline v |> Lwt.return

let () = Eio_main.run @@ fun env -> Lwt_main.run @@ main env

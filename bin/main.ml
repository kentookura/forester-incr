open Forester_core
open Current_incr
open Forester_incr
open Forester_frontend
open Irmin_defs
open Build_cache
module Tty = Asai.Tty.Make (Forester_core.Reporter.Message)

let make_dir ~env dir = Eio.Path.(Eio.Stdenv.fs env / dir)
let make_dirs ~env = List.map (make_dir ~env)

let internal_config_from_config ~env
    (config : Forester_frontend.Config.Forest_config.t) =
  Forest.
    {
      env;
      root = config.root;
      assets_dirs = make_dirs ~env config.assets;
      theme_dir = make_dir ~env config.theme;
      stylesheet = config.stylesheet;
      ignore_tex_cache = false;
      no_assets = false;
      no_theme = false;
    }

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Forester_core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  Eio_main.run @@ fun env ->
  let cache_path = Eio.Path.(env#cwd / "build") in
  let forest_config =
    Forester_frontend.Config.parse_forest_config_file "forest.toml"
  in
  let internal_cfg =
    {
      (internal_config_from_config ~env forest_config) with
      no_assets = true;
      no_theme = true;
    }
  in
  let clock = Eio.Stdenv.clock env in
  let cache_config = Irmin_fs_unix.conf ~path:cache_path ~clock in
  let repo = Cache.Backend.Repo.v cache_config in
  let dirs = make_dirs ~env Config.(forest_config.trees) in
  let cache = Irmin_defs.Cache.main repo in

  Cache_handle.run ~env:cache @@ fun () ->
  let changed = changed_trees dirs in
  let open Code in
  let info =
    List.filter_map
      (fun tree ->
        match tree.addr with
        | Some addr ->
            Some Range.{ loc = None; value = Asai.Diagnostic.text addr }
        | None -> None)
      changed
  in
  Reporter.emitf Profiling ~extra_remarks:info
    "Trees that have changed since the last cache update:";

  let reval = trees_to_reevaluate dirs in
  let info =
    reval |> Addr_set.to_list
    |> List.map (fun addr ->
           Range.{ loc = None; value = Asai.Diagnostic.textf "%a" pp_addr addr })
  in
  Reporter.emitf Profiling ~extra_remarks:info
    "Trees that need to be reevaluated";

  let _ = update_cache dirs in
  let changed = changed_trees dirs in
  List.iter
    (fun tree ->
      match Code.(tree.addr) with
      | Some addr -> Format.printf "%s\n" addr
      | None -> ())
    changed

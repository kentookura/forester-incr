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
  let config =
    Forester_frontend.Config.parse_forest_config_file "forest.toml"
  in
  let internal_cfg =
    {
      (internal_config_from_config ~env config) with
      no_assets = true;
      no_theme = true;
    }
  in
  let clock = Eio.Stdenv.clock env in
  let config = Irmin_fs_unix.conf ~path:cache_path ~clock in
  let repo = Cache.Backend.Repo.v config in
  let dirs = [ Eio.Path.(Eio.Stdenv.cwd env / "trees") ] in
  let cache = Irmin_defs.Cache.main repo in

  Cache'.run ~env:cache @@ fun () ->
  let changed = changed_trees dirs in
  let open Code in
  List.iter
    (fun tree ->
      match tree.addr with Some addr -> Format.printf "%s\n" addr | None -> ())
    changed;
  let _ = update_cache dirs in
  let changed = changed_trees dirs in
  let open Code in
  List.iter
    (fun tree ->
      match tree.addr with Some addr -> Format.printf "%s\n" addr | None -> ())
    changed
(* let reval = trees_to_reevaluate dirs in *)
(* Addr_set.iter (fun addr -> Format.printf "%a\n" pp_addr addr) reval *)

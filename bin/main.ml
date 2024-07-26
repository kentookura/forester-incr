open Current_incr
open Forester_incr
open Forester_frontend
open Irmin_defs
module Tty = Asai.Tty.Make (Forester_core.Reporter.Message)

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Forester_core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  Eio_main.run @@ fun env ->
  let config_path = Eio.Path.(env#cwd / "build") in
  let clock = Eio.Stdenv.clock env in
  let config = Irmin_fs_unix.conf ~path:config_path ~clock in
  let repo = Cache.Backend.Repo.v config in
  let dirs = [ Eio.Path.(Eio.Stdenv.cwd env / "trees") ] in
  let cache = Irmin_defs.Cache.main repo in
  Build_cache.update_cache dirs cache

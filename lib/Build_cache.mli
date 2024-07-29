open Eio
open Irmin_defs
open Forester_core

module S : sig
  val yield : Eio.Fs.dir_ty Eio.Path.t -> unit
  val run : (unit -> unit) -> Eio.Fs.dir_ty Eio.Path.t Seq.t

  val register_printer :
    ([ `Yield of Eio.Fs.dir_ty Eio.Path.t ] -> string option) -> unit
end

module Cache' : sig
  val read : unit -> Cache.t
  val scope : (Cache.t -> Cache.t) -> (unit -> 'a) -> 'a
  val run : env:Cache.t -> (unit -> 'a) -> 'a
  val register_printer : ([ `Read ] -> string option) -> unit
end

(*
  val addr_of_fs_path : Eio.Fs.dir_ty Eio.Path.t -> Forester_core.addr
  val store_path_of_addr : Forester_core.addr -> Irmin_defs.Cache.path
  val store_path_of_path : Eio.Fs.dir_ty Eio.Path.t -> Irmin_defs.Cache.path
  val pp_store_path : Format.formatter -> string list -> unit
  val pp_path : [> Eio.Fs.dir_ty ] Eio.Path.t Fmt.t
  val changed_content : Fs.dir_ty Path.t -> Cache.t -> Code.tree option
*)

val update_content : Fs.dir_ty Path.t -> (unit, Cache.write_error) result

val update_cache :
  Fs.dir_ty Path.t list -> (unit, Cache.write_error list) result

val changed_trees : Fs.dir_ty Path.t list -> Code.tree list
val trees_to_reevaluate : Fs.dir_ty Path.t list -> Addr_set.t
val trees_to_rerender : Fs.dir_ty Path.t list -> Addr_set.t

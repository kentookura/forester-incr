open Irmin_defs
open Eio

val update_cache : Fs.dir_ty Path.t list -> Cache.t -> unit

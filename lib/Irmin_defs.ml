module Content : Irmin.Contents.S with type t = Forester_core.Code.t = struct
  open Irmin.Type

  type t = Forester_core.Code.t

  let t = Forester_irmin.Code.t
  let merge ~old:_ _a b = Irmin.Merge.ok b
  let merge = Irmin.Merge.(option (v t merge))
end

module Cache = Irmin_fs_unix.KV.Make (Content)
module Cache_info = Irmin_unix.Info (Cache.Info)

let info message = Cache_info.v "%s" message

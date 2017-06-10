val filename : string

val init : unit -> Async.Unix.Exit_or_signal.t Async_unix.Import.Deferred.t
val setup :
  int32 ->
  int32 ->
  int32 ->
  int32 list -> Async.Unix.Exit_or_signal.t Async_unix.Import.Deferred.t

val devname : unit -> string
val macaddr : unit -> bytes
val ipaddr : unit -> int32
val netmask : unit -> int32
val router : unit -> int32

val arp_cache : unit -> (int32, bytes) Core.Hashtbl.t
val arp_find : int32 Core.Hashtbl.key -> bytes option
val arp_set : int32 Core.Hashtbl.key -> bytes -> unit
val arp_add_exn : int32 Core.Hashtbl.key -> bytes -> unit

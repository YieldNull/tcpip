
val fd : Unix.file_descr

val init : unit -> unit
val setup :
  int32 ->
  int32 ->
  int32 ->
  int32 list -> unit

val devname : unit -> string
val macaddr : unit -> bytes
val ipaddr : unit -> int32
val netmask : unit -> int32
val router : unit -> int32

val arp_cache : unit -> (int32, bytes) Core.Hashtbl.t
val arp_find : int32 -> bytes option
val arp_set : int32 -> bytes -> unit
val arp_add_exn : int32 -> bytes -> unit

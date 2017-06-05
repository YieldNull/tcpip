val filename : string

val init : unit -> Async.Unix.Exit_or_signal.t Async_unix.Import.Deferred.t
val setup :
  Ipaddr.V4.t ->
  Ipaddr.V4.t ->
  Ipaddr.V4.t ->
  Ipaddr.V4.t list -> Async.Unix.Exit_or_signal.t Async_unix.Import.Deferred.t

val devname : unit -> string
val macaddr : unit -> Macaddr.t
val ipaddr : unit -> Ipaddr.V4.t
val netmask : unit -> Ipaddr.V4.t
val router : unit -> Ipaddr.V4.t

val arp_cache : unit -> (int32, string) Core.Hashtbl.t
val arp_find : int32 Core.Hashtbl.key -> string option
val arp_set : int32 Core.Hashtbl.key -> string -> unit
val arp_add_exn : int32 Core.Hashtbl.key -> string -> unit

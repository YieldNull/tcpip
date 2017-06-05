
type etype = | IPV4 | ARP | Others

type t =
  { dmac : Macaddr.t;
    smac : Macaddr.t;
    etype : etype;
  }

val mtu : int
val mpkt : int

val sizeof_ethernet : int
val etype_to_int : etype -> int
val int_to_etype : int -> etype option

val build : bytes -> bytes -> etype -> bytes -> bytes
val parse : bytes -> t

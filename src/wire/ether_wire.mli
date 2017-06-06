
type etype = | IPV4 | ARP

type t =
  { dmac : bytes;
    smac : bytes;
    etype : etype;
  }

val mtu : int
val mpkt : int

val size : int

val etype_to_int : etype -> int

val create : dmac:bytes -> smac:bytes -> etype:etype -> t

val of_frame : Cstruct.t -> t option
val to_pkt : t -> Cstruct.t

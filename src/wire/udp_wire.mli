open Cstruct

type t =
  { sport : uint16;
    dport : uint16;
    len   : uint16;
    checksum : uint16;
  }

val size : int

val create : sip:int32 -> dip:int32 -> sport:int -> dport:int -> Cstruct.t -> t

val of_frame : Cstruct.t -> Ipv4_wire.t -> t option
val to_pkt : t -> Cstruct.t

open Cstruct

type oper =
  | REQUEST
  | REPLY

type t =
  { htype : uint16;
    ptype : uint16;
    hlen  : uint8;
    plen  : uint8;
    oper  : oper;
    sha   : bytes;
    spa   : uint32;
    tha   : bytes;
    tpa   : uint32;
  }

val size : int

val create : sha:bytes -> spa:int32 -> tha:bytes -> tpa:int32 -> oper:oper -> t

val of_frame : Cstruct.t -> t option
val to_pkt : t -> Cstruct.t

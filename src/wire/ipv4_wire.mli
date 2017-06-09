
type protocol =
  | ICMP
  | TCP
  | UDP

type t =
  { len       : int;
    fid       : int;
    hasnext   : bool;
    offset    : int;
    ttl       : int;
    protocol  : protocol;
    sip       : int32;
    dip       : int32;
  }

val size : int
val protocol_to_int : protocol -> int

val create : ?ttl:int -> protocol:protocol -> sip:int32 -> dip:int32 -> int -> t list

val of_frame : Cstruct.t -> t option
val to_pkt : t -> Cstruct.t

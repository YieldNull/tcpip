open Ipaddr

type protocol =
  | ICMP
  | TCP
  | UDP
  | Others

type t =
  { sip : int32;
    dip : int32;
    fid : int;
    offset : int;
    hasnext : bool;
    protocol : protocol;
    payload : bytes;
  }

val build : ?ttl:int -> protocol -> int32 -> int32 -> bytes -> bytes list

val parse : bytes -> t option

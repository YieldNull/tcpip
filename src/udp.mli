type t =
  { sport : int;
    dport : int;
    payload : bytes;
  }

val build : int32 -> int32 -> int -> int -> bytes -> bytes
val parse : Ipv4.t -> t option

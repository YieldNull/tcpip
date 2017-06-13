val handle : Cstruct.t -> Ipv4_wire.t -> unit
val handle_tcp : Tcp_wire.t -> unit

val listen : Socket.t -> int -> unit
val accept : Socket.t -> unit

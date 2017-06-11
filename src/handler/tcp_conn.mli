type t =
  { mutable state       : Tcp_state.t;
    mutable snd_isn     : int32;
    mutable snd_unack   : int32;
    mutable snd_next    : int32;
    mutable snd_window  : int;
    mutable snd_urgent  : int32;
    mutable rcv_isn     : int32;
    mutable rcv_next    : int32;
    mutable rcv_window  : int;
    mutable rcv_urgent  : int32;
  }

val open_listen : unit -> t
val open_send : unit -> t

val gen_rst : ?conn:t -> Tcp_wire.t -> Tcp_wire.t
val gen_fin_ack : t -> Tcp_wire.t -> Tcp_wire.t
val gen_syn_ack  : t -> Tcp_wire.t -> Tcp_wire.t
val gen_ack  : t -> Tcp_wire.t -> Tcp_wire.t

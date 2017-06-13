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
    snd_queue : (bytes) Core.Squeue.t;
    rcv_queue : (bytes) Core.Squeue.t;
  }

val create_conn : unit -> t

val gen_rst : ?conn:t -> Tcp_wire.t -> Tcp_wire.t
val gen_fin : t -> Tcp_wire.t -> Tcp_wire.t
val gen_syn_ack  : t -> Tcp_wire.t -> Tcp_wire.t

val gen_ack : ?data:Cstruct.t -> t -> Tcp_wire.t -> Tcp_wire.t
val gen_syn_active : sport:int -> dip:int32 -> dport:int -> t -> Tcp_wire.t

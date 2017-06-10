type t =
  { mutable state : Tcp_state.t;
  }

val open_pas : t
val open_act : t

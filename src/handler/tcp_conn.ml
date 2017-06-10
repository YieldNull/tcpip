open Core
open Tcp_wire
open Tcp_wire.Option

type t =
  { mutable state : Tcp_state.t;
  }

let open_pas =
  { state = Tcp_state.ST_LISTEN }

let open_act =
  { state = Tcp_state.ST_CLOSED }

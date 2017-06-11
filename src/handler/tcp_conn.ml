(* Improving TCP's Robustness to Blind In-Window Attacks
   https://tools.ietf.org/html/rfc5961
*)

open Core
open Tcp_wire
open Tcp_wire.Option
open Tcp_state
open Int32

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

let open_listen () =
  { state = Tcp_state.ST_LISTEN;
    snd_isn     = 0l;
    snd_unack   = 0l;
    snd_next    = 0l;
    snd_window  = 65535;
    snd_urgent  = 0l;
    rcv_isn     = 0l;
    rcv_next    = 0l;
    rcv_window  = 65535;
    rcv_urgent  = 0l;
  }

let open_send () =
  { state = Tcp_state.ST_CLOSED;
    snd_isn     = 0l;
    snd_unack   = 0l;
    snd_next    = 0l;
    snd_window  = 65535;
    snd_urgent  = 0l;
    rcv_isn     = 0l;
    rcv_next    = 0l;
    rcv_window  = 65535;
    rcv_urgent  = 0l;
  }

let gen_rst ?conn tcp =
  let open Int32 in
  let seq, ctrls =
    if is_ctrl_set tcp.ctrl ACK then
      tcp.ack, [RST]
    else
      0l, (if is_ctrl_set tcp.ctrl SYN then [RST;ACK] else [RST])
      (* In the SYN-SENT state,the RST is acceptable
         if the ACK field acknowledges the SYN. [rfc5961#3.2] *)
  in
  let ack = (of_int_exn @@ Cstruct.len tcp.payload) + tcp.seq + 1l in
  { sip     = Iface.ipaddr ();
    dip     = tcp.sip;
    sport   = tcp.dport;
    dport   = tcp.sport;
    seq     = seq;
    ack     = ack;
    ctrl    = ctrl_list_to_int ctrls;
    window  = 0;
    urgent  = 0;
    options = [];
    payload = Cstruct.of_bytes Caml.Bytes.empty;
  }


let gen_fin_ack conn tcp =
  conn.snd_unack <- tcp.ack;
  conn.rcv_next <- (of_int_exn @@ Cstruct.len tcp.payload) + tcp.seq;
  { sip     = Iface.ipaddr ();
    dip     = tcp.sip;
    sport   = tcp.dport;
    dport   = tcp.sport;
    seq     = conn.snd_unack;
    ack     = conn.rcv_next;
    ctrl    = ctrl_list_to_int [FIN;ACK];
    window  = conn.rcv_window;
    urgent  = 0;
    options = [];
    payload = Cstruct.of_bytes Caml.Bytes.empty;
  }

let gen_syn_ack conn tcp =
  conn.snd_isn <- Random.int32 2147483647l + 1l;
  conn.snd_unack <- conn.snd_isn;
  conn.rcv_next <- tcp.seq + 1l;
  { sip     = Iface.ipaddr ();
    dip     = tcp.sip;
    sport   = tcp.dport;
    dport   = tcp.sport;
    seq     = conn.snd_unack;
    ack     = conn.rcv_next;
    ctrl    = ctrl_list_to_int [SYN;ACK];
    window  = conn.rcv_window;
    urgent  = 0;
    options = [];
    payload = Cstruct.of_bytes Caml.Bytes.empty;
  }

let gen_ack conn tcp =
  conn.snd_unack <- tcp.ack;
  conn.rcv_next <- (of_int_exn @@ Cstruct.len tcp.payload) + tcp.seq + 1l;
  { sip     = Iface.ipaddr ();
    dip     = tcp.sip;
    sport   = tcp.dport;
    dport   = tcp.sport;
    seq     = conn.snd_unack;
    ack     = conn.rcv_next;
    ctrl    = ctrl_list_to_int [ACK];
    window  = conn.rcv_window;
    urgent  = 0;
    options = [];
    payload = Cstruct.of_bytes Caml.Bytes.empty;
  }

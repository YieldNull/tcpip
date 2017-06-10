(*
https://tools.ietf.org/html/rfc5961
*)

open Tcp_wire
open Tcp_wire.Option
open Tcp_state
open Ipaddr
open Utils

let window_size = 1 lsl 16 - 1

let syn dip dport =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = dip;
      sport   = Random.int 10000 + 1024;
      dport   = dport;
      seq     = Random.int32 100l;
      ack     = 0l;
      ctrl = ctrl_list_to_int [SYN];
      window  = window_size;
      urgent  = 0;
      options = [Max_segmentation_size (mss 1460);];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let syn_ack pkt =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkt.sip;
      sport   = pkt.dport;
      dport   = pkt.sport;
      seq     = Random.int32 100l;
      ack     = Int32.add pkt.seq  1l;
      ctrl = ctrl_list_to_int [SYN; ACK];
      window  = window_size;
      urgent  = 0;
      options = [Max_segmentation_size (mss 1460);];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let ack pkt =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkt.sip;
      sport   = pkt.dport;
      dport   = pkt.sport;
      seq     = pkt.ack;
      ack     = Int32.add pkt.seq 1l;
      ctrl = ctrl_list_to_int [ACK];
      window  = window_size;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let fin pkt =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkt.sip;
      sport   = pkt.dport;
      dport   = pkt.sport;
      seq     = pkt.ack;
      ack     = Int32.add pkt.seq 1l;
      ctrl = ctrl_list_to_int [FIN];
      window  = window_size;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let fin_ack pkt =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkt.sip;
      sport   = pkt.dport;
      dport   = pkt.sport;
      seq     = pkt.ack;
      ack     = Int32.add pkt.seq 1l;
      ctrl = ctrl_list_to_int [FIN; ACK];
      window  = window_size;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

open Core

type t =
  { remote_ip  : int32;
    local_ip   : int32;
    local_port : int;
    conns : (int, Tcp_conn.t) Hashtbl.t; (* remote port -> connection *)
  }

let handlers = Hashtbl.create ~hashable:Int.hashable () (* local port -> handler *)

let find_conn pkt =
  match Hashtbl.find handlers pkt.dport with
  | None -> None
  | Some hd ->
    if (hd.remote_ip = 0l || hd.remote_ip = pkt.sip) && hd.local_ip = pkt.dip then
      Hashtbl.find hd.conns pkt.sport
    else
      None

let rst pkt =
  let open Int32 in
  let seq = if is_ctrl_set pkt.ctrl ACK then pkt.ack else 0l in
  let ack = (of_int_exn @@ Cstruct.len pkt.payload) + pkt.seq + 1l in
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkt.sip;
      sport   = pkt.dport;
      dport   = pkt.sport;
      seq     = seq;
      ack     = ack;
      ctrl    = ctrl_list_to_int [RST; ACK];
      window  = 0;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Caml.Bytes.empty;
    } in
  to_pkt tcp

let send_frame writer dip pkt =
  match Iface.arp_find dip with
  | None -> ()
  | Some dmac -> let ether = Ether_wire.ipv4_pkt dmac in
    let ip = Ipv4_wire.tcp_pkt dip (Cstruct.len pkt) in
    send writer (Cstruct.concat [ether; ip; pkt])

let handle writer frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> match find_conn tcp with
    | None -> send_frame writer tcp.sip (rst tcp)
    | _ -> ()

(* Improving TCP's Robustness to Blind In-Window Attacks
   https://tools.ietf.org/html/rfc5961
*)

open Tcp_wire
open Tcp_wire.Option
open Tcp_state
open Tcp_conn
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
      ctrl    = ctrl_list_to_int [SYN];
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
      ctrl    = ctrl_list_to_int [SYN; ACK];
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
      ctrl    = ctrl_list_to_int [ACK];
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
      ctrl    = ctrl_list_to_int [FIN];
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
      ctrl    = ctrl_list_to_int [FIN; ACK];
      window  = window_size;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

open Core

type t =
  { remote_ip  : int32;
    local_port : int;
    conns : (int, Tcp_conn.t) Hashtbl.t; (* remote port -> connection *)
  }

let handlers = let hd = Hashtbl.create ~hashable:Int.hashable () in
  let conns = Hashtbl.create ~hashable:Int.hashable () in
  Hashtbl.add_exn hd ~key:80
    ~data:
      { remote_ip = 0l;
        local_port = 5678;
        conns     = conns
      };
  hd
(* local port -> handler *)

let find_conn tcp =
  match Hashtbl.find handlers tcp.dport with
  | None -> None
  | Some handler ->
    if handler.remote_ip = 0l || handler.remote_ip = tcp.sip then
      Some (Hashtbl.find_or_add handler.conns tcp.sport
              ~default:(fun () -> open_listen ()))
    else
      None

let send_frame writer tcp =
  match Iface.arp_find tcp.dip with
  | None -> ()
  | Some dmac -> let pkt = to_pkt tcp in
    let ether = Ether_wire.ipv4_pkt dmac in
    let ip = Ipv4_wire.tcp_pkt tcp.dip (Cstruct.len pkt) in
    send writer (Cstruct.concat [ether; ip; pkt])

let ctrl_to_action num =
  if is_ctrl_set num SYN then
    if is_ctrl_set num ACK then AT_SYN_ACK else AT_SYN
  else if is_ctrl_set num FIN then AT_FIN
  else if is_ctrl_set num RST then AT_RST
  else AT_ACK

let handle writer frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> match find_conn tcp with
    | None -> send_frame writer (gen_rst tcp)
    | Some conn ->
      let action = ctrl_to_action tcp.ctrl in
      match trans_state conn.state action with
      | None -> send_frame writer (gen_rst tcp)
      | Some (state, sending) ->
        conn.state <- state;
        match conn.state, sending with
        | ST_ESTABLISHED, Some AT_ACK ->
          send_frame writer (gen_fin_ack conn tcp);
          begin
            match trans_state state AT_CLOSE with
            | Some (s, _) -> conn.state <- s;
            | None -> ()
          end
        | _, Some AT_ACK -> send_frame writer (gen_ack conn tcp)
        | _, Some AT_SYN_ACK -> send_frame writer (gen_syn_ack conn tcp)
        | _, _ -> ()

(* Improving TCP's Robustness to Blind In-Window Attacks
   https://tools.ietf.org/html/rfc5961
*)

open Core
open Tcp_wire
open Tcp_state
open Tcp_wire.Option

module Bytes = Caml.Bytes

type t =
  { local_ip    : int32;
    local_port  : int;
    remote_ip   : int32;
    remote_port : int;
    isn : int32;
    snd_buf : Sbuffer.t;
    rcv_buf : Sbuffer.t;
    local_wscale : int;
    mutable remote_wscale : int;
    mutable mss         : int;
    mutable state       : Tcp_state.t;
    mutable snd_unack   : int32;
    mutable snd_next    : int32;
    mutable snd_window  : int;
    mutable rcv_next    : int32;
  }

let create ~lip ~lport ~rip ~rport =
  let isn = Random.int32 2147483647l in
  let window = 65535 * 32 in
  { local_ip      = lip;
    local_port    = lport;
    local_wscale  = 5;
    remote_ip     = rip;
    remote_port   = rport;
    remote_wscale = 0;
    state       = Tcp_state.ST_CLOSED;
    mss         = 1460;
    isn         = isn;
    snd_unack   = isn;
    snd_next    = isn;
    snd_window  = window;
    snd_buf     = Sbuffer.create window;
    rcv_next    = 0l;
    rcv_buf     = Sbuffer.create window;
  }

let rcvpkt t tcp =
  if is_ctrl_set tcp.ctrl SYN then
    begin
      let open Int32 in
      let wscale = Core.Option.value (find_opt_wscale tcp) ~default:0 in
      let mss = Core.Option.value (find_opt_mss tcp) ~default:t.mss in
      t.remote_wscale <- wscale;
      t.mss <- mss;
      t.rcv_next <- tcp.seq + 1l
    end
  else if tcp.ack <= t.snd_next then
    begin
      t.snd_unack <- tcp.ack;
      t.snd_window <- tcp.window;
      if tcp.seq = t.rcv_next then
        let data = Cstruct.to_string tcp.payload in
        let len = Bytes.length data in
        let count = Sbuffer.write t.rcv_buf ~buf:data in
        if count <> len then failwith "buff overflow";
        let open Int32 in
        if is_ctrl_set tcp.ctrl FIN then
          (* The segment length (SEG.LEN) includes both data and sequence
             space occupying controls. [RFC793#3.3 page 26] *)
          t.rcv_next <- t.rcv_next + 1l
        else
          t.rcv_next <- t.rcv_next + (of_int_exn len)
    end

let send_frame tcp =
  match Iface.arp_find tcp.dip with
  | None -> ()
  | Some dmac -> let pkt = to_pkt tcp in
    let ether = Ether_wire.ipv4_pkt dmac in
    let ip = Ipv4_wire.tcp_pkt tcp.dip (Cstruct.len pkt) in
    Inetio.send_cstruct (Cstruct.concat [ether; ip; pkt])

let send ?options ?payload t ~ctrls =
  let options = Core.Option.value options ~default:[] in
  let payload = Core.Option.value payload ~default:Bytes.empty in
  let window = (Sbuffer.freesize t.rcv_buf) lsr t.local_wscale in
  let seq = t.snd_next in
  if List.mem ctrls SYN ~equal:(=) || List.mem ctrls FIN ~equal:(=) then
    t.snd_next <- Int32.(+) t.snd_next 1l
  else
    t.snd_next <- Int32.(+) t.snd_next (Int32.of_int_exn @@ Bytes.length payload)
  ;
  send_frame
    { sip     = t.local_ip;
      dip     = t.remote_ip;
      sport   = t.local_port;
      dport   = t.remote_port;
      seq     = seq;
      ack     = t.rcv_next;
      ctrl    = ctrl_list_to_int ctrls;
      window  = window;
      urgent  = 0;
      options = options;
      payload = Cstruct.of_bytes payload;
    }

let send_syn t =
  send t ~ctrls:[SYN]
    ~options:
      [ Max_segmentation_size (mss t.mss);
        Window_scale (wscale t.local_wscale);
      ]

let send_syn_ack t =
  send t ~ctrls:[SYN; ACK]
    ~options:
      [ Max_segmentation_size (mss t.mss);
        Window_scale (wscale t.local_wscale);
      ]

let send_ack t =
  send t ~ctrls:[ACK]

let send_data t data =
  send t ~ctrls:[ACK] ~payload:data

let send_fin t =
  send t ~ctrls:[FIN; ACK]

let send_rst tcp =
  let seq, ctrls =
    if is_ctrl_set tcp.ctrl ACK then
      tcp.ack, [RST]
    else
      0l, (if is_ctrl_set tcp.ctrl SYN then [RST;ACK] else [RST])
      (* In the SYN-SENT state,the RST is acceptable
         if the ACK field acknowledges the SYN. [rfc5961#3.2] *)
  in
  let open Int32 in
  let ack = tcp.seq + 1l in
  send_frame
    { sip     = tcp.dip;
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

let read_buf t len =
  let buf = Bytes.create len in
  let len = Sbuffer.read t.rcv_buf ~buf in
  Bytes.sub buf 0 len

let write_buf t data =
  let count = Sbuffer.write t.snd_buf ~buf:data in
  let buf = Bytes.create t.mss in
  let len = Sbuffer.read t.snd_buf buf in
  send_data t (Bytes.sub buf 0 len);
  count

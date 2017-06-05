open Utils
open Core
open Ipaddr

module Bytes = Caml.Bytes

[%%cstruct
  type ipv4 =
    { vihl : uint8; (* Version and IHL *)
      tos  : uint8; (* DSCP and ECN *)
      len  : uint16;
      fid  : uint16;
      offset : uint16; (* flags and offset *)
      ttl  : uint8;
      protocol : uint8;
      checksum : uint16;
      sip : uint32;
      dip : uint32;
    }[@@big_endian]
]

[%%cenum
  type protocol =
    | ICMP [@id 0x01]
    | TCP [@id 0x06]
    | UDP [@id 0x11]
    | Others
  [@@uint8]
]

type t =
  { sip : int32;
    dip : int32;
    fid : int;
    offset : int;
    hasnext : bool;
    protocol : protocol;
    payload : Caml.Bytes.t;
  }

let max_fragment = Ether.mtu - sizeof_ipv4

let fid = ref 0

let gen_id () = fid := !fid + 1; !fid

let build ?(ttl = 64) protocol source_ip dest_ip data =
  let protocol = protocol_to_int protocol in
  let total = Bytes.length data in
  let groups = total / max_fragment + 1 in
  List.rev @@
  List.fold (List.range 1 groups ~stop:`inclusive) ~init:[] ~f:(fun acc group ->
      let total_length, flags =
        if group = groups then
          total - max_fragment * (groups - 1) + sizeof_ipv4, 0x000
        else
          Ether.mtu, 0x001
      in
      let offset = (group - 1) * max_fragment in
      let header = Cstruct.create sizeof_ipv4 in
      let flags_offset = (flags lsl 13) lor offset in
      let fid = gen_id () in
      let time_to_live = ttl in
      set_ipv4_vihl header 0x45;
      set_ipv4_tos header 0x00;
      set_ipv4_len header total_length;
      set_ipv4_fid header fid;
      set_ipv4_offset header flags_offset;
      set_ipv4_ttl header time_to_live;
      set_ipv4_protocol header protocol;
      set_ipv4_checksum header 0x0000;
      set_ipv4_sip header source_ip;
      set_ipv4_dip header dest_ip;
      set_ipv4_checksum header (checksum (Cstruct.to_string header));
      let fragment = Bytes.create total_length in
      Bytes.blit (Cstruct.to_string header) 0 fragment 0 sizeof_ipv4;
      Bytes.blit data offset fragment sizeof_ipv4 (total_length - sizeof_ipv4);
      fragment :: acc
    )

let parse packet =
  let header = Bytes.sub packet 0 sizeof_ipv4 in
  if not (validate header) then None
  else
    let cs = Cstruct.of_bytes header in
    let len = get_ipv4_len cs in
    let fid = get_ipv4_fid cs in
    let flag_offset = get_ipv4_offset cs in
    let hasnext = flag_offset lsr 13 = 1 in
    let offset = flag_offset land 0x1fff in
    let protocol = Option.value ~default:Others (int_to_protocol @@ get_ipv4_protocol cs) in
    let sip = get_ipv4_sip cs in
    let dip = get_ipv4_dip cs in
    let payload = Bytes.sub packet sizeof_ipv4 (len - sizeof_ipv4) in
    Some { sip; dip; fid; offset; hasnext; protocol; payload }

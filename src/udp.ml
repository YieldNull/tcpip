open Utils
open Core

module Bytes = Caml.Bytes

[%%cstruct
  type udp =
    { sport    : uint16;
      dport    : uint16;
      length   : uint16;
      checksum : uint16;
    }[@@big_endian]
]

[%%cstruct
  type psudp =
    { sip       : uint32;
      dip       : uint32;
      zeros     : uint8;
      protocol  : uint8;
      udplen    : uint16;
      udp       : uint8 [@len 8];
    }[@@big_endian]
]

type t =
  { sport : int;
    dport : int;
    payload : Caml.Bytes.t;
  }

let build_pseudo sip dip len udp =
  let psudp = Cstruct.create sizeof_psudp in
  set_psudp_sip psudp sip;
  set_psudp_dip psudp dip;
  set_psudp_zeros psudp 0;
  set_psudp_protocol psudp 0x11;
  set_psudp_udplen psudp len;
  set_psudp_udp udp 0 psudp;
  Cstruct.to_string psudp

let build sip dip sport dport payload =
  let udp = Cstruct.create sizeof_udp in
  let udplen = sizeof_udp + Bytes.length payload in
  set_udp_sport udp sport;
  set_udp_dport udp dport;
  set_udp_length udp udplen;
  set_udp_checksum udp 0;
  let psudp = build_pseudo sip dip udplen (Cstruct.to_string udp) in
  set_udp_checksum udp (checksum (Bytes.cat psudp payload));
  Bytes.cat (Cstruct.to_string udp) payload

let parse ipv4 =
  let open Ipv4 in
  let packet = ipv4.payload in
  let len = Bytes.length packet in
  let payload = Bytes.sub packet sizeof_udp (len - sizeof_udp) in
  let psudp = build_pseudo ipv4.sip ipv4.dip len (Bytes.sub packet 0 sizeof_udp) in
  if not (validate @@ Bytes.cat psudp payload) then None
  else
    let cs = Cstruct.of_bytes packet in
    let sport = get_udp_sport cs in
    let dport = get_udp_dport cs in
    Some { sport; dport; payload }

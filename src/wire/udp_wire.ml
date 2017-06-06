open Utils
open Core
open Cstruct

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
  { sport : uint16;
    dport : uint16;
    len   : uint16;
    checksum : uint16;
  }

let size = sizeof_udp

let build_pseudo sip dip len udp =
  let psudp = Cstruct.create sizeof_psudp in
  set_psudp_sip psudp sip;
  set_psudp_dip psudp dip;
  set_psudp_zeros psudp 0;
  set_psudp_protocol psudp 0x11;
  set_psudp_udplen psudp len;
  set_psudp_udp (Cstruct.to_string udp) 0 psudp;
  psudp

let create ~sip ~dip ~sport ~dport payload =
  let udp = Cstruct.create sizeof_udp in
  let udplen = sizeof_udp + Cstruct.len payload in
  set_udp_sport udp sport;
  set_udp_dport udp dport;
  set_udp_length udp udplen;
  set_udp_checksum udp 0;
  let psudp = build_pseudo sip dip udplen udp in
  let checksum = checksum_list [ psudp; payload ] in
  { sport; dport; len = udplen; checksum; }

let of_frame frame ipv4 =
  let base = Ether_wire.size + Ipv4_wire.size in
  let pkt = Cstruct.sub frame base sizeof_udp in
  let len = get_udp_length pkt in
  let psudp = build_pseudo ipv4.Ipv4_wire.sip ipv4.Ipv4_wire.dip len pkt in
  let payload = Cstruct.sub frame (base + sizeof_udp) (len - sizeof_udp) in
  if not (validate_list [ psudp; payload ]) then None
  else
    let sport = get_udp_sport pkt in
    let dport = get_udp_dport pkt in
    let checksum = get_udp_checksum pkt in
    Some { sport; dport; len; checksum }

let to_pkt t =
  let pkt = Cstruct.create sizeof_udp in
  set_udp_sport pkt t.sport;
  set_udp_dport pkt t.dport;
  set_udp_length pkt t.len;
  set_udp_checksum pkt t.checksum;
  pkt

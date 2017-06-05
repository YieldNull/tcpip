open Ipaddr
open Core

module Bytes = Caml.Bytes

[%%cstruct
  type ethernet =
    { dmac : uint8 [@len 6];
      smac : uint8 [@len 6];
      etype : uint16;
    }[@@big_endian]
]

[%%cenum
  type etype =
    | IPV4 [@id 0x0800]
    | ARP [@id 0x0806]
    | Others
  [@@uint16]
]

type t =
  { dmac : Macaddr.t;
    smac : Macaddr.t;
    etype : etype;
  }

let mtu = 1500
let mpkt = 1500 + sizeof_ethernet

let build smac dmac etype payload =
  let pkt = Cstruct.create sizeof_ethernet in
  set_ethernet_dmac dmac 0 pkt;
  set_ethernet_smac smac 0 pkt;
  set_ethernet_etype pkt (etype_to_int etype);
  Bytes.cat (Cstruct.to_string pkt) payload

let parse packet =
  let packet = Cstruct.of_bytes packet in
  let dmac = Macaddr.of_bytes_exn @@ Cstruct.to_string @@ get_ethernet_dmac packet in
  let smac = Macaddr.of_bytes_exn @@ Cstruct.to_string @@ get_ethernet_smac packet in
  let etype = Option.value ~default:Others (int_to_etype @@ get_ethernet_etype packet) in
  { dmac; smac; etype }

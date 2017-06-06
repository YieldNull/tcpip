(* https://en.wikipedia.org/wiki/Ethernet_frame#Ethernet_II *)

open Cstruct

[%%cstruct
  type ether =
    { dmac : uint8 [@len 6];
      smac : uint8 [@len 6];
      etype : uint16;
    }[@@big_endian]
]

[%%cenum
  type etype =
    | IPV4 [@id 0x0800]
    | ARP [@id 0x0806]
  [@@uint16]
]

type t =
  { dmac : bytes;
    smac : bytes;
    etype : etype;
  }

let mtu = 1500
let mpkt = 1500 + sizeof_ether
let size = sizeof_ether

let create ~dmac ~smac ~etype =
  { dmac; smac; etype }

let of_frame frame =
  let pkt = Cstruct.sub frame 0 size in
  let dmac = Cstruct.to_string @@ get_ether_dmac pkt in
  let smac = Cstruct.to_string @@ get_ether_smac pkt in
  match int_to_etype @@ get_ether_etype pkt with
  | None -> None
  | Some etype ->
    Some { dmac; smac; etype }

let to_pkt t =
  let pkt = Cstruct.create sizeof_ether in
  set_ether_dmac t.dmac 0 pkt;
  set_ether_smac t.smac 0 pkt;
  set_ether_etype pkt (etype_to_int t.etype);
  pkt

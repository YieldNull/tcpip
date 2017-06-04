[%%cstruct
  type ethernet =
    { tmac : uint8 [@len 6];
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

let build smac tmac etype payload =
  let pkt = Cstruct.create sizeof_ethernet in
  set_ethernet_tmac tmac 0 pkt;
  set_ethernet_smac smac 0 pkt;
  set_ethernet_etype pkt (etype_to_int etype);
  Cstruct.to_string @@ Cstruct.append pkt payload

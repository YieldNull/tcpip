(* Ether Address Resolution Protocol
   https://tools.ietf.org/html/rfc826
*)

open Cstruct
open Ipaddr

[%%cstruct
  type arp =
    { htype : uint16;
      ptype : uint16;
      hlen  : uint8;
      plen  : uint8;
      oper  : uint16;
      sha   : uint8 [@len 6];
      spa   : uint32;
      tha   : uint8 [@len 6];
      tpa   : uint32;
    }[@@big_endian]
]

[%%cenum
  type oper =
    | REQUEST [@id 1]
    | REPLY [@id 2]
  [@@uint16]
]

type t =
  { htype : uint16;
    ptype : uint16;
    hlen  : uint8;
    plen  : uint8;
    oper  : oper;
    sha   : bytes;
    spa   : uint32;
    tha   : bytes;
    tpa   : uint32;
  }

let size = sizeof_arp

let create ~sha ~spa ~tha ~tpa ~oper =
  { htype = 1;
    ptype = Ether_wire.etype_to_int Ether_wire.IPV4;
    hlen  = 6;
    plen  = 4;
    oper  = oper;
    sha; spa; tha; tpa;
  }

let of_frame frame =
  let pkt = Cstruct.sub frame Ether_wire.size size in
  let htype = get_arp_htype pkt in
  let ptype = get_arp_ptype pkt in
  let hlen = get_arp_hlen pkt in
  let plen = get_arp_plen pkt in
  let oper = int_to_oper @@ get_arp_oper pkt in
  let sha = Cstruct.to_string @@ get_arp_sha pkt in
  let spa = get_arp_spa pkt in
  let tha = Cstruct.to_string @@ get_arp_tha pkt in
  let tpa = get_arp_tpa pkt in
  match oper with
  | None -> None
  | Some oper ->
    Some { htype; ptype; hlen; plen; oper; sha; spa; tha; tpa }

let to_pkt t =
  let pkt = Cstruct.create size in
  set_arp_htype pkt t.htype;
  set_arp_ptype pkt t.ptype;
  set_arp_hlen pkt t.hlen;
  set_arp_plen pkt t.plen;
  set_arp_oper pkt (oper_to_int t.oper);
  set_arp_sha t.sha 0 pkt;
  set_arp_spa pkt t.spa;
  set_arp_tha t.tha 0 pkt;
  set_arp_tpa pkt t.tpa;
  pkt

let request ip =
  to_pkt @@ create ~sha:(Iface.macaddr ()) ~spa:(Iface.ipaddr ())
    ~tha:(V4.to_bytes V4.broadcast) ~tpa:ip ~oper:REQUEST

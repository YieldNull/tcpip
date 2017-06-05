open Ipaddr
open Core

(* Ether Address Resolution Protocol
   https://tools.ietf.org/html/rfc826
*)

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

let handle packet =
  let packet = Cstruct.of_bytes packet in
  let ptype = get_arp_ptype packet in
  if ptype = Ether.etype_to_int Ether.IPV4 then
    let sha = Cstruct.to_string @@ get_arp_sha packet in
    let spa = get_arp_spa packet in
    let tpa = get_arp_tpa packet in
    let oper = get_arp_oper packet in
    let merge_flag =
      match Iface.arp_find spa with
      | Some _ -> Iface.arp_set spa sha; true
      | None -> false
    in
    let iface_ip = V4.to_int32 @@ Iface.ipaddr () in
    let iface_mac = Macaddr.to_bytes @@ Iface.macaddr () in
    if Int32.equal tpa iface_ip && (sha <> iface_mac) then
      begin
        if merge_flag then Iface.arp_add_exn spa sha;
        if int_to_oper oper = Some REQUEST then begin
          set_arp_sha iface_mac 0 packet;
          set_arp_spa packet iface_ip;
          set_arp_tha sha 0 packet ;
          set_arp_tpa packet spa;
          set_arp_oper packet (oper_to_int REPLY);
          Some (Ether.build iface_mac sha Ether.ARP (Cstruct.to_string packet))
        end else None
      end else None
  else None

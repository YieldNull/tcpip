open Ipaddr
open Core

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

let cache = Hashtbl.create ~hashable:Int32.hashable ()

let handle packet =
  let ptype = get_arp_ptype packet in
  if ptype = 0x0800 then
    let sha = Cstruct.to_string @@ get_arp_sha packet in
    let spa = get_arp_spa packet in
    let tpa = get_arp_tpa packet in
    let oper = get_arp_oper packet in
    let merge_flag =
      match Hashtbl.find cache spa with
      | Some _ -> Hashtbl.set cache ~key:spa ~data:sha; true
      | None -> false
    in
    let iface_ip = V4.to_int32 @@ Iface.get_ipaddr () in
    let iface_mac = Macaddr.to_bytes @@ Iface.get_macaddr () in
    if Int32.equal tpa iface_ip && (sha <> iface_mac) then
      begin
        if merge_flag then Hashtbl.add_exn cache ~key:spa ~data:sha;
        if int_to_oper oper = Some REQUEST then begin
          set_arp_sha iface_mac 0 packet;
          set_arp_spa packet iface_ip;
          set_arp_tha sha 0 packet ;
          set_arp_tpa packet spa;
          set_arp_oper packet (oper_to_int REPLY);
          Some (Ethernet.build iface_mac sha Ethernet.ARP packet)
        end else None
      end else None
  else None

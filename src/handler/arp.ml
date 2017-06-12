open Arp_wire
open Ipaddr

let handle frame =
  match of_frame frame with
  | None -> ()
  | Some arp ->
    if arp.ptype = Ether_wire.etype_to_int Ether_wire.IPV4 then
      let merge_flag =
        match Iface.arp_find arp.spa with
        | Some _ -> Iface.arp_set arp.spa arp.sha; true
        | None -> false
      in
      let iface_ip = Iface.ipaddr () in
      let iface_mac = Iface.macaddr () in
      if Int32.equal arp.tpa iface_ip && (arp.sha <> iface_mac) then
        begin
          if not merge_flag then Iface.arp_add_exn arp.spa arp.sha;
          if arp.oper = REQUEST then begin
            let arp_t = Arp_wire.create ~sha:iface_mac
                ~spa:iface_ip ~tha:arp.sha ~tpa:arp.spa ~oper:REPLY in
            let pkt = to_pkt arp_t in
            let ether = Ether_wire.to_pkt @@ Ether_wire.create ~dmac:arp.sha
                ~smac:iface_mac ~etype:Ether_wire.ARP in
            Inetio.send_cstruct (Cstruct.append ether pkt)
          end
        end

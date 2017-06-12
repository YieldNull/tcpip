open Udp_wire

let handle frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some udp ->
    if udp.sport = Dhcp_wire.server_port
    && udp.dport = Dhcp_wire.client_port
    then
      let payload = Cstruct.sub frame
          (Ether_wire.size + Ipv4_wire.size + Udp_wire.size)
          (udp.len - Udp_wire.size)
      in match Dhcp.handle payload with
      | Some pkt -> Inetio.send_cstruct pkt
      | _ -> ()

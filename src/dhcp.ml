open Ipaddr
open Dhcp_wire
open Core

let discover () =
  let mac = Iface.macaddr () in
  let xid = Random.int32 0x7fffffffl in
  let packet =
    { op = BOOTREQUEST;
      htype = Ethernet_10mb;
      hlen = 6;
      hops = 0;
      xid = xid;
      secs = 0;
      flags = Broadcast;
      ciaddr = Ipaddr.V4.any;
      yiaddr = Ipaddr.V4.any;
      siaddr = Ipaddr.V4.any;
      giaddr = Ipaddr.V4.any;
      chaddr = mac;
      sname = "";
      file = "";
      options = [
        Message_type DHCPDISCOVER;
        Max_message Ether.mtu;
        Client_id (Hwaddr mac);
        Ip_lease_time 7776000l;
        Hostname (Iface.devname ());
        Parameter_requests [
          SUBNET_MASK; ROUTERS; DNS_SERVERS; IP_LEASE_TIME;
        ]
      ]
    }
  in
  let smac = Macaddr.to_bytes mac in
  let dmac = Macaddr.to_bytes Macaddr.broadcast in
  let sip = V4.to_int32 V4.any in
  let dip = V4.to_int32 V4.broadcast in
  let sport = client_port in
  let dport = server_port in
  let dhcp = buf_of_pkt packet in
  let udp = Udp.build sip dip sport dport dhcp in
  let ip = Ipv4.build Ipv4.UDP sip dip udp in
  Ether.build smac dmac Ether.IPV4 (List.hd_exn ip)

let request mac xid serverip requestip =
  let packet =
    { op = BOOTREQUEST;
      htype = Ethernet_10mb;
      hlen = 6;
      hops = 0;
      xid = xid;
      secs = 0;
      flags = Broadcast;
      ciaddr = Ipaddr.V4.any;
      yiaddr = Ipaddr.V4.any;
      siaddr = Ipaddr.V4.any;
      giaddr = Ipaddr.V4.any;
      chaddr = mac;
      sname = "";
      file = "";
      options = [
        Message_type DHCPREQUEST;
        Max_message (Ether.mtu);
        Client_id (Hwaddr mac);
        Hostname (Iface.devname ());
        Server_identifier serverip;
        Request_ip requestip;
        Parameter_requests [
          SUBNET_MASK; ROUTERS; DNS_SERVERS; IP_LEASE_TIME;
        ]
      ]
    }
  in
  let smac = Macaddr.to_bytes mac in
  let dmac = Macaddr.to_bytes Macaddr.broadcast in
  let sip = V4.to_int32 V4.any in
  let dip = V4.to_int32 V4.broadcast in
  let sport = client_port in
  let dport = server_port in
  let dhcp = buf_of_pkt packet in
  let udp = Udp.build sip dip sport dport dhcp in
  let ip = Ipv4.build Ipv4.UDP sip dip udp in
  Ether.build smac dmac Ether.IPV4 (List.hd_exn ip)

let handle_offer pkt =
  let ipaddr = pkt.yiaddr in
  let serverip = List.find_map_exn pkt.options ~f:(function
      | Server_identifier ip -> Some ip
      | _ -> None
    ) in
  let xid = pkt.xid in
  let mac = pkt.chaddr in
  Some (request mac xid serverip ipaddr)

let handle_ack pkt =
  let ipaddr = pkt.yiaddr in
  let netmask = Option.value_exn (find_subnet_mask pkt.options) in
  let router = List.hd_exn @@ collect_routers pkt.options in
  let dns = collect_dns_servers pkt.options in
  ignore @@ Iface.setup ipaddr netmask router dns;
  None

let handle packet =
  let pkt = match pkt_of_buf (Cstruct.of_bytes packet) (Bytes.length packet) with
    | Result.Ok pkt -> pkt
    | _ -> failwith "Invalid DHCP packet"
  in
  let t = find_message_type pkt.options in
  match t with
  | Some DHCPOFFER -> handle_offer pkt
  | Some DHCPACK -> handle_ack pkt
  | _ -> None

open Ipaddr
open Utils
open Dhcp_wire
open Core

let discover mac =
  let xid = Random.int32 0x7fffffffl in
  let packet =
    { srcmac = mac;
      dstmac = Macaddr.broadcast;
      srcip = Ipaddr.V4.any;
      dstip = Ipaddr.V4.broadcast;
      srcport = client_port;
      dstport = server_port;
      op = BOOTREQUEST;
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
        Max_message 1500;
        Client_id (Hwaddr mac);
        Ip_lease_time 7776000l;
        Hostname "tap.local";
        Parameter_requests [
          SUBNET_MASK; ROUTERS; DNS_SERVERS; IP_LEASE_TIME;
        ]
      ]
    }
  in
  Cstruct.to_string @@ buf_of_pkt packet

let request mac xid serverip requestip =
  let packet =
    { srcmac = mac;
      dstmac = Macaddr.broadcast;
      srcip = Ipaddr.V4.any;
      dstip = Ipaddr.V4.broadcast;
      srcport = client_port;
      dstport = server_port;
      op = BOOTREQUEST;
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
        Max_message 1500;
        Client_id (Hwaddr mac);
        Hostname "tap.local";
        Server_identifier serverip;
        Request_ip requestip;
        Parameter_requests [
          SUBNET_MASK; ROUTERS; DNS_SERVERS; IP_LEASE_TIME;
        ]
      ]
    }
  in
  Cstruct.to_string @@ buf_of_pkt packet

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
  let netmask = List.find_map_exn pkt.options ~f:(function
      | Subnet_mask mask -> Some mask
      | _ -> None
    ) in
  let routers = List.find_map_exn pkt.options ~f:(function
      | Routers routers -> Some routers
      | _ -> None
    ) in
  ignore @@ Async.Unix.system @@
  sprintf "ifconfig %s %s netmask %s" Iface.devname (V4.to_string ipaddr) (V4.to_string netmask);
  Iface.set_ipaddr ipaddr;
  None

let handle packet =
  let pkt = match pkt_of_buf (Cstruct.of_bytes packet) (String.length packet) with
    | Result.Ok pkt -> pkt
    | _ -> failwith "Invalid DHCP packet"
  in
  let t = List.find_map_exn pkt.options ~f:(function
      | Message_type t -> Some t
      | _ -> None
    ) in
  match t with
  | DHCPOFFER -> handle_offer pkt
  | DHCPACK -> handle_ack pkt
  | _ -> None

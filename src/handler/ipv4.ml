open Ipv4_wire

let handle writer frame ether =
  match of_frame frame with
  | None -> ()
  | Some ipv4 ->
    match ipv4.protocol with
    | UDP -> Udp.handle writer frame ipv4
    | TCP -> Tcp.handle writer frame ipv4
    | ICMP -> Icmp.handle writer frame ether ipv4

open Ipv4_wire

let handle frame ether =
  match of_frame frame with
  | None -> ()
  | Some ipv4 ->
    match ipv4.protocol with
    | UDP -> Udp.handle frame ipv4
    | TCP -> Tcp.handle frame ipv4
    | ICMP -> Icmp.handle frame ether ipv4

open Ether_wire

let handle frame =
  match of_frame frame with
  | None -> ()
  | Some ether ->
    match ether.etype with
    | IPV4 -> Ipv4.handle frame ether
    | ARP -> Arp.handle frame

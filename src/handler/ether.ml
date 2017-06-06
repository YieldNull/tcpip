open Ether_wire

let handle writer frame =
  match of_frame frame with
  | None -> ()
  | Some ether ->
    match ether.etype with
    | IPV4 -> Ipv4.handle writer frame
    | ARP -> Arp.handle writer frame

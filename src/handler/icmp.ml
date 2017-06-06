open Icmp_wire

let handle writer frame ether ipv4 =
  match of_frame frame with
  | Some (Echo_requets req) ->
    let icmp = to_pkg @@ Echo_reply req in
    let ip = Ipv4_wire.to_pkt @@ List.hd @@ Ipv4_wire.create
        ~protocol:Ipv4_wire.ICMP
        ~sip:ipv4.Ipv4_wire.dip
        ~dip:ipv4.Ipv4_wire.sip (Cstruct.len icmp)
    in
    let ether = Ether_wire.to_pkt @@ Ether_wire.rev_mac ether in
    Utils.send writer (Cstruct.concat [ ether; ip; icmp ])
  | _ -> ()

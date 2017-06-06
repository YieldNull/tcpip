open Ipv4_wire

let handle writer frame =
  match of_frame frame with
  | None -> ()
  | Some ipv4 ->
    match ipv4.protocol with
    | UDP -> Udp.handle writer frame ipv4
    | _ -> ()

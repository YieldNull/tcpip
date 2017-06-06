(* INTERNET CONTROL MESSAGE PROTOCOL
   https://tools.ietf.org/html/rfc792
*)

open Core
open Utils
open Cstruct

[%%cstruct
  type icmp =
    { tp       : uint8;
      code     : uint8;
      checksum : uint16;
    }[@@big_endian]
]

[%%cstruct
  type echo =
    { id        : uint16;
      seq       : uint16;
    }[@@big_endian]
]

[%%cenum
  type typecode =
    | ECHO_REPLY              [@id 0]
    | DESTINATION_UNREACHABLE [@id 3]
    | ECHO_REQUEST            [@id 8]
    | TIME_EXCEEDED           [@id 11]
  [@@uint8]
]

type echo =
  { id        : uint16;
    seq       : uint16;
    payload   : Cstruct.t;
  }

type t =
  | Echo_requets of echo
  | Echo_reply of echo
  | Destination_unreachable
  | Time_exceeded

let create_echo ?payload ~id ~seq =
  let payload = match payload with
    | Some p -> p
    | _ -> Cstruct.of_bytes @@ String.init 48 ~f:char_of_int
  in
  { id; seq; payload }

let echo_of_frame frame =
  let base = Ether_wire.size + Ipv4_wire.size + sizeof_icmp in
  let pkt = Cstruct.sub frame base (Cstruct.len frame  - base) in
  let id = get_echo_id pkt in
  let seq = get_echo_seq pkt in
  let payload = Cstruct.sub pkt 4 (Cstruct.len pkt - 4) in
  { id; seq; payload }

let echo_to_pkt echo =
  let pkg = Cstruct.create sizeof_echo in
  set_echo_id pkg echo.id;
  set_echo_seq pkg echo.seq;
  Cstruct.append pkg echo.payload

let of_frame frame =
  let base = (Ether_wire.size + Ipv4_wire.size) in
  let pkg = Cstruct.sub frame base (Cstruct.len frame - base) in
  if not (validate pkg) then None
  else
    let tp = get_icmp_tp pkg in
    let code = get_icmp_code pkg in
    match int_to_typecode tp with
    | Some ECHO_REQUEST -> if code <> 0 then None else Some (Echo_requets (echo_of_frame frame))
    | Some ECHO_REPLY -> if code <> 0 then None else Some (Echo_reply (echo_of_frame frame))
    | _ -> None

let to_pkg t =
  let tp, payload = match t with
    | Echo_requets ec -> ECHO_REQUEST, echo_to_pkt ec
    | Echo_reply ec -> ECHO_REPLY, echo_to_pkt ec
    | _ -> failwith "not implemented yet"
  in
  let icmp = Cstruct.create sizeof_icmp in
  set_icmp_tp icmp (typecode_to_int tp);
  set_icmp_code icmp 0;
  set_icmp_checksum icmp 0;
  set_icmp_checksum icmp (checksum_list [ icmp; payload ]);
  Cstruct.append icmp payload

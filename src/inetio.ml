open Core
open Unix

let buffer = Bytes.create Ether_wire.mpkt

let read () =
  let len = Unix.read Iface.fd ~buf:buffer ~pos:0 ~len:Ether_wire.mpkt in
  let frame = Cstruct.create len in
  Cstruct.blit_from_bytes buffer 0 frame 0 len;
  frame

let send_bytes ?pos ?len frame =
  let pos = Option.value pos ~default:0 in
  let len = Option.value len ~default:(Bytes.length frame) in
  ignore @@ Unix.write Iface.fd ~buf:frame ~pos ~len

let send_cstruct frame =
  send_bytes (Cstruct.to_string frame)

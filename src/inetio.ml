open Core
open Unix

let snd_queue = Squeue.create 1024

let buffer = Bytes.create Ether_wire.mpkt

let read () =
  let len = Unix.read Iface.fd ~buf:buffer ~pos:0 ~len:Ether_wire.mpkt in
  Caml.Bytes.sub buffer 0 len

let send frame =
  ignore @@ Squeue.push_or_drop snd_queue frame

let send_cstruct cstruct =
  send (Cstruct.to_string cstruct)

let pop_snd () = Squeue.pop snd_queue

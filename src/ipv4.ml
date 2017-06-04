open Utils
open Core

type protocol =
  | ICMP
  | TCP
  | UDP

let protocol_num p =
  match p with
  | ICMP -> 0x01
  | TCP -> 0x06
  | UDP -> 0x11

let mtu = 1500

let header_len = 20

let max_fragment = mtu - header_len

let identification = ref 0

let gen_id () = identification := !identification + 1; !identification

let build protocol source_ip dest_ip data =
  let protocol = protocol_num protocol in
  let total = Bytes.length data in
  let groups = total / max_fragment + 1 in
  List.rev @@
  List.fold (List.range 1 groups ~stop:`inclusive) ~init:[] ~f:(fun acc group ->
      let total_length, flags =
        if group = groups then
          total - max_fragment * (groups - 1) + header_len, 0x000
        else
          mtu, 0x001
      in
      let offset = (group - 1) * max_fragment in
      let header = Bytes.create header_len in
      let flags_offset = (flags lsl 13) lor offset in
      let identification = gen_id () in
      let time_to_live = 64 in
      set_int header ~pos:0 ~len:1 0x45;
      set_int header ~pos:1 ~len:1 0x00;
      set_int header ~pos:2 ~len:2 total_length;
      set_int header ~pos:4 ~len:2 identification;
      set_int header ~pos:6 ~len:2 flags_offset;
      set_int header ~pos:8 ~len:1 time_to_live;
      set_int header ~pos:9 ~len:1 protocol;
      set_int header ~pos:10 ~len:2 0x0000;
      set_ipv4 header ~pos:12 source_ip;
      set_ipv4 header ~pos:16 dest_ip;
      set_int header ~pos:10 ~len:2 (checksum header);
      let fragment = Bytes.create total_length in
      Caml.Bytes.blit header 0 fragment 0 header_len;
      Caml.Bytes.blit data offset fragment header_len (total_length - header_len);
      fragment :: acc
    )

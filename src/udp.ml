open Utils

let header_len = 8

let pseudo_len = 12

let build src_ip dest_ip src_port dest_port data =
  let header = Bytes.create header_len in
  let udp_len = Bytes.length data + header_len in
  set_int header ~pos:0 ~len:2 src_port;
  set_int header ~pos:2 ~len:2 dest_port;
  set_int header ~pos:4 ~len:2 udp_len;
  set_int header ~pos:6 ~len:2 0x0000;
  let pseudo_header = Bytes.create pseudo_len in
  set_ipv4 pseudo_header ~pos:0 src_ip;
  set_ipv4 pseudo_header ~pos:4 dest_ip;
  set_int pseudo_header ~pos:8 ~len:1 0x00;
  set_int pseudo_header ~pos:9 ~len:1 0x11;
  set_int pseudo_header ~pos:10 ~len:2 udp_len;
  let udp_packet = Bytes.cat header data in
  let pseudo_packet = Bytes.cat pseudo_header udp_packet in
  let checksum = checksum pseudo_packet in
  set_int udp_packet ~pos:6 ~len:2 checksum;
  udp_packet

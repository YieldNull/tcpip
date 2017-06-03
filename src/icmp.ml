open Utils

let build_icmp_request () =
  let _type = 0x08 in
  let code = 0x00 in
  let identifier = 0x1024 in
  let sequence_numer = 0x0000 in
  let icmp = Bytes.create (8 + 48) in
  set_int icmp ~pos:0 ~len:1 _type;
  set_int icmp ~pos:1 ~len:1 code;
  set_int icmp ~pos:2 ~len:2 0;
  set_int icmp ~pos:4 ~len:2 identifier;
  set_int icmp ~pos:6 ~len:2 sequence_numer;
  Bytes.fill icmp 8 48 (char_of_int 0xff);
  set_int icmp ~pos:2 ~len:2 (checksum icmp);
  icmp

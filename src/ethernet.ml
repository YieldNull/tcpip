open Utils

type etype = IPV4 | ARP

let build src_mac dest_mac _type data =
  let ether_type =
    match _type with
    | IPV4 -> 0x0800
    | ARP -> 0x0806
  in
  let header = Bytes.create 14 in
  Bytes.blit dest_mac 0 header 0 6;
  Bytes.blit src_mac 0 header 6 6;
  set_int header ~pos:12 ~len:2 ether_type;
  Bytes.cat header data

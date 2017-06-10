(* INTERNET PROTOCOL
   https://tools.ietf.org/html/rfc791
*)

open Core

[%%cstruct
  type ipv4 =
    { vihl     : uint8; (* Version and IHL *)
      tos      : uint8; (* DSCP and ECN *)
      len      : uint16;
      fid      : uint16;
      offset   : uint16; (* flags and offset *)
      ttl      : uint8;
      protocol : uint8;
      checksum : uint16;
      sip      : uint32;
      dip      : uint32;
    }[@@big_endian]
]

[%%cenum
  type protocol =
    | ICMP [@id 0x01]
    | TCP  [@id 0x06]
    | UDP  [@id 0x11]
  [@@uint8]
]

type t =
  { len       : int;
    fid       : int;
    hasnext   : bool;
    offset    : int;
    ttl       : int;
    protocol  : protocol;
    sip       : int32;
    dip       : int32;
  }

let size = sizeof_ipv4
let max_fragment = Ether_wire.mtu - size

let fid_table = Hashtbl.create ~hashable:Int32.hashable ()

let gen_id dip = Hashtbl.find_or_add fid_table dip ~default:(fun () -> 0) + 1

let create ?(ttl = 64) ~protocol ~sip ~dip payload_len =
  let total = payload_len in
  let groups = total / max_fragment + 1 in
  let fid = if groups > 1 then gen_id sip else Random.int 0xffff in
  List.rev @@
  List.fold (List.range 1 groups ~stop:`inclusive) ~init:[] ~f:(fun acc group ->
      let total_length, hasnext =
        if group = groups then
          total - max_fragment * (groups - 1) + sizeof_ipv4, false
        else
          Ether_wire.mtu, true
      in
      let offset = (group - 1) * max_fragment in
      { len = total_length; fid; hasnext; offset; ttl; protocol; sip; dip } :: acc
    )

let of_frame frame =
  let pkt = Cstruct.sub frame Ether_wire.size size in
  if not (Utils.validate pkt) then None
  else
    let vihl = get_ipv4_vihl pkt in
    let tos = get_ipv4_tos pkt in
    let len = get_ipv4_len pkt in
    let fid = get_ipv4_fid pkt in
    let flag_offset = get_ipv4_offset pkt in
    let ttl = get_ipv4_ttl pkt in
    let protocol = get_ipv4_protocol pkt in
    let sip = get_ipv4_sip pkt in
    let dip = get_ipv4_dip pkt in
    match int_to_protocol protocol with
    | None -> None
    | Some protocol ->
      if vihl <> 0x45 || tos <> 0x00 then None
      else
        let hasnext = flag_offset lsr 13 = 0x001 in
        let offset = flag_offset land 0x1fff in
        Some { len; fid; hasnext; offset; ttl; protocol; sip; dip }

let to_pkt t =
  let pkt = Cstruct.create size in
  set_ipv4_vihl pkt 0x45;
  set_ipv4_tos pkt 0x00;
  set_ipv4_len pkt t.len;
  set_ipv4_fid pkt t.fid;
  set_ipv4_offset pkt ((if t.hasnext then 1 else 0) lsl 13 + t.offset);
  set_ipv4_ttl pkt t.ttl;
  set_ipv4_protocol pkt (protocol_to_int t.protocol);
  set_ipv4_checksum pkt 0;
  set_ipv4_sip pkt t.sip;
  set_ipv4_dip pkt t.dip;
  set_ipv4_checksum pkt (Utils.checksum pkt);
  pkt

let tcp_pkt ?(ttl = 64) dip len =
  to_pkt @@ List.hd_exn @@ create ~ttl ~protocol:TCP ~sip:(Iface.ipaddr ()) ~dip len

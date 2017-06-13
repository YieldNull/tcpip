(* TRANSMISSION CONTROL PROTOCOL
   https://tools.ietf.org/html/rfc793

   TCP Selective Acknowledgment Options
   https://tools.ietf.org/html/rfc2018

   TCP Extensions for High Performance
   https://tools.ietf.org/html/rfc7323
*)

open Core
open Cstruct

module Option = struct
  [%%cenum
    type opcode =
      | END_OF_OPTIONS        [@id 0]
      | NO_OPERATION          [@id 1]
      | MAX_SEGMENTATION_SIZE [@id 2]
      | WINDOW_SCALE          [@id 3]
      | SACK_PERMITTED        [@id 4]
      | SACK                  [@id 5]
      | TIMESTAMPS            [@id 8]
    [@@uint8]
  ]

  [%%cstruct
    type eol =
      { kind : uint8;
      }[@@big_endian]
  ]

  [%%cstruct
    type noper =
      { kind : uint8;
      }[@@big_endian]
  ]

  [%%cstruct
    type mss =
      { kind : uint8;
        len  : uint8;
        mss  : uint16;
      }[@@big_endian]
  ]

  [%%cstruct
    type wscale =
      { kind : uint8;
        len  : uint8;
        cnt  : uint8;
      }[@@big_endian]
  ]

  [%%cstruct
    type sack =
      { kind : uint8;
        len  : uint8;
      }[@@big_endian]
  ]

  [%%cstruct
    type tmstamps =
      { kind  : uint8;
        len   : uint8;
        tsval : uint32;
        tsecr : uint32;
      }[@@big_endian]
  ]

  type kind = { kind : uint8; }

  type mss =
    { kind : uint8;
      len  : uint8;
      mss  : uint16;
    }

  type wscale =
    { kind : uint8;
      len  : uint8;
      cnt  : uint8;
    }

  type sackpm =
    { kind : uint8;
      len  : uint8;
    }

  type sack =
    { kind    : uint8;
      len     : uint8;
      blocks  : (uint32 * uint32) list
    }

  type tmstamps =
    { kind  : uint8;
      len   : uint8;
      tsval : uint32;
      tsecr : uint32;
    }

  type t =
    | End_of_options        of kind     (* kind 0 *)
    | No_operation          of kind     (* kind 1 *)
    | Max_segmentation_size of mss      (* kind 2 *)
    | Window_scale          of wscale   (* kind 3 *)
    | Sack_permitted        of sackpm   (* kind 4 *)
    | Sack                  of sack     (* kind 5 *)
    | Timestamps            of tmstamps (* kind 8 *)

  let assert_len actual expected = if expected <> actual then failwith "malformed option"

  let eol () =
    { kind = opcode_to_int END_OF_OPTIONS }

  let eol_of_buf buf =
    let kind = get_eol_kind buf in { kind }

  let eol_to_buf (t:kind) =
    let buf = Cstruct.create sizeof_eol in
    set_eol_kind buf t.kind;
    buf

  let noper () =
    { kind = opcode_to_int NO_OPERATION }

  let noper_of_buf buf =
    let kind = get_noper_kind buf in { kind }

  let noper_to_buf (t:kind) =
    let buf = Cstruct.create sizeof_noper in
    set_noper_kind buf t.kind;
    buf

  let mss mss =
    { kind = opcode_to_int MAX_SEGMENTATION_SIZE;
      len  = sizeof_mss;
      mss  = mss;
    }

  let mss_of_buf buf =
    let kind = get_mss_kind buf in
    let len = get_mss_len buf in
    let mss = get_mss_mss buf in
    assert_len len sizeof_mss;
    { kind; len; mss }

  let mss_to_buf (t:mss) =
    let buf = Cstruct.create sizeof_mss in
    set_mss_kind buf t.kind;
    set_mss_len buf t.len;
    set_mss_mss buf t.mss;
    buf

  let wscale cnt =
    { kind = opcode_to_int WINDOW_SCALE;
      len  = sizeof_wscale;
      cnt  = cnt;
    }

  let wscale_of_buf buf =
    let kind = get_wscale_kind buf in
    let len = get_wscale_len buf in
    let cnt = get_wscale_cnt buf in
    assert_len len sizeof_wscale;
    { kind; len; cnt }

  let wscale_to_buf (t:wscale) =
    let buf = Cstruct.create sizeof_wscale in
    set_wscale_kind buf t.kind;
    set_wscale_len buf t.len;
    set_wscale_cnt buf t.cnt;
    buf

  let sackpm () =
    { kind = opcode_to_int SACK_PERMITTED; len = 2 }

  let sackpm_of_buf buf =
    let kind = get_sack_kind buf in
    let len = get_sack_len buf in
    assert_len len sizeof_sack;
    { kind; len }

  let sackpm_to_buf (t:sackpm) =
    let buf = Cstruct.create sizeof_sack in
    set_sack_kind buf t.kind;
    set_sack_len buf t.len;
    buf

  let sack blocks =
    let kind = opcode_to_int SACK in
    let len = (List.length blocks) * 8 + sizeof_sack in
    { kind; len; blocks }

  let sack_of_buf buf =
    let kind = get_sack_kind buf in
    let len = get_sack_len buf in
    let blocks = List.map (List.range 0 ((len - sizeof_sack) / 8)) ~f:(fun i ->
        let left = Cstruct.BE.get_uint32 buf (i * 8 + sizeof_sack) in
        let right = Cstruct.BE.get_uint32 buf (i * 8 + sizeof_sack + 4 ) in
        left, right
      ) in
    { kind; len; blocks }

  let sack_to_buf (t:sack) =
    let buf = Cstruct.create t.len in
    set_sack_kind buf t.kind;
    set_sack_len buf t.len;
    List.iteri t.blocks ~f:(fun i b ->
        let left, right = b in
        Cstruct.BE.set_uint32 buf (sizeof_sack + i) left;
        Cstruct.BE.set_uint32 buf (sizeof_sack + i + 4) right
      );
    buf

  let tmstamps tsval tsecr =
    { kind = opcode_to_int TIMESTAMPS;
      len  = sizeof_tmstamps;
      tsval; tsecr
    }

  let tmstamps_of_buf buf =
    let kind = get_tmstamps_kind buf in
    let len = get_tmstamps_len buf in
    let tsval = get_tmstamps_tsval buf in
    let tsecr = get_tmstamps_tsecr buf in
    assert_len len sizeof_tmstamps;
    { kind; len; tsval; tsecr }

  let tmstamps_to_buf (t:tmstamps) =
    let buf = Cstruct.create sizeof_tmstamps in
    set_tmstamps_kind buf t.kind;
    set_tmstamps_len buf t.len;
    set_tmstamps_tsval buf t.tsval;
    set_tmstamps_tsecr buf t.tsecr;
    buf

  let list_of_buf buf =
    let rec aux acc buf =
      let take opt len =
        aux (opt :: acc) (Cstruct.shift buf len)
      in
      match int_to_opcode @@ Cstruct.get_uint8 buf 0 with
      | None -> let len = Cstruct.get_uint8 buf 1 in aux acc (Cstruct.shift buf len)
      | Some kind -> match kind with
        | END_OF_OPTIONS -> (End_of_options (eol_of_buf buf)) :: acc
        | NO_OPERATION -> take (No_operation (noper_of_buf buf)) sizeof_noper
        | MAX_SEGMENTATION_SIZE -> take (Max_segmentation_size (mss_of_buf buf)) sizeof_mss
        | WINDOW_SCALE -> take (Window_scale (wscale_of_buf buf)) sizeof_wscale
        | SACK_PERMITTED -> take (Sack_permitted (sackpm_of_buf buf)) sizeof_sack
        | SACK -> let sack = sack_of_buf buf in take (Sack sack) sack.len
        | TIMESTAMPS -> take (Timestamps (tmstamps_of_buf buf)) sizeof_tmstamps
    in
    if Cstruct.len buf = 0 then [] else aux [] buf

  let list_to_buf options =
    Cstruct.concat @@ List.map options ~f:(function
        | End_of_options opt -> eol_to_buf opt
        | No_operation opt -> noper_to_buf opt
        | Max_segmentation_size opt -> mss_to_buf opt
        | Window_scale opt -> wscale_to_buf opt
        | Sack_permitted opt -> sackpm_to_buf opt
        | Sack opt -> sack_to_buf opt
        | Timestamps opt -> tmstamps_to_buf opt
      )
end

[%%cstruct
  type tcp =
    { sport     : uint16;
      dport     : uint16;
      seq       : uint32;
      ack       : uint32;
      hdlen     : uint8; (* first 4 bits *) (* The number of 32 bit words in the TCP Header *)
      ctrl   : uint8; (* last 6 bits *)
      window    : uint16;
      checksum  : uint16;
      urgent    : uint16;
    }[@@big_endian]
]

[%%cstruct
  type pstcp =
    { sip       : uint32;
      dip       : uint32;
      zeros     : uint8;
      protocol  : uint8;
      len       : uint16;
    }[@@big_endian]
]

type ctrl = URG | ACK | PSH | RST | SYN | FIN

type t =
  { sip     : uint32;
    dip     : uint32;
    sport   : uint16;
    dport   : uint16;
    seq     : uint32;
    ack     : uint32;
    ctrl    : uint8;
    window  : uint16;
    urgent  : uint16;
    options : Option.t list;
    payload : Cstruct.t;
  }

let to_string t =
  sprintf "sip:%s dip:%s sport:%d dport:%d seq:%ld ack:%ld window:%d"
    (Ipaddr.V4.to_string (Ipaddr.V4.of_int32 t.sip))
    (Ipaddr.V4.to_string (Ipaddr.V4.of_int32 t.dip))
    t.sport t.dport t.seq t.ack t.window

let int_to_ctrl_list num =
  let num = num land 0b00111111 in
  let ctrls = [ URG; ACK; PSH; RST; SYN; FIN ] in
  let mask = 0b00100000 in
  List.filter_mapi ctrls ~f:(fun i v ->
      if num land (mask lsr i) > 0 then Some v else None
    )

let ctrl_to_int = function
  | URG -> 0b00100000
  | ACK -> 0b00010000
  | PSH -> 0b00001000
  | RST -> 0b00000100
  | SYN -> 0b00000010
  | FIN -> 0b00000001

let ctrl_list_to_int lst =
  List.fold lst ~init:0 ~f:(fun acc c -> acc + (ctrl_to_int c))

let is_ctrl_set num ctrl = num land (ctrl_to_int ctrl) > 0

let hdlen_of_int num = (num lsr 4) * 4

let int_to_hdlen len = (len / 4) lsl 4

let build_pseudo_header sip dip len =
  let pstcp = Cstruct.create sizeof_pstcp in
  set_pstcp_sip pstcp sip;
  set_pstcp_dip pstcp dip;
  set_pstcp_zeros pstcp 0;
  set_pstcp_protocol pstcp (Ipv4_wire.protocol_to_int Ipv4_wire.TCP);
  set_pstcp_len pstcp len;
  pstcp

let of_frame frame ipv4 =
  let base = Ether_wire.size + Ipv4_wire.size in
  let pkt = Cstruct.shift frame base in
  let len = Cstruct.len frame - base in
  let sip = ipv4.Ipv4_wire.sip in
  let dip = ipv4.Ipv4_wire.dip in
  let pstcp = build_pseudo_header sip dip len in
  if not (Utils.validate_list [pstcp; pkt]) then None
  else
    let sport = get_tcp_sport pkt in
    let dport = get_tcp_dport pkt in
    let seq = get_tcp_seq pkt in
    let ack = get_tcp_ack pkt in
    let hdlen = hdlen_of_int @@ get_tcp_hdlen pkt in
    let ctrl = get_tcp_ctrl pkt in
    let window = get_tcp_window pkt in
    let urgent = get_tcp_urgent pkt in
    let options = Option.list_of_buf (Cstruct.sub pkt sizeof_tcp (hdlen - sizeof_tcp)) in
    let payload = Cstruct.shift pkt hdlen in
    Some { sip; dip; sport; dport; seq; ack;
           ctrl; window; urgent; options; payload }

let to_pkt t =
  let header = Cstruct.create sizeof_tcp in
  set_tcp_sport header t.sport;
  set_tcp_dport header t.dport;
  set_tcp_seq header t.seq;
  set_tcp_ack header t.ack;
  set_tcp_ctrl header t.ctrl;
  set_tcp_window header t.window;
  set_tcp_checksum header 0;
  set_tcp_urgent header t.urgent;
  let options = Option.list_to_buf t.options in
  let opt_len = Cstruct.len options in
  let pad_len = (4 - (opt_len mod 4)) mod 4 in
  let padding = Cstruct.create pad_len in
  let hdlen = sizeof_tcp + opt_len + pad_len in
  Cstruct.memset padding 0;
  set_tcp_hdlen header (int_to_hdlen hdlen);
  let len = hdlen + (Cstruct.len t.payload) in
  let pstcp = build_pseudo_header t.sip t.dip len in
  set_tcp_checksum header (Utils.checksum_list [pstcp; header; options; padding; t.payload]);
  Cstruct.concat [header; options; padding; t.payload]

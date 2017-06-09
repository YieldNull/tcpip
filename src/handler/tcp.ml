open Tcp_wire
open Tcp_wire.Option

let syn dip dport =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = dip;
      sport   = Random.int 10000 + 1024;
      dport   = dport;
      seq     = Random.int32 100l;
      ack     = 0l;
      control = control_list_to_int [SYN];
      window  = 1 lsl 16 - 1;
      urgent  = 0;
      options = [Max_segmentation_size (mss 1460);];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let syn_ack pkg =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkg.sip;
      sport   = pkg.dport;
      dport   = pkg.sport;
      seq     = Random.int32 100l;
      ack     = Int32.add pkg.seq  1l;
      control = control_list_to_int [SYN; ACK];
      window  = 1 lsl 16 - 1;
      urgent  = 0;
      options = [Max_segmentation_size (mss 1460);];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let ack pkg =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkg.sip;
      sport   = pkg.dport;
      dport   = pkg.sport;
      seq     = pkg.ack;
      ack     = Int32.add pkg.seq 1l;
      control = control_list_to_int [ACK];
      window  = 1 lsl 16 - 1;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let fin_ack pkg =
  let tcp =
    { sip     = Iface.ipaddr ();
      dip     = pkg.sip;
      sport   = pkg.dport;
      dport   = pkg.sport;
      seq     = pkg.ack;
      ack     = Int32.add pkg.seq 1l;
      control = control_list_to_int [FIN; ACK];
      window  = 1 lsl 16 - 1;
      urgent  = 0;
      options = [];
      payload = Cstruct.of_bytes Bytes.empty;
    } in
  to_pkt tcp

let handle writer frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> ()

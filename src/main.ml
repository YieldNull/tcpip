open Ipaddr
open Core
open Async
open Utils

let parse_udp writer frame =
  let base = 34 in
  let src_port = int_of_bytes @@ String.sub frame ~pos:base ~len:2 in
  let dest_port = int_of_bytes @@ String.sub frame ~pos:(2 + base) ~len:2 in
  if src_port = 67 && dest_port = 68 then
    match Dhcp.handle frame with
    | Some pkt -> Writer.write writer pkt; return @@ ignore @@ Writer.flushed writer
    | _ -> return ()
  else
    return ()

let parse_ip writer packet =
  let base = 14 in
  let protocol = int_of_char @@ String.get packet (9 + base) in
  (* printf "%d\n" protocol; *)
  match protocol with
  | 0x11 -> parse_udp writer packet
  | _ -> return ()

let parse_arp writer packet =
  match Arp.handle @@ Cstruct.of_string
    @@ String.sub packet ~pos:14 ~len:Arp.sizeof_arp
  with
  | Some pkt -> Writer.write writer pkt; return @@ ignore @@ Writer.flushed writer
  | _ -> return ()

let parse_ethernet writer packet =
  let t = String.sub packet ~pos:12 ~len:2 in
  match int_of_bytes t with
  | 0x0800 -> parse_ip writer packet
  | 0x0806 -> parse_arp writer packet
  | _ -> return ()

let rec drive reader writer =
  let writer = Writer.create (Reader.fd reader) in
  let buf = (Bytes.create 1514) in
  Reader.read reader buf
  >>= fun i ->
  parse_ethernet writer buf
  >>= fun _ -> drive reader writer

let write writer =
  let macaddr = Tuntap.get_macaddr Iface.devname in
  let packet = Dhcp.discover macaddr in
  Iface.set_macaddr macaddr;
  Writer.write writer packet;
  ignore @@ Writer.flushed writer

let setup () =
  Unix.openfile ("/dev/" ^ Iface.devname) ~mode:[Unix.(`Rdwr)]
  >>| fun fd ->
  Unix.system @@
  sprintf "ifconfig %s up && ifconfig bridge1 addm %s" Iface.devname Iface.devname
  >>| function
  | Result.Ok _ -> let reader = Reader.create fd in
    let writer = Writer.create fd in
    ignore @@ drive reader writer;
    Clock.run_after (Time.Span.of_int_sec 10) (fun _ ->
        write writer) ()
  | _ -> failwith "initialization failed"

let () =
  ignore @@ setup ();
  never_returns (Scheduler.go ())

open Ipaddr
open Core
open Async
open Utils
open Ether

module Bytes = Caml.Bytes
module Udp = Tcpip.Udp

let handle_ip writer frame =
  let pkt_ipv4 = Ipv4.parse @@ Bytes.sub frame sizeof_ethernet
      (Bytes.length frame - sizeof_ethernet)
  in
  match pkt_ipv4 with
  | None -> return ()
  | Some pkt ->
    match pkt.Ipv4.protocol with
    | Ipv4.UDP -> begin
        let pkt_udp = Udp.parse pkt in
        match pkt_udp with
        | None -> return ()
        | Some pkt_udp ->
          if pkt_udp.Udp.sport = 67 && pkt_udp.Udp.dport = 68 then
            match Dhcp.handle pkt_udp.Udp.payload with
            | Some pkt -> Writer.write writer pkt; return @@ ignore @@ Writer.flushed writer
            | _ -> return ()
          else
            return ()
      end
    | _ -> return ()

let handle_arp writer frame =
  match Arp.handle
          (Bytes.sub frame sizeof_ethernet Arp.sizeof_arp)
  with
  | Some pkt -> Writer.write writer pkt; return @@ ignore @@ Writer.flushed writer
  | _ -> return ()

let handle_frame writer frame =
  let ether = Ether.parse frame in
  match ether.etype with
  | IPV4 -> handle_ip writer frame
  | ARP -> handle_arp writer frame
  | _ -> return ()

let rec drive reader writer =
  let buf = (Bytes.create Ether.mpkt) in
  Reader.read reader buf
  >>= fun _ -> handle_frame writer buf
  >>= fun _ -> drive reader writer

let setup () =
  Unix.openfile Iface.filename ~mode:[Unix.(`Rdwr)]
  >>| fun fd -> Iface.init ()
  >>| function
  | Result.Ok _ -> (* reserve 10 secs for starting wireshark *)
    Clock.run_after (Time.Span.of_int_sec 10) (fun () ->
        let reader = Reader.create fd in
        let writer = Writer.create fd in
        ignore @@ drive reader writer;
        let packet = Dhcp.discover () in
        Writer.write writer packet;
        ignore @@ Writer.flushed writer
      ) ()
  | _ -> failwith "initialization failed"

let () =
  ignore @@ setup ();
  never_returns (Scheduler.go ())

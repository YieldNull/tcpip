(* Improving TCP's Robustness to Blind In-Window Attacks
   https://tools.ietf.org/html/rfc5961
*)

open Tcp_wire
open Tcp_conn
open Core
open Ipaddr

type t =
  { remote_ip  : int32;
    conns      : (int, Tcp_conn.t) Hashtbl.t; (* remote port -> connection *)
    handler    : Tcp_conn.t -> Tcp_wire.t -> Tcp_wire.t list
  }

let handlers = Hashtbl.create ~hashable:Int.hashable () (* local port -> handler *)

let find_conn tcp =
  match Hashtbl.find handlers tcp.dport with
  | None -> None
  | Some handler ->
    if handler.remote_ip = 0l || handler.remote_ip = tcp.sip then
      Some (handler.handler,
            (Hashtbl.find_or_add handler.conns tcp.sport
               ~default:(fun () -> open_listen ())))
    else
      None

let send_frame tcp =
  match Iface.arp_find tcp.dip with
  | None -> ()
  | Some dmac -> let pkt = to_pkt tcp in
    let ether = Ether_wire.ipv4_pkt dmac in
    let ip = Ipv4_wire.tcp_pkt tcp.dip (Cstruct.len pkt) in
    Inetio.send_cstruct (Cstruct.concat [ether; ip; pkt])

let handle frame ipv4 =
  let open Async in
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> match find_conn tcp with
    | None -> send_frame (gen_rst tcp)
    | Some (f, conn) ->
      List.iter (f conn tcp) ~f:(fun pkt -> send_frame pkt)

let listen port handler =
  Hashtbl.add_exn handlers ~key:port
    ~data:
      { remote_ip  = 0l;
        conns      = Hashtbl.create ~hashable:Int.hashable ();
        handler    = handler;
      }

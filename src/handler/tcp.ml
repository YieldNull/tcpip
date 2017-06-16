(* Improving TCP's Robustness to Blind In-Window Attacks
    https://tools.ietf.org/html/rfc5961
*)
open Core
open Socket
open Tcp_wire
open Tcp_conn
open Tcp_state

module AddrPair = struct
  module T = struct
    type t = Sockaddr.t * Sockaddr.t [@@deriving compare, sexp, hash]
  end
  include T
  include Hashable.Make(T)

  let of_socket socket = (sockaddr_exn socket), (peer_exn socket)

  let of_tcp tcp =
    let remoteaddr = Sockaddr.create tcp.sip tcp.sport in
    let localaddr  = Sockaddr.create tcp.dip tcp.dport in
    localaddr, remoteaddr
end

type clientconn = Socket.t * Tcp_conn.t

type serverconn =
  { socket   : Socket.t;
    backlog  : int;
    queue    : (AddrPair.t) Squeue.t;
  }

let openports:((int,serverconn) Shashtbl.t) =
  Shashtbl.create ~hashable:Int.hashable ()

let opensocks:((AddrPair.t, clientconn) Shashtbl.t) =
  Shashtbl.create ~hashable:AddrPair.hashable ()

let establish socket tcp =
  match Shashtbl.find openports (port_exn socket) with
  | Some sv ->
    Squeue.push sv.queue (AddrPair.of_tcp tcp)
  | None ->
    let msg = Sexp.to_string @@ sexp_of_response (Res_Socket socket) in
    Utils.sendto_file socket.pipename msg

let transfer socket conn tcp =
  print_endline (to_string tcp);
  Tcp_conn.rcvpkt conn tcp;
  let prestate = conn.state in
  match trans_state prestate (ctrl_to_action tcp.ctrl) with
  | None -> send_rst tcp
  | Some (newstate, action) ->
    begin
      match action with
      | Some AT_ACK -> send_ack conn
      | Some AT_SYN_ACK -> send_syn_ack conn
      | Some AT_FIN -> send_fin conn
      | _ -> ()
    end;
    conn.state <- newstate;
    begin
      match newstate with
      | ST_ESTABLISHED ->
        if prestate <> ST_ESTABLISHED
        then establish socket tcp
      | ST_TIME_WAIT | ST_CLOSED ->
        Shashtbl.remove opensocks (AddrPair.of_socket socket)
      | _ -> ()
    end

let handle_tcp tcp =
  match Shashtbl.find opensocks (AddrPair.of_tcp tcp) with
  | Some (sock, conn) -> transfer sock conn tcp
  | _ ->
    match Shashtbl.find openports tcp.dport with
    | None -> send_rst tcp
    | Some sv ->
      let svip = Socket.ipaddr_exn sv.socket in
      if (svip = 0l || tcp.dip = svip) &&
         (is_ctrl_set tcp.ctrl SYN) &&
         Squeue.length sv.queue < sv.backlog
      then
        let conn = Tcp_conn.create ~lip:tcp.dip ~lport:tcp.dport ~rip:tcp.sip ~rport:tcp.sport in
        conn.state <- ST_LISTEN;
        let newsock = Socket.create sv.socket.socktype in
        let addr = Sockaddr.create tcp.dip tcp.dport in
        newsock.sockaddr <- Some addr;
        newsock.peer <- Some (Sockaddr.create tcp.sip tcp.sport);
        let addrpair = AddrPair.of_socket newsock in
        Shashtbl.add_exn opensocks ~key:addrpair ~data:(newsock, conn);
        transfer newsock conn tcp
      else
        send_rst tcp

let handle frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> handle_tcp tcp

exception NotServerConn
exception CannotClose

let listen socket backlog =
  let queue = Squeue.create backlog in
  let server = { socket; backlog; queue; } in
  Shashtbl.add_exn openports ~key:(Socket.port_exn socket) ~data:server

let accept socket =
  let sv = Shashtbl.find_exn openports (Socket.port_exn socket) in
  let addrpair = Squeue.pop sv.queue in
  let sock, _ = Shashtbl.find_exn opensocks addrpair in
  sock

let read socket len =
  let _, conn = Shashtbl.find_exn opensocks (AddrPair.of_socket socket) in
  read_buf conn len

let write socket data =
  let _, conn = Shashtbl.find_exn opensocks (AddrPair.of_socket socket) in
  write_buf conn data

let close socket =
  match socket.peer with
  | Some addr ->
    let _, conn = Shashtbl.find_exn opensocks (AddrPair.of_socket socket) in
    Handler.add_task (fun () ->
        match trans_state conn.state AT_CLOSE with
        | None -> ()
        | Some (newstate, action) ->
          conn.state <- newstate;
          begin
            match newstate with
            | ST_TIME_WAIT | ST_CLOSED ->
              Shashtbl.remove opensocks (AddrPair.of_socket socket)
            | _ -> ()
          end;
          match action with
          | Some AT_FIN -> send_fin conn
          | _ -> ()
      )
  | None -> Shashtbl.remove openports (port_exn socket)

let connect socket peer =
  let local = Sockaddr.create (Iface.ipaddr ()) (Random.int (65535 - 1024) + 1024) in
  socket.sockaddr <- Some local;
  socket.peer <- Some peer;
  let conn = Tcp_conn.create ~lip:local.ipaddr ~lport:local.port
      ~rip:peer.ipaddr ~rport:peer.port in
  let addrpair = AddrPair.of_socket socket in
  Shashtbl.add_exn opensocks ~key:addrpair ~data:(socket, conn);
  conn.state <- ST_SYN_SENT;
  Handler.add_task (fun () -> send_syn conn)

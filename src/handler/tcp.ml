(* Improving TCP's Robustness to Blind In-Window Attacks
   https://tools.ietf.org/html/rfc5961
*)
open Tcp_wire
open Tcp_conn
open Tcp_state
open Core
open Socket

type clientconn = Socket.t * Tcp_conn.t

type serverconn =
  { socket  : Socket.t;
    queue   : (Tcp_wire.t) Squeue.t;
    conns   : (sockaddr, clientconn) Shashtbl.t
  }

type conntype =
  | ClientConn of clientconn
  | ServerConn of serverconn

let connections:((int,conntype) Shashtbl.t) = Shashtbl.create ~hashable:Int.hashable ()

let send_frame tcp =
  match Iface.arp_find tcp.dip with
  | None -> ()
  | Some dmac -> let pkt = to_pkt tcp in
    let ether = Ether_wire.ipv4_pkt dmac in
    let ip = Ipv4_wire.tcp_pkt tcp.dip (Cstruct.len pkt) in
    Inetio.send_cstruct (Cstruct.concat [ether; ip; pkt])

let send_rst tcp = send_frame (gen_rst tcp)

let establish socket tcp =
  let sockaddr = Sockaddr.create tcp.sip tcp.sport in
  let peer = Socket.socket socket.socktype in
  peer.sockaddr <- Some sockaddr;
  socket.peer <- Some peer;
  let msg = Sexp.to_string @@ sexp_of_response (Res_Socket socket) in
  Utils.sendto_file socket.pipename msg

let transfer socket conn tcp =
  let prestate = conn.state in
  let action = ctrl_to_action tcp.ctrl in
  match trans_state prestate action with
  | None -> send_rst tcp
  | Some (newstate, action) ->
    conn.state <- newstate;
    if prestate <> ST_ESTABLISHED && newstate = ST_ESTABLISHED then
      establish socket tcp;
    match action with
    | Some AT_ACK -> send_frame (gen_ack conn tcp)
    | Some AT_SYN_ACK -> send_frame (gen_syn_ack conn tcp)
    | Some AT_FIN -> send_frame (gen_fin conn tcp)
    | _ -> ()

let handle_tcp tcp =
  let remoteaddr = Sockaddr.create tcp.sip tcp.sport in
  match Shashtbl.find connections tcp.dport with
  | None -> send_rst tcp
  | Some (ClientConn (sock, conn)) -> let peer = Socket.peer_exn sock in
    if peer.sockaddr = Some remoteaddr then
      transfer sock conn tcp
    else send_rst tcp
  | Some (ServerConn sv) -> let svip = Socket.ipaddr_exn sv.socket in
    if svip = 0l || tcp.dip = svip then
      if is_ctrl_set tcp.ctrl SYN then
        begin
          if not (Squeue.push_or_drop sv.queue tcp) then
            send_rst tcp
        end
      else
        match Shashtbl.find sv.conns remoteaddr with
        | None -> send_rst tcp
        | Some (sock, conn) -> transfer sock conn tcp

let handle frame ipv4 =
  match of_frame frame ipv4 with
  | None -> ()
  | Some tcp -> handle_tcp tcp

exception NotServerConn

let listen socket backlog =
  let server =
    { socket;
      queue = Squeue.create backlog;
      conns = Shashtbl.create ~hashable:Socket.Sockaddr.hashable ();
    }
  in
  Shashtbl.add_exn connections ~key:(Socket.port_exn socket) ~data:(ServerConn server)

let accept socket =
  match Shashtbl.find_exn connections (Socket.port_exn socket) with
  | ClientConn _ -> raise NotServerConn
  | ServerConn sv ->
    let tcp = Squeue.pop sv.queue in
    let conn = create_conn () in
    let sockaddr = Sockaddr.create tcp.sip tcp.sport in
    conn.state <- ST_LISTEN;
    Shashtbl.add_exn sv.conns ~key:sockaddr ~data:(socket, conn);
    Handler.add_task (fun () -> handle_tcp tcp)

open Core

type socktype =
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
[@@deriving sexp]

module Sockaddr = struct
  module T = struct
    type t =
      { ipaddr : int32;
        port   : int;
      }
    [@@deriving hash, compare, sexp]
  end
  include T
  include Hashable.Make(T)

  let create ipaddr port = { ipaddr; port }
end

type sockaddr = Sockaddr.t =
  { ipaddr : int32;
    port   : int;
  }[@@deriving sexp]

type shutdown =
  | SHUTDOWN_OUT
  | SHUTDOWN_IN
  | SHUTDOWN_ALL
[@@deriving sexp]

type t =
  { socktype          : socktype;
    pipename          : string;
    mutable sockaddr  : sockaddr option;
    mutable peer      : sockaddr option;
  }
[@@deriving sexp]

module Msg = struct
  type request =
    | Req_Connect of t * sockaddr
    | Req_Listen  of t * int
    | Req_Accept  of t
    | Req_Close   of t * shutdown
    | Req_Read    of t * int
    | Req_Write   of t * string
  [@@deriving sexp]

  type response =
    | Res_Socket of t
    | Res_Data of string
    | Res_Err of Error.t
    | Res_OK
  [@@deriving sexp]
end
include Msg

exception SocketNotBound
exception SocketNotConnected
exception SocketNoAddress

let ipaddr_exn socket =
  match socket.sockaddr with
  | Some addr -> addr.ipaddr
  | _ -> raise SocketNotBound

let port_exn socket =
  match socket.sockaddr with
  | Some addr -> addr.port
  | _ -> raise SocketNotBound

let peer_exn socket =
  match socket.peer with
  | Some peer -> peer
  | _ -> raise SocketNotConnected

let sockaddr_exn socket =
  match socket.sockaddr with
  | Some sockaddr -> sockaddr
  | _ -> raise SocketNoAddress

let pipename = "pipe_ctrl"

let buf = String.create 81920

let rcv_msg socket =
  let fd = Unix.openfile socket.pipename ~mode:[Unix.O_RDONLY] in
  let len = Unix.read fd ~buf in
  Unix.close fd;
  let data = String.sub buf ~pos:0 ~len in
  print_endline data;
  response_of_sexp @@ Sexp.of_string data

let mutex = Mutex.create ()

let send_msg socket req =
  Mutex.lock mutex;
  let fd = Unix.openfile pipename ~mode:[Unix.O_WRONLY] in
  let msg = Sexp.to_string (sexp_of_request req) in
  print_endline msg;
  protect
    ~f:(fun () ->
        ignore @@ Unix.write fd msg
      )
    ~finally:(fun () -> Mutex.unlock mutex);
  Unix.close fd;
  rcv_msg socket

let create socktype =
  let pipename = "pipe_" ^ (Time.to_string (Time.now ())) in
  Unix.mkfifo pipename ~perm:0o660;
  { socktype; sockaddr = None; peer = None; pipename; }

let bind socket sockaddr =
  socket.sockaddr <- Some sockaddr

let listen socket backlog =
  let req = Req_Listen (socket, backlog) in
  match send_msg socket req with
  | Res_OK -> ()
  | Res_Err e -> ()
  | _ -> failwith "unkonwn exception"

let accept socket =
  let req = Req_Accept socket in
  match send_msg socket req with
  | Res_Socket newsock -> newsock
  | _ -> failwith "unkonwn exception"

let connect socket sockaddr = ()

let read ?(pos = 0) ?len socket ~buf =
  let len = Option.value len ~default:(String.length buf) in
  match send_msg socket (Req_Read (socket, len)) with
  | Res_Data data ->
    let len = String.length data in
    String.blit ~src:data ~src_pos:0 ~dst:buf ~dst_pos:pos ~len;
    len
  | _ -> failwith "unkonwn exception"

let write ?(pos = 0) ?len socket ~buf =
  let len = Option.value len ~default:(String.length buf) in
  let data = String.sub buf ~pos ~len in
  match send_msg socket (Req_Write (socket, data)) with
  | Res_OK -> ()
  | _ -> failwith "unkonwn exception"

(* TODO Return the number of bytes actually written *)

let close ?(shutdown = SHUTDOWN_ALL) socket =
  match socket.sockaddr with
  | None -> ()
  | _ ->
    match send_msg socket (Req_Close (socket, shutdown)) with
    | Res_OK -> Unix.remove socket.pipename
    | _ -> failwith "unkonwn exception"

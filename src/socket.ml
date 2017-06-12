open Core

type socktype =
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
[@@deriving sexp]

type sockaddr =
  { ipaddr : int32;
    port   : int;
  }
[@@deriving sexp]

type shutdown =
  | SHUTDOWN_OUT
  | SHUTDOWN_IN
  | SHUTDOWN_ALL
[@@deriving sexp]

type t =
  { socktype          : socktype;
    mutable sockaddr  : sockaddr option;
    mutable connected : t option
  }
[@@deriving sexp]

let socket socktype = { socktype; sockaddr = None; connected = None }

let bind socket sockaddr = socket.sockaddr <- sockaddr

let listen socket backlog = ()

let accept socket = ()

let connect socket sockaddr = ()

let read ?(pos = 0) ?len socket ~buf = ()

let write ?(pos = 0) ?len socket ~buf = ()

let close ?(shutdown = SHUTDOWN_ALL) socket = ()

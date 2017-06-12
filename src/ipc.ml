open Core
open Unix

let pipename = "pipe"

type request =
  | Connect of Socket.t * Socket.sockaddr
  | Listen  of Socket.t * int
  | Accept  of Socket.t
  | Close   of Socket.t * Socket.shutdown
  | Read    of Socket.t
  | Write   of Socket.t * string
[@@deriving sexp]

type response =
  | Sockaddr of Socket.sockaddr
  | Payload  of string
  | OK
[@@deriving sexp]


let handle fd = ()

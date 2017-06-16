open Core
open Socket
open Ipaddr

let () =
  let client = create SOCK_STREAM in
  let ip = "192.168.1.103" |> V4.of_string_exn |> V4.to_int32 in
  connect client (Sockaddr.create ip 8888);
  ignore @@ write client "GET / HTTP/1.1\r\nUser-Agent: tuntap\r\n\r\n";
  let buf = String.create 1500 in
  let len = read client buf in
  let data = String.sub buf ~pos:0 ~len in
  print_endline data;
  close client

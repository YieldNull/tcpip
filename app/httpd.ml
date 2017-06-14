open Core
open Socket

let () =
  let server = create SOCK_STREAM in
  bind server { ipaddr = 0l; port = 80 };
  listen server 50;
  let socket = accept server in
  let buf = String.create 1500 in
  let len = read socket buf in
  let data = String.sub buf ~pos:0 ~len in
  print_endline data;
  write socket "<html><body><h1>Hello World!</h1></body></html";
  close socket;
  close server

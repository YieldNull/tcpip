open Core
open Socket

let () =
  let socket = socket SOCK_STREAM in
  bind socket { ipaddr = 0l; port = 80 };
  listen socket 50;
  let newsocket = accept socket in
  ()

open Core
open Socket

let buf = String.create 81920

let send_response socket res =
  let msg = Sexp.to_string @@ sexp_of_response res in
  Utils.sendto_file socket.pipename msg

let handle pipe =
  let len = Unix.read pipe ~buf in
  if len > 0 then
    let data = String.sub buf ~pos:0 ~len in
    let req = request_of_sexp @@ Sexp.of_string data in
    match req with
    | Req_Listen (socket, backlog) ->
      Tcp.listen socket backlog;
      send_response socket Res_OK
    | Req_Accept socket ->
      let newsock = Tcp.accept socket in
      send_response socket (Res_Socket newsock)
    | Req_Close (socket, _) ->
      Tcp.close socket;
      send_response socket Res_OK
    | Req_Read (socket, len) ->
      let data = Tcp.read socket len in
      send_response socket (Res_Data data)
    | Req_Write (socket, data) ->
      Tcp.write socket data;
      send_response socket Res_OK
    | _ -> ()

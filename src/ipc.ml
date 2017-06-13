open Core
open Socket

let buf = String.create 81920

let handle pipe =
  let len = Unix.read pipe ~buf in
  if len > 0 then
    let data = String.sub buf ~pos:0 ~len in
    let req = request_of_sexp @@ Sexp.of_string data in
    match req with
    | Req_Listen (socket, backlog) ->
      let msg =
        Tcp.listen socket backlog;
        Sexp.to_string @@ sexp_of_response (Res_OK)
      in
      write socket msg
    | Req_Accept socket -> Tcp.accept socket
    | _ -> ()

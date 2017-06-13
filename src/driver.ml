open Core
open Unix

let start_thread_read () =
  let read () =
    while true do
      ignore @@ select ~read:[Iface.fd] ~write:[] ~except:[] ~timeout:`Never ();
      let frame = Inetio.read () in
      if Bytes.length frame > 0 then
        Handler.add_task (fun () -> Ether.handle (Cstruct.of_bytes frame))
    done
  in
  ignore (Thread.create read ())

let start_thread_send () =
  let send () =
    while true do
      let frame = Inetio.pop_snd () in
      ignore @@ write Iface.fd ~buf:frame
    done
  in
  ignore (Thread.create send ())

let start_thread_ipc () =
  let polling () =
    Unix.mkfifo Socket.pipename ~perm:0o660;
    let pipe = openfile Socket.pipename ~mode:[O_RDONLY] in
    while true do
      let fds = select ~read:[pipe] ~write:[] ~except:[] ~timeout:`Never () in
      let ready = fds.Unix.Select_fds.read in
      if List.mem ready pipe ~equal:(=) then
        Ipc.handle pipe
    done
  in
  ignore (Thread.create polling ())

let listen_signal () =
  let open Caml.Sys in
  let f = fun _ ->
    ignore @@ Unix.system ("rm -f " ^ Socket.pipename);
    exit 0
  in
  set_signal sigint (Signal_handle f);
  set_signal sigabrt (Signal_handle f);
  set_signal sigquit (Signal_handle f)

let () =
  Iface.init ();
  start_thread_read ();
  start_thread_send ();
  start_thread_ipc ();
  Inetio.send (Cstruct.to_string (Dhcp.discover ()));
  listen_signal ();
  Handler.handle_forever ()

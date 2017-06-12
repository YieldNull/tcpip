open Core
open Unix

let polling () =
  Unix.mkfifo Ipc.pipename ~perm:0o660;
  let pipe = openfile Ipc.pipename ~mode:[O_RDWR] in
  while true do
    let fds = select ~read:[pipe; Iface.fd] ~write:[] ~except:[] ~timeout:`Never () in
    let read_fds = fds.Select_fds.read in
    if List.mem read_fds Iface.fd ~equal:(=) then
      ignore (Thread.create (fun () ->
          Ether.handle (Inetio.read ())
        ) ())
    else
      ignore (Thread.create (fun () ->
          Ipc.handle pipe
        ) ())
  done

let () =
  Iface.init ();
  polling ();
  ignore (Thread.create (fun () ->
      Inetio.send_cstruct (Dhcp.discover ())
    ) ())

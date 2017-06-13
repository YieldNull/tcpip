open Core

type task = unit -> unit

let queue = Squeue.create 2048

let add_task task = Squeue.push queue task

let handle_forever () =
  while true do
    let task = Squeue.pop queue in
    task ()
  done

open Core

type ('a, 'b) t =
  { mutex   : Mutex.t;
    hashtbl : ('a, 'b) Hashtbl.t;
  }

let create ~hashable () =
  { mutex   = Mutex.create ();
    hashtbl = Hashtbl.create ~hashable ();
  }

let wrap t f =
  Mutex.lock t.mutex;
  protect ~f:f ~finally:(fun () -> Mutex.unlock t.mutex)

let add_exn t ~key ~data =
  wrap t (fun () -> Hashtbl.add_exn t.hashtbl ~key ~data)

let find t key =
  wrap t (fun () -> Hashtbl.find t.hashtbl key)

let find_exn t key =
  wrap t (fun () -> Hashtbl.find_exn t.hashtbl key)

let remove t key =
  wrap t (fun () -> Hashtbl.remove t.hashtbl key)

open Core

module Bytes = Caml.Bytes

type t =
  { buffer  : Bytes.t;
    maxsize : int;
    mutex   : Mutex.t;
    not_empty : Condition.t;
    not_full  : Condition.t;
    mutable writep  : int; (* next index of buffer to write *)
    mutable readp   : int; (* next index of buffer to read *)
    mutable overlap : bool;
  }

let create len =
  if len > Caml.Sys.max_string_length
  then failwith "exceed Sys.max_string_length";
  { buffer = Bytes.create len;
    maxsize = len;
    writep = 0;
    readp  = 0;
    overlap = false;
    mutex  = Mutex.create ();
    not_empty = Condition.create ();
    not_full = Condition.create ();
  }

let unsafe_len t =
  if t.writep = t.readp then
    if t.overlap then t.maxsize else 0
  else if t.writep > t.readp then
    t.writep - t.readp
  else
    t.maxsize - (t.readp - t.writep)

let unsafe_is_full t = unsafe_len t = t.maxsize

let unsafe_is_empty t = unsafe_len t = 0

let wrap t f =
  Mutex.lock t.mutex;
  protect ~f ~finally:(fun () ->
      if (not (unsafe_is_empty t)) then Condition.signal t.not_empty;
      if (not (unsafe_is_full t)) then Condition.signal t.not_full;
      Mutex.unlock t.mutex
    )

let wait_not_empty t =
  (* other thread may lock the mutex after being signalled and make buffer empty *)
  while unsafe_is_empty t do
    Condition.wait t.not_empty t.mutex
  done

let wait_not_full t =
  while unsafe_is_full t do
    Condition.wait t.not_full t.mutex
  done

let freesize t = wrap t (fun () -> t.maxsize - (unsafe_len t))

let write ?(pos = 0) ?len t ~buf =
  let len = Option.value len ~default:(Bytes.length buf) - pos in
  wrap t (fun () ->
      wait_not_full t;
      let count = ref 0 in
      while !count < len && not (unsafe_is_full t) do
        Bytes.set t.buffer t.writep (Bytes.get buf (pos + !count));
        count := !count + 1;
        t.writep <- if t.writep + 1 < t.maxsize then t.writep + 1 else 0;
        if t.writep = t.readp then t.overlap <- true
      done;
      !count
    )

let read ?(pos = 0) ?len t ~buf =
  let len = Option.value len ~default:(Bytes.length buf) - pos in
  wrap t (fun () ->
      wait_not_empty t;
      let count = ref 0 in
      while !count < len && not (unsafe_is_empty t) do
        Bytes.set buf (pos + !count) (Bytes.get t.buffer t.readp);
        count := !count + 1;
        t.readp <- if t.readp + 1 < t.maxsize then t.readp + 1 else 0;
        if t.overlap then t.overlap <- false
      done;
      !count
    )

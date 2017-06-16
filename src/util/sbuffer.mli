module Bytes = Core.Caml.Bytes

type t

val create : int -> t

val freesize : t -> int

val write : ?pos:int -> ?len:int -> t -> buf:bytes -> int
val read : ?pos:int -> ?len:int -> t -> buf:bytes -> int

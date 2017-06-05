val set_int : string -> pos:int -> len:int -> int -> unit
val set_ipv4 : string -> pos:int -> int32 -> unit
val set_byte_arr : string -> pos:int -> int list -> unit
val set_bytes : bytes -> pos:int -> bytes -> unit
val int_of_bytes : bytes -> int
val checksum : bytes -> int
val validate : bytes -> bool

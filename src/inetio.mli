val read : unit -> Cstruct.t
val send_cstruct : Cstruct.t -> unit
val send_bytes : ?pos:int -> ?len:int -> bytes -> unit

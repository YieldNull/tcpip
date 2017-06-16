val read : unit -> bytes
val send : bytes -> unit
val send_cstruct : Cstruct.t -> unit

val pop_snd : unit -> bytes

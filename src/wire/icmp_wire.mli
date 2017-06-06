open Cstruct

type typecode =
  | ECHO_REPLY
  | DESTINATION_UNREACHABLE
  | ECHO_REQUEST
  | TIME_EXCEEDED

type echo =
  { id        : uint16;
    seq       : uint16;
    payload   : Cstruct.t;
  }

type t =
  | Echo_requets of echo
  | Echo_reply of echo
  | Destination_unreachable
  | Time_exceeded

val create_echo : ?payload:Cstruct.t -> id:int -> seq:int -> echo

val of_frame : Cstruct.t -> t option
val to_pkg : t -> Cstruct.t

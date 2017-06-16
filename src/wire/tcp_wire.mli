open Cstruct

module Option : sig
  type opcode =
    | END_OF_OPTIONS
    | NO_OPERATION
    | MAX_SEGMENTATION_SIZE
    | WINDOW_SCALE
    | SACK_PERMITTED
    | SACK
    | TIMESTAMPS


  type kind = { kind : uint8; }

  type mss =
    { kind : uint8;
      len  : uint8;
      mss  : uint16;
    }

  type wscale =
    { kind : uint8;
      len  : uint8;
      cnt  : uint8;
    }

  type sackpm =
    { kind : uint8;
      len  : uint8;
    }

  type sack =
    { kind    : uint8;
      len     : uint8;
      blocks  : (uint32 * uint32) list
    }

  type tmstamps =
    { kind  : uint8;
      len   : uint8;
      tsval : uint32;
      tsecr : uint32;
    }

  type t =
    | End_of_options        of kind     (* kind 0 *)
    | No_operation          of kind     (* kind 1 *)
    | Max_segmentation_size of mss      (* kind 2 *)
    | Window_scale          of wscale   (* kind 3 *)
    | Sack_permitted        of sackpm   (* kind 4 *)
    | Sack                  of sack     (* kind 5 *)
    | Timestamps            of tmstamps (* kind 8 *)


  val eol : unit -> kind

  val eol_to_buf : kind -> Cstruct.t

  val noper : unit -> kind

  val noper_to_buf : kind -> Cstruct.t

  val mss : uint16 -> mss

  val mss_to_buf : mss -> Cstruct.t

  val wscale : uint8 -> wscale

  val wscale_to_buf : wscale -> Cstruct.t

  val sackpm : unit -> sackpm

  val sackpm_to_buf : sackpm -> Cstruct.t

  val sack : (uint32 * uint32) list -> sack

  val sack_to_buf : sack -> Cstruct.t

  val tmstamps : uint32 -> uint32 -> tmstamps

  val tmstamps_to_buf : tmstamps -> Cstruct.t

  val list_of_buf : Cstruct.t -> t list

  val list_to_buf : t list -> Cstruct.t
end

type ctrl = URG | ACK | PSH | RST | SYN | FIN

type t =
  { sip     : uint32;
    dip     : uint32;
    sport   : uint16;
    dport   : uint16;
    seq     : uint32;
    ack     : uint32;
    ctrl    : uint8;
    window  : uint16;
    urgent  : uint16;
    options : Option.t list;
    payload : Cstruct.t;
  }

val int_to_ctrl_list : int -> ctrl list
val ctrl_list_to_int : ctrl list -> int
val is_ctrl_set : int -> ctrl -> bool

val of_frame : Cstruct.t -> Ipv4_wire.t -> t option
val to_pkt : t -> Cstruct.t

val to_string : t -> string

val find_opt_mss : t -> int option
val find_opt_wscale : t -> int option

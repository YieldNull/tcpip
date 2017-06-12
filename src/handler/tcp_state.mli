type t =
  | ST_CLOSED
  | ST_LISTEN
  | ST_AT_SYN_SENT
  | ST_AT_SYN_RECEIVED
  | ST_ESTABLISHED
  | ST_FIN_WAIT1
  | ST_FIN_WAIT2
  | ST_CLOSE_WAIT
  | ST_CLOSING
  | ST_LAST_ACK
  | ST_TIME_WAIT

type action =
  | AT_OPEN_ACT
  | AT_OPEN_PAS
  | AT_CLOSE
  | AT_SYN
  | AT_ACK
  | AT_FIN
  | AT_RST
  | AT_SYN_ACK
  | AT_TIMEOUT

val ctrl_to_action : int -> action
val trans_state : t -> action -> (t * action option) option

(*
  TCP Connection State Diagram
  https://tools.ietf.org/html/rfc793#page-23

  TCPIP State Transition Diagram
  http://www.cs.northwestern.edu/~agupta/cs340/project2/TCPIP_State_Transition_Diagram.pdf

  Reset Processing
  https://tools.ietf.org/html/rfc793#page-37
*)

open Core

type t =
  | ST_CLOSED
  | ST_LISTEN
  | ST_SYN_SENT
  | ST_SYN_RECEIVED
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

let action_len = 11

let action_to_int = function
  | AT_OPEN_ACT -> 0
  | AT_OPEN_PAS -> 1
  | AT_CLOSE -> 2
  | AT_SYN -> 3
  | AT_ACK -> 4
  | AT_FIN -> 5
  | AT_RST -> 6
  | AT_SYN_ACK -> 7
  | AT_TIMEOUT -> 8

let state_to_int = function
  | ST_CLOSED       -> 0
  | ST_LISTEN       -> 1
  | ST_SYN_SENT     -> 2
  | ST_SYN_RECEIVED -> 3
  | ST_ESTABLISHED  -> 4
  | ST_FIN_WAIT1    -> 5
  | ST_FIN_WAIT2    -> 6
  | ST_CLOSE_WAIT   -> 7
  | ST_CLOSING      -> 8
  | ST_LAST_ACK     -> 9
  | ST_TIME_WAIT    -> 10

let (>>) action target = (action_to_int action), target

let table =
  let arr =
    [|
      ST_CLOSED,       [ AT_OPEN_PAS >> ST_LISTEN, None;
                         AT_OPEN_ACT >> ST_SYN_SENT, Some AT_SYN;
                         AT_RST      >> ST_CLOSED, None ];

      ST_LISTEN,       [ AT_SYN   >> ST_SYN_RECEIVED, Some AT_SYN_ACK;
                         AT_CLOSE >> ST_CLOSED, None;
                         AT_RST   >> ST_LISTEN, None ];
      (* The state transition from LISTEN to SYN_SENT is legal in the TCP protocol
         but is not supported by Berkeley sockets and is rarely seen. *)

      ST_SYN_SENT,     [ AT_SYN     >> ST_SYN_RECEIVED, Some AT_SYN_ACK;
                         AT_SYN_ACK >> ST_ESTABLISHED, Some AT_ACK;
                         AT_CLOSE   >> ST_CLOSED, None;
                         AT_TIMEOUT >> ST_CLOSED, None;
                         AT_RST     >> ST_CLOSED, None];

      ST_SYN_RECEIVED, [ AT_ACK      >> ST_ESTABLISHED, None;
                         AT_CLOSE    >> ST_FIN_WAIT1, Some AT_FIN;
                         AT_RST      >> ST_LISTEN, None ];
      (* If the receiver was in SYN-RECEIVED state
         and had previously been in the LISTEN state,
         then the receiver returns to the LISTEN state,
         otherwise the receiver aborts the connection and goes to the CLOSED state. *)
      (* simultaneous open is rare, so simply set state to LISTEN *)

      ST_ESTABLISHED,  [ AT_ACK     >> ST_ESTABLISHED, Some AT_ACK;
                         AT_CLOSE   >> ST_FIN_WAIT1, Some AT_FIN;
                         AT_FIN     >> ST_CLOSE_WAIT, Some AT_ACK;
                         AT_RST     >> ST_CLOSED, None ];

      ST_FIN_WAIT1,    [ AT_ACK >> ST_FIN_WAIT2, None;
                         AT_FIN >> ST_TIME_WAIT, Some AT_ACK;
                         AT_RST >> ST_CLOSED, None ];

      ST_FIN_WAIT2,    [ AT_FIN >> ST_TIME_WAIT, Some AT_ACK;
                         AT_RST >> ST_CLOSED, None ];

      ST_CLOSE_WAIT,   [ AT_CLOSE >> ST_LAST_ACK, Some AT_FIN;
                         AT_RST   >> ST_CLOSED, None ];

      ST_CLOSING,      [ AT_ACK >> ST_TIME_WAIT, None;
                         AT_RST >> ST_CLOSED, None ];

      ST_LAST_ACK,     [ AT_ACK >> ST_CLOSED, None;
                         AT_RST >> ST_CLOSED, None ];

      ST_TIME_WAIT,    [ AT_TIMEOUT >> ST_CLOSED, None;
                         AT_RST     >> ST_CLOSED, None ];
    |]
  in
  Array.map arr ~f:(fun tuple ->
      let actions = Array.create ~len:action_len None in
      let _, table = tuple in
      List.iter table ~f:(fun e ->
          let (action_index, state), sending = e in
          Array.set actions action_index (Some (state, sending))
        );
      actions
    )

let ctrl_to_action num =
  let open Tcp_wire in
  if is_ctrl_set num SYN then
    if is_ctrl_set num ACK then AT_SYN_ACK else AT_SYN
  else if is_ctrl_set num FIN then AT_FIN
  else if is_ctrl_set num RST then AT_RST
  else AT_ACK

let trans_state state action =
  let trans_table = Array.get table (state_to_int state) in
  Array.get trans_table (action_to_int action)

open Ipaddr

let devname = "tap0"
let macaddr = ref Macaddr.broadcast
let ipaddr = ref V4.any

let get_ipaddr () = !ipaddr

let get_macaddr () = !macaddr

let set_ipaddr addr = ipaddr := addr

let set_macaddr addr = macaddr := addr

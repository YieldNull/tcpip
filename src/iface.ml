open Ipaddr
open Core

type t =
  { devname : string;
    mutable macaddr : Caml.Bytes.t;
    mutable ipaddr  : int32;
    mutable netmask : int32;
    mutable router  : int32;
    mutable dns : int32 list;
    arp_cache : (int32, string) Hashtbl.t;
  }


let tap =
  { devname = "tap0";
    macaddr = Macaddr.to_bytes Macaddr.broadcast;
    ipaddr  = V4.to_int32 V4.any;
    netmask = V4.to_int32 V4.any;
    router  = V4.to_int32 V4.routers;
    dns = [];
    arp_cache = Hashtbl.create ~hashable:Int32.hashable ();
  }

let bridge = "bridge1"

let fd = Unix.openfile ("/dev/" ^ tap.devname) ~mode:[Unix.O_RDWR]

external stub_get_macaddr : string -> string = "stub_get_macaddr"

let init () =
  tap.macaddr <- stub_get_macaddr tap.devname;
  ignore @@ Unix.system @@
  sprintf "ifconfig %s up && ifconfig %s addm %s" tap.devname bridge tap.devname

let setup ipaddr netmask router dns =
  tap.ipaddr <- ipaddr;
  tap.netmask <- netmask;
  tap.router <- router;
  tap.dns <- dns;
  ignore @@ Unix.system @@
  sprintf "ifconfig %s %s netmask %s"
    tap.devname
    (V4.to_string @@ V4.of_int32 tap.ipaddr)
    (V4.to_string @@ V4.of_int32 tap.netmask)

let devname () = tap.devname
let macaddr () = tap.macaddr
let ipaddr  () = tap.ipaddr
let netmask () = tap.netmask
let router  () = tap.router
let arp_cache () = tap.arp_cache

let arp_find ip = Hashtbl.find tap.arp_cache ip
let arp_set ip mac = Hashtbl.set tap.arp_cache ~key:ip ~data:mac
let arp_add_exn ip mac = Hashtbl.add_exn tap.arp_cache ~key:ip ~data:mac

open Ipaddr
open Core
open Async

type t =
  { devname : string;
    mutable macaddr : Macaddr.t;
    mutable ipaddr  : V4.t;
    mutable netmask : V4.t;
    mutable router  : V4.t;
    mutable dns : V4.t list;
    arp_cache : (int32, string) Hashtbl.t;
  }


let tap =
  { devname = "tap0";
    macaddr = Macaddr.broadcast;
    ipaddr  = V4.any;
    netmask = V4.any;
    router  = V4.routers;
    dns = [];
    arp_cache = Hashtbl.create ~hashable:Int32.hashable ();
  }

let bridge = "bridge1"

let filename = "/dev/" ^ tap.devname

external stub_get_macaddr : string -> string = "stub_get_macaddr"

let init () =
  tap.macaddr <- Macaddr.of_bytes_exn @@ stub_get_macaddr tap.devname;
  Unix.system @@
  sprintf "ifconfig %s up && ifconfig %s addm %s" tap.devname bridge tap.devname

let setup ipaddr netmask router dns =
  tap.ipaddr <- ipaddr;
  tap.netmask <- netmask;
  tap.router <- router;
  tap.dns <- dns;
  Unix.system @@
  sprintf "ifconfig %s %s netmask %s"
    tap.devname (V4.to_string tap.ipaddr) (V4.to_string tap.netmask)

let devname () = tap.devname
let macaddr () = tap.macaddr
let ipaddr  () = tap.ipaddr
let netmask () = tap.netmask
let router  () = tap.router
let arp_cache () = tap.arp_cache

let arp_find ip = Hashtbl.find tap.arp_cache ip
let arp_set ip mac = Hashtbl.set tap.arp_cache ~key:ip ~data:mac
let arp_add_exn ip mac = Hashtbl.add_exn tap.arp_cache ~key:ip ~data:mac

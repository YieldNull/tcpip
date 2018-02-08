A TCP/IP stack written in [OCaml](https://ocaml.org/), based on linux tap device.

## Setup On Mac OS X

Install tuntap from [tuntaposx](http://tuntaposx.sourceforge.net/download.xhtml)

Create a bridge with member `en0`. Then build and run the driver program.

The `tap0` device will be added to the bridge, and get an IP address from the DHCP Server.

```
$ sudo ifconfig bridge1 create
$ sudo ifconfig bridge1 addm en0
$ sudo ./build src/driver.native
$ sudo ./driver.native
```

## TODO

- [x] Ethernet
- [x] ARP
- [x] IPV4
- [x] UDP
- [x] DHCP
- [x] ICMP
- [x] TCP
  - [x] State Transition
  - [x] Connection Management
  - [ ] Flow Control
  - [ ] Congestion Control

## LICENSE

MIT License

/*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <net/if.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#ifdef __FreeBSD__
#include <netinet/in.h>
#endif /* __FreeBSD__ */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>

#if defined(__linux__)
#include <linux/if_tun.h>

static void tun_raise_error(char *prefix, int fd) {
  char buf[1024];
  snprintf(buf, sizeof(buf) - 1, "tun[%s]: %s", prefix, strerror(errno));
  buf[sizeof(buf) - 1] = '\0';
  if (fd >= 0)
    close(fd);
  caml_failwith(buf);
}

CAMLprim value stub_get_macaddr(value devname) {
  CAMLparam1(devname);
  CAMLlocal1(hwaddr);

  int fd;
  struct ifreq ifq;

  fd = socket(PF_INET, SOCK_DGRAM, 0);
  strcpy(ifq.ifr_name, String_val(devname));

  if (ioctl(fd, SIOCGIFHWADDR, &ifq) == -1)
    tun_raise_error("SIOCGIFHWADDR", fd);

  close(fd);

  hwaddr = caml_alloc_string(6);
  memcpy(String_val(hwaddr), ifq.ifr_hwaddr.sa_data, 6);

  CAMLreturn(hwaddr);
}

#elif (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__)
#include <ifaddrs.h>
#include <net/if_dl.h>

CAMLprim value stub_get_macaddr(value devname) {
  CAMLparam1(devname);
  CAMLlocal1(v_mac);

  struct ifaddrs *ifap, *p;
  char *mac_addr[6];
  int found = 0;
  char name[IFNAMSIZ];
  snprintf(name, sizeof name, "%s", String_val(devname));

  if (getifaddrs(&ifap) != 0) {
    err(1, "get_mac_addr");
  }

  for (p = ifap; p != NULL; p = p->ifa_next) {
    if ((strcmp(p->ifa_name, name) == 0) && (p->ifa_addr != NULL)) {
      char *tmp = LLADDR((struct sockaddr_dl *)(p)->ifa_addr);
      memcpy(mac_addr, tmp, 6);
      found = 1;
      break;
    }
  }

  freeifaddrs(ifap);
  if (!found)
    err(1, "get_mac_addr");

  v_mac = caml_alloc_string(6);
  memcpy(String_val(v_mac), mac_addr, 6);
  CAMLreturn(v_mac);
}
#endif

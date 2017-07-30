open Mirage

(** Always compile DNS zone file data into the unikernel *)
let data = crunch "./data"

let stack =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (static_ipv4_stack ~arp:farp default_network)

let logger = syslog_udp stack

(** Define the shape of the service.  We depend on the `dns`
    package, and it requires a logging console, a read-only
    key/value store and a TCP/IP stack. *)
let dns_handler =
  let packages = [
    package ~min:"0.20.0" ~sublibs:["mirage"] "dns";
    package "duration"
  ] in
  foreign
    ~deps:[abstract logger]
    ~packages
    "Unikernel.Main" (kv_ro @-> stackv4 @-> job)

let () =
  register "dns" [dns_handler $ data $ stack]

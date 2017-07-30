open Mirage

(** Always compile DNS zone file data into the unikernel *)
let data = crunch "./data"

(** Define the shape of the service.  We depend on the `dns`
    package, and it requires a logging console, a read-only
    key/value store and a TCP/IP stack. *)
let dns_handler =
  let packages = [
    package ~min:"0.20.0" ~sublibs:["mirage"] "dns";
    package "duration"
  ] in
  foreign
    ~packages
    "Unikernel.Main" (kv_ro @-> stackv4 @-> job)

let stack =
  if_impl Key.is_unix
    (socket_stackv4 [Ipaddr.V4.any])
    (generic_stackv4 default_network)

let () =
  register "dns" [dns_handler $ data $ stack]

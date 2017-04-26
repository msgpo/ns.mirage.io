open Lwt.Infix

let server_src = Logs.Src.create "server" ~doc:"DNS server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

(* Server settings *)
let listening_port = 53

module Main (K:Mirage_types_lwt.KV_RO) (S:Mirage_types_lwt.STACKV4) = struct

  module U = S.UDPV4
  module Resolver = Dns_resolver_mirage.Make(OS.Time)(S)

  let load_zone k =
    K.size k "mirage.io.zone"
    >>= function
    | Error _ -> Lwt.fail (Failure "test.zone not found")
    | Ok sz ->
      Server_log.info (fun f -> f "Loading %Ld bytes of zone data" sz);
      K.read k "mirage.io.zone" 0L sz
      >>= function
      | Error _ -> Lwt.fail (Failure "test.zone error reading")
      | Ok pages -> Lwt.return (Cstruct.concat pages |> Cstruct.to_string)

  let serve s zonebuf =
    let open Dns_server in
    let process = process_of_zonebuf zonebuf in
    let processor = (processor_of_process process :> (module PROCESSOR)) in
    let udp = S.udpv4 s in
    S.listen_udpv4 s ~port:listening_port (
      fun ~src ~dst ~src_port buf ->
        Server_log.info (fun f -> f "Got DNS query via UDP");
        let src' = (Ipaddr.V4 dst), listening_port in
        let dst' = (Ipaddr.V4 src), src_port in
        process_query buf (Cstruct.len buf) src' dst' processor >>= function
        | None ->
          Server_log.info (fun f -> f "No response");
          Lwt.return ()
        | Some rbuf ->
          Server_log.info (fun f -> f "Sending reply");
          U.write ~src_port:listening_port ~dst:src ~dst_port:src_port udp rbuf >>= function
          | Error e -> Server_log.warn (fun f -> f "Failure sending reply: %a" U.pp_error e);
            Lwt.return_unit
          | Ok () -> Lwt.return ()
    );
    Server_log.info (fun f -> f "DNS server listening on UDP port %d" listening_port);
    S.listen s

  let start kv_store stack =
    Logs.(set_level (Some Info));
    load_zone kv_store >>= fun zonebuf ->
    serve stack zonebuf
end

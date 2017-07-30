open Lwt.Infix

let src = Logs.Src.create "server" ~doc:"DNS server"
module Log = (val Logs.src_log src : Logs.LOG)

(* Server settings *)
let listening_port = 53

module Main (K:Mirage_types_lwt.KV_RO) (S:Mirage_types_lwt.STACKV4) = struct

  module U = S.UDPV4
  module Resolver = Dns_resolver_mirage.Make(OS.Time)(S)

  let load_zone k =
    K.size k "mirage.io.zone" >>= function
    | Error _ -> Lwt.fail (Failure "test.zone not found")
    | Ok sz ->
      Log.info (fun f -> f "Loading %Ld bytes of zone data" sz);
      K.read k "mirage.io.zone" 0L sz >>= function
      | Error _ -> Lwt.fail (Failure "test.zone error reading")
      | Ok pages -> Lwt.return (Cstruct.concat pages |> Cstruct.to_string)

  let to_str b = try Dns.Packet.(to_string (parse b)) with _ -> "parse error"

  let serve s zonebuf =
    let open Dns_server in
    let process = process_of_zonebuf zonebuf in
    let processor = (processor_of_process process :> (module PROCESSOR)) in
    let udp = S.udpv4 s in
    S.listen_udpv4 s ~port:listening_port (
      fun ~src ~dst ~src_port buf ->
        Log.info (fun f -> f "%a:%d query %s"
                     Ipaddr.V4.pp_hum src src_port (to_str buf));
        let src' = Ipaddr.V4 dst, listening_port in
        let dst' = Ipaddr.V4 src, src_port in
        process_query buf (Cstruct.len buf) src' dst' processor >>= function
        | None ->
          Log.info (fun f -> f "%a:%d no response"
                       Ipaddr.V4.pp_hum src src_port);
          Lwt.return ()
        | Some rbuf ->
          Log.info (fun f -> f "%a:%d reply %s"
                       Ipaddr.V4.pp_hum src src_port (to_str rbuf));
          let dst_port = src_port
          and src_port = listening_port
          in
          U.write ~src_port ~dst:src ~dst_port udp rbuf >>= function
          | Error e ->
            Log.warn (fun f -> f "%a:%d failure sending reply: %a"
                         Ipaddr.V4.pp_hum src src_port U.pp_error e);
            Lwt.return_unit
          | Ok () -> Lwt.return ()
    );
    Log.info (fun f -> f "DNS server listening on UDP port %d" listening_port);
    S.listen s

  let start kv_store stack _ =
    load_zone kv_store >>= fun zonebuf ->
    serve stack zonebuf
end

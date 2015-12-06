open Lwt

open V1
open V1_LWT


type ('a, 'e, 'c) m = ([< `Ok of 'a | `Error of 'e | `Eof ] as 'c) Lwt.t

let (>>==) (a : ('a, 'e, _) m) (f : 'a -> ('b, 'e, _) m) : ('b, 'e, _) m =
  a >>= function
    | `Ok x                -> f x
    | `Error _ | `Eof as e -> return e


module Color = struct
  open Printf
  let red    fmt = sprintf ("\027[31m"^^fmt^^"\027[m")
  let green  fmt = sprintf ("\027[32m"^^fmt^^"\027[m")
  let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
  let blue   fmt = sprintf ("\027[36m"^^fmt^^"\027[m")
end


module Log (C: CONSOLE) = struct

  let log_trace c str = C.log_s c (Color.green "+ %s" str)

  and log_data c str buf =
    let repr = String.escaped (Cstruct.to_string buf) in
    C.log_s c (Color.blue "  %s: " str ^ repr)
  and log_error c e = C.log_s c (Color.red "+ err: %s" e)

end

let make_tracer dump =
  let traces = ref [] in
  let trace sexp =
    traces := Sexplib.Sexp.to_string_hum sexp :: !traces
  and flush () =
    let msgs = List.rev !traces in
    traces := [] ;
    Lwt_list.iter_s dump msgs in
  (trace, flush)

module Client (C  : CONSOLE)
              (Resolver : Resolver_lwt.S)
              (Conduit : Conduit_mirage.S)
              (KV : KV_RO) =
struct

  module X509 = Tls_mirage.X509 (KV) (Clock)
  module L    = Log (C)

  open Ipaddr

  let opportunistically_connect c uri res con =
    C.log c ("Attempting connection to " ^ (Uri.to_string uri));
    let ctx = Cohttp_mirage.Client.ctx res con in
    Cohttp_mirage.Client.get ~ctx uri >>= fun (response, body) ->
    C.log c (Printf.sprintf "opportunistically encrypted connection to %s succeeded!" 
               (Uri.to_string uri));
    let response_metadata = Sexplib.Sexp.to_string_hum
        (Cohttp.Response.sexp_of_t response) in
    C.log c ("response metadata: "  ^ response_metadata);
    Cohttp_lwt_body.to_string body >>= fun body ->
    C.log c ("response body: " ^ body);
    C.log c "All done.";
    Lwt.return_unit

  let scrupulously_connect c uri host res con =
    let initial = Cstruct.of_string @@
      "GET / HTTP/1.1\r\nConnection: Close\r\nHost: " ^ host ^ "\r\n\r\n" in
    let chat c tls =
      let rec dump () =
        Conduit_mirage.Flow.read tls >>== fun buf ->
        L.log_data c "recv" buf >> dump () in
      Conduit_mirage.Flow.write tls initial >> dump ()
    in
    Resolver_lwt.resolve_uri uri res >>= function
    | `Unix_domain_socket _ | `Unknown _ | `Vchan_direct _ |
      `Vchan_domain_socket _ -> L.log_error c "Endpoint resolved to a non-network conduit type"
    | `TCP endp -> L.log_error c "Endpoint resolved to plain TCP; aborting"
    | `TLS (name, endp) ->
      match endp with
      | `Unix_domain_socket _ | `Unknown _ | `Vchan_direct _ | 
        `Vchan_domain_socket _ -> L.log_error c "TLS-wrapped endpoint resolved to a non-network conduit type"
      | `TLS _ -> L.log_error c "TLS-wrapped endpoint claims to be TLS itself"
      | `TCP (server_ip, port) when port <> 443 -> L.log_error c "TCP-wrapped endpoint is not on port 443"
      | `TCP (server_ip, port) when port = 443 ->
        C.log c ("Have verified that Conduit will contact this endpoint via TLS: " ^ (Ipaddr.to_string server_ip));
        Conduit_mirage.client (`TLS (name, endp)) >>= fun client ->
        C.log c "Conduit client setup completed";
        C.log c "Attempting connection...";
        Conduit_mirage.connect con client >>= fun tls ->
        chat c tls
        >>= function
        | `Error e -> L.log_error c (Conduit_mirage.Flow.error_message e)
        | `Eof     -> L.log_trace c "eof."
        | `Ok _    -> assert false

  let start c res con kv =
    let uri = Uri.make ~scheme:"https" ~host:"mirage.io" ~port:443 ~path:"/" () in
    opportunistically_connect c uri res con >>= fun () ->
    scrupulously_connect c uri "mirage.io" res con >>= fun () ->
    Lwt.return_unit
end

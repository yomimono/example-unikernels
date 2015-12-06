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

  let start c res con kv =
    let uri = Uri.make ~scheme:"https" ~host:"mirage.io" ~port:443 ~path:"/" () in
    opportunistically_connect c uri res con >>= fun () ->
    Lwt.return_unit
end

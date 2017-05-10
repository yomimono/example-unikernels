module App (Clock : Mirage_clock.PCLOCK)
            (Server : Cohttp_lwt.Server) = struct

open Lwt.Infix

let app_log_src = Logs.Src.create "credits" ~doc:"roll credits!"
module App_log = (val Logs.src_log app_log_src : Logs.LOG)

(* Apply the [Webmachine.Make] functor to the Lwt_unix-based IO module
 * exported by cohttp. For added convenience, include the [Rd] module
 * as well so you don't have to go reaching into multiple modules to
 * access request-related information. *)
module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Server.IO)
end

(* Create a new class that inherits from [Wm.resource] and provides
 * implementations for its two virtual methods, and overrides some of
 * its default methods.
 *)
class credits = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  (* Only allow GET requests to this resource *)
  method allowed_methods rd =
    Wm.continue [`GET] rd

  (* Setup the resource to handle multiple content-types. Webmachine will
   * perform content negotiation as described in RFC 7231:
   *
   *   https://tools.ietf.org/html/rfc7231#section-5.3.2
   *
   * Content negotiation can be a complex process. However for simple Accept
   * headers its fairly straightforward. Here's what content negotiation will
   * produce in some of these simple cases:
   *
   *     Accept             | Called method
   *   ---------------------+----------------
   *     "text/plain"       | self#to_text
   *     "text/html"        | self#to_html
   *     "text/*"           | self#to_html
   *     "application/json" | self#to_json
   *     "application/*"    | self#to_json
   *     "*/*"              | self#to_html
   *)
  method content_types_provided rd =
    Wm.continue [
      ("text/html"       , self#to_html);
      ("text/plain"      , self#to_text);
    ] rd

  (* Since only GET requests are allowed, there's no need to provide handlers
   * for requests containing certain content types. This method will never be
   * called, but it's necessary to provide an implementation since it's
   * [virtual] in the [Wm.resource] virtual class. *)
  method content_types_accepted rd =
    Wm.continue [] rd

  method process_post rd =
    Cohttp_lwt_body.to_string rd.Wm.Rd.req_body >>= fun _body ->
    let rd = Wm.Rd.redirect "/" rd in
    Wm.continue true rd

  (* A helper method that returns what to say hello to. If a path wildcard
   * called ["what"] was introduced by a route, it will use whatever string in
   * that position. Otherwise, it defaults to the string ["world"]. *)
  method private what rd =
    try
      let name = Wm.Rd.lookup_path_info_exn "what" rd in
      let l = 
        match name with
        | "solo5" -> Names.solo5
        | "packaging" -> Names.packaging
        | "docs" -> Names.docs
        | "results" -> Names.results
        | "logs" -> Names.logs
        | "disaggregated_module_types" | "module" | "module_types"
          -> Names.disaggregated_module_types
        | "clocks_and_time" | "clocks" | "time" -> Names.clocks_and_time
        | "topkg" -> Names.topkg
        in
      (name, l)
    with Match_failure _ | Not_found -> ("contributors", Names.contributors)

  (* Returns an html-based representation of the resource *)
  method private to_html rd =
    App_log.debug (fun f -> f "generating response with %s\n%!"
      @@ fst (self#what rd));
    let body =
      let header, people = self#what rd in
let li fmt = Format.fprintf fmt "<li>%s</li>" in
      Format.asprintf
        "<html>
<head><style media=\"screen\" type=\"text/css\">%s</style></head>
<body><div id=\"titles\"><div id=\"titlecontent\">
<h1>%s</h1>
<ul>%a</ul>
        </div></div>
</body></html>\n"
        Css.crawl_style
        header Fmt.(list li) people
    in
    Wm.continue (`String body) rd

  (* Returns a plaintext representation of the resource *)
  method private to_text rd =
    let name, people = self#what rd in
    let text = Format.asprintf "%s: %a!" name Fmt.(list string) people in
    Wm.continue (`String text) rd

end

let start _clock http =
  (* Listen on port 8080 *)
  let port = Key_gen.http_port () in
  (* The route table. Both routes use the [hello] resource defined above.
   * However, the second one contains the [:what] wildcard in the path. The
   * value of that wildcard can be accessed in the resource by calling
   *
   *   [Wm.Rd.lookup_path_info "what" rd]
   *)
  let routes = [
     ("/:what"      , fun () -> new credits);
  ] in
  let callback (_ch,_conn) request body =
    let open Cohttp in
    (* Perform route dispatch. If [None] is returned, then the URI path did not
     * match any of the route patterns. In this case the server should return a
     * 404 [`Not_found]. *)
    Wm.dispatch' routes ~body ~request
    >|= begin function
      | None        -> (`Not_found, Header.init (), `String "Not found", [])
      | Some result -> result
    end
    >>= fun (status, headers, body, path) ->
      App_log.debug (fun f -> f "%d - %s %s (path: %s)"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
        (String.concat ", " path));
      (* Finally, send the response to the client *)
      Server.respond ~headers ~body ~status ()
  in
  (* Create the server and handle requests with the function defined above. Try
   * it out with some of these curl commands:
   *
   *   [curl -H"Accept:text/html" "http://localhost:8080"]
   *   [curl -H"Accept:text/plain" "http://localhost:8080"]
   *   [curl -H"Accept:application/json" "http://localhost:8080"]
  *)
  let conn_closed (_ch,conn) =
    App_log.debug (fun f -> f "connection %s closed\n%!"
      @@ Cohttp.Connection.to_string conn)
  in
  let config = Server.make ~callback ~conn_closed () in
  (* Server.create  ~mode:(`TCP(`Port port)) config >|= fun () ->
    Printf.eprintf "hello_lwt: listening on 0.0.0.0:%d%!" port *)
  let mode = `TCP port in
  http mode config

end

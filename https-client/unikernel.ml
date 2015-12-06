open Lwt

open V1
open V1_LWT
open Sexplib.Conv

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

module Github_clock(Clock : V1.CLOCK) (Time : V1_LWT.TIME) : Github_s.Time = struct
  let now () = Clock.time ()
  let sleep n = Time.sleep n
end

module Client (C  : CONSOLE)
              (Clock : V1.CLOCK)
              (Time : V1_LWT.TIME)
              (Resolver : Resolver_lwt.S)
              (Conduit : Conduit_mirage.S)
              (KV : KV_RO) =
struct

  module L    = Log (C)
  module GHC = Github_clock(Clock)(Time)

  let start c _clock _time res con kv =
    let module Resolving_client : Cohttp_lwt.Client = struct

      module Channel = Channel.Make(Conduit_mirage.Flow)
      module HTTP_IO = Cohttp_mirage_io.Make(Channel)

      module Net_IO = struct

        module IO = HTTP_IO

        type 'a io = 'a Lwt.t
        type ic = Channel.t
        type oc = Channel.t
        type flow = Conduit_mirage.Flow.flow

        type ctx = {
          resolver: Resolver_lwt.t;
          conduit : Conduit_mirage.t;
        }

        let sexp_of_ctx { resolver; _ } = Resolver_lwt.sexp_of_t resolver

        let default_ctx =
          { resolver = res; conduit = con }

        let connect_uri ~ctx uri =
          Resolver_lwt.resolve_uri ~uri ctx.resolver >>= fun endp ->
          Conduit_mirage.client endp >>= fun client ->
          Conduit_mirage.connect ctx.conduit client >>= fun flow ->
          let ch = Channel.create flow in
          return (flow, ch, ch)

        let _close channel = Lwt.catch (fun () -> Channel.close channel) (fun _ ->
            return_unit)
        let close_in ic = ignore_result (_close ic)
        let close_out ic = ignore_result (_close ic)
        let close ic oc = ignore_result (_close ic >>= fun () -> _close oc)

      end
      let ctx resolver conduit = { Net_IO.resolver; conduit }

      (* Build all the core modules from the [Cohttp_lwt] functors *)
      include Cohttp_lwt.Make_client(HTTP_IO)(Net_IO)

    end in
    let module Github = Github_core.Make(GHC)(Resolving_client) in
    let get_token () = Github.(Monad.(run (
        let note = "get_token via ocaml-github" in                                    
        Token.create ~user:Github_creds.username ~pass:Github_creds.password ~note ()
        >>~ function                                                                  
        | Result auth ->                                                              
          let token = Token.of_auth auth in                                           
          prerr_endline (Token.to_string token);                                      
          return ()                                                                   
        | Two_factor _ -> embed (fail (Failure "get_token doesn't support 2fa, yet")) 
      )))
    in
    let user token =
      Github.(Monad.(run (User.current_info ~token ()
                                     >|= Response.value))) >>= fun user ->
      Lwt.return (Github_j.string_of_user_info user)
    in
    let issues token user repo =
      let open Github in
      let open Monad in
      let issues = Issue.for_repo ~token ~user ~repo () in
      Stream.iter (fun issue ->
          let num = Github_t.(issue.issue_number) in
          C.log c (Printf.sprintf "issue %d: %s\n%!" num Github_t.(issue.issue_title));
          let issue_comments = Issue.comments ~token ~user ~repo ~num () in
          Stream.to_list issue_comments
          >>= fun comments ->
          List.iter (fun comment ->
              C.log c Github_t.(Printf.sprintf "  > %Ld: %s\n"
                                  comment.issue_comment_id comment.issue_comment_body)
            ) comments;
          return ()
        ) issues
    in
    let repositories token =
      let user = "yomimono" in
      Github.(Monad.(run (
        let repos = User.repositories ~token ~user () in
        Stream.iter (fun repo ->
            C.log c Github_t.(repo.repository_full_name);
            match Github_t.(repo.repository_has_issues) with
            | true ->
              issues token user (Github_t.(repo.repository_name)) >>= fun _ ->
              return ()
            | false ->
              return ()
        ) repos
        )))
    in
    let token = Github.Token.of_string Github_creds.oauth_token in
    user token >>= fun user -> C.log c ("User: " ^ user);
    repositories token >>= fun () ->
    (* get_token () >>= fun () -> *)

    Lwt.return_unit
end

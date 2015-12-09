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
    let module Resolving_client = struct

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
    let module Context = struct
      let v () =
        Lwt.return (Some (res, con))
    end in
    let module Inflator = struct
      module IDBytes = struct
        include Bytes
        let concat = String.concat
        let of_bytes t = t
        let to_bytes t = t
      end
      module XInflator = Decompress.Inflate.Make(IDBytes)
      module XDeflator = Decompress.Deflate.Make(IDBytes)

      let inflate ?output_size buf =
        let output = match output_size with
          | None -> Bytes.create (Mstruct.length buf)
          | Some n -> Bytes.create n
        in
        let inflator = XInflator.make (`String (0, (Mstruct.to_string buf))) output in
        let rec eventually_inflate inflator acc =
          match XInflator.eval inflator with
          | `Ok n ->
            let res = Mstruct.of_string (IDBytes.concat "" (List.rev acc)) in
            Mstruct.shift buf n;
            Some res
          | `Error -> None
          | `Flush n ->
            match XInflator.contents inflator with
            | 0 -> 
              let res = Mstruct.of_string (IDBytes.concat "" (List.rev acc)) in
              Some res
            | _ ->
              let tmp = Bytes.copy output in
              XInflator.flush inflator;
              eventually_inflate inflator (tmp :: acc)
        in
        eventually_inflate inflator []

      let deflate ?level buf =
        let output = Bytes.create 62 in
        let deflator = XDeflator.make ~window_bits:15
            (`String (0, (Cstruct.to_string buf))) output in
        let rec eventually_deflate deflator acc =
          match XDeflator.eval deflator with
          | `Ok ->
            C.log c (Printf.sprintf "%d items in the accumulator for deflation\n" (List.length acc));
            let res = IDBytes.concat "" (List.rev acc) in
            C.log c (Printf.sprintf "deflated buffer of %d bytes to %d bytes"
                       (Cstruct.len buf) (String.length res));
            Cstruct.of_string res
          | `Error -> failwith "Error deflating an archive :("
          | `Flush n ->
            C.log c (Printf.sprintf "flushing %d (%d) bytes from %d byte buffer" n
                       (XDeflator.contents deflator)
                       (Bytes.length output));
            let tmp = Bytes.create n in
            Bytes.blit_string output 0 tmp 0 n;
            XDeflator.flush deflator;
            eventually_deflate deflator (tmp :: acc)
        in
        eventually_deflate deflator []

    end in
    let module Mirage_git_memory =
      Irmin_mirage.Irmin_git.Memory(Context)(Inflator) in
    let module Store =
      Mirage_git_memory(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
    in
     let module Sync = Irmin.Sync(Store) in
        let config = Irmin_mirage.Irmin_git.config () in
        let task = Irmin.Task.create ~date:(Int64.of_int (int_of_float (Clock.time ())))
            ~owner:"MirageOS Irmin Scraperbot" in

    let issues branch token user repo =
      let store branch path value =
        Store.update (branch "updating with issue body") path value
      in
      let open Github in
      let open Monad in
      let issues = Issue.for_repo ~token ~user ~repo () in
      Stream.iter (fun issue ->
          let issue_id = Github_t.(issue.issue_number) in
          C.log c (Printf.sprintf "issue %d: %s\n%!" issue_id Github_t.(issue.issue_title));
          let issue_comments = Issue.comments ~token ~user ~repo ~num:issue_id () in
          Stream.to_list issue_comments
          >>= fun comments -> embed (
          Lwt_list.iter_p (fun comment ->
              let comment_id = Int64.to_int (Github_t.(comment.issue_comment_id)) in
              let path = Irmin.Path.String_list.of_hum (Printf.sprintf
                                                          "%s/%s/issues/%L/%L"
                                                          user repo issue_id comment_id) in
              Github_t.(store branch path comment.issue_comment_body)
            ) comments )
        ) issues
    in
    let scrape_issues user token branch =
      let user = "yomimono" in
      Github.(Monad.(run (
        let repos = User.repositories ~token ~user () in
        Stream.iter (fun repo ->
            C.log c Github_t.(repo.repository_full_name);
            match Github_t.(repo.repository_has_issues) with
            | true ->
              issues branch token user (Github_t.(repo.repository_name))
            | false ->
              return ()
        ) repos
        )))
    in
    KV.read kv "token" 0 4096 >>= function
    | `Error _ | `Ok [] | `Ok (_::_::_) -> L.log_error c "kv_ro error reading token"
    | `Ok (buf::[]) -> Lwt.return (Github.Token.of_string (Cstruct.to_string buf))
      >>= fun token ->
      let rec read_and_sync primary remote =
        scrape_issues "yomimono" token primary >>= fun () ->
        C.log c "issue scrape to local in-memory store complete; updating local
           backup";
        (* sync irmin to db in dom0 *) 
        Sync.push (primary "push to backup server") remote >>= function
        | `Error -> L.log_error c "error pushing data to remote repository"
          (* >>= fun () -> Time.sleep 1.0 >>= fun () -> read_and_sync primary
             remote *)
        | `Ok -> Time.sleep 1.0 >>= fun () -> read_and_sync primary remote
      in
      C.log c "token read!";
      let remote = Irmin.remote_uri "git://irmin-backup/local_issues" in
      Store.Repo.create config >>= fun repo ->
      Store.master task repo >>= fun primary ->
      Sync.pull (primary "pull from backup server") remote `Update >>= function
      | `Conflict s -> L.log_error c ("conflict: " ^ s)
      | `Ok `Error -> L.log_error c "error syncing before our scrape :("
      | `Ok `No_head ->
        C.log c "no head? no problem!";
        read_and_sync primary remote
      | `Ok `Ok ->
        C.log c "checked the backup server.  commencing scrape!";
        read_and_sync primary remote
end

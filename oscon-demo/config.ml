open Mirage

let stack = generic_stackv4 default_network
(* set ~tls to false to get a plain-http server *)
let https_srv = http_server @@ conduit_direct ~tls:false stack

let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["port"] in
  Key.(create "http_port" Arg.(opt int 80 doc))

let main =
  let packages = [
    package ~min:"0.4.1" "webmachine";
  ] in
  let keys = [ Key.abstract http_port ] in
  foreign
    ~packages ~keys
    "Unikernel.App" (pclock @-> http @-> job)

let () =
  register "app" [main $ default_posix_clock $ https_srv]

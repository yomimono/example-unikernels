open Mirage

let secrets_dir = "sekrit"

let disk =
  match get_mode () with
  | `Unix | `MacOSX -> direct_kv_ro secrets_dir
  | `Xen  -> crunch secrets_dir

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | _        -> `Socket
  with Not_found ->
    match get_mode () with
    | `Unix | `MacOSX -> `Socket
    | `Xen -> `Direct

let dhcp =
  try match Sys.getenv "ADDR" with
    | "static" -> `Static
    | _   -> `Dhcp
  with Not_found -> `Dhcp

let stack console =
  match net with
  | `Direct -> direct_stackv4_with_dhcp console tap0
  | `Socket -> socket_stackv4 console [Ipaddr.V4.any]


let build_stack console =
  let ns = Ipaddr.V4.of_string_exn "208.67.222.222" in
  let stack = stack console in
  (conduit_direct ~tls:true stack), (Mirage.resolver_dns ~ns stack)

let tracing = mprof_trace ~size:1000000 ()

let client =
  foreign "Unikernel.Client" @@ console @-> resolver @-> conduit @-> kv_ro @-> job

let () =
  let (con, res) = build_stack default_console in
  add_to_opam_packages [ "dns" ; "tls" ] ;
  add_to_ocamlfind_libraries [ "dns.mirage"; "tls"; "tls.mirage" ] ;
  register ~tracing "tls-client" [ client $ default_console $ res $ con $ disk ]

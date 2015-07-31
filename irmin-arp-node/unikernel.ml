open Lwt.Infix

let netmask = Ipaddr.V4.of_string_exn "255.255.255.0"

let echo_port = 7

let pester_interval = 5.0
let crosstalk_interval = 90.0

let root = "demo_results"

let strip = Ipaddr.V4.to_string

module Client (C: V1_LWT.CONSOLE) (N: V1_LWT.NETWORK) (Clock: V1.CLOCK)
    (Random: V1.RANDOM) = struct
  module Time = OS.Time
  module E = Ethif.Make(N)
  module A = Irmin_arp.Arp.Make(E)(Clock)(Time)(Random)(Irmin_mem.Make)
  module IPV4 = Ipv4.Make(E)(A)
  module TCP = Tcp.Flow.Make(IPV4)(Time)(Clock)(Random)

  let ignore_errors c fn = function
    | `Ok q -> fn q
    | `Error _ -> C.log_s c "Server: error reading or writing from flow"
    | `Eof -> C.log_s c "Server: EOF reading or writing from flow"

  let get_arp ~netif ~root ~node ?(pull=[]) () =
    E.connect netif >>= function
    | `Error _ -> Lwt.fail (failwith "Ethif.connect failed!")
    | `Ok ethif ->
      (* for now, assume Irmin_mem *)
      let config = Irmin_mem.config () in
      A.connect ethif config ~node:[node] ~pull >>= function
      | `Ok arp -> Lwt.return (netif, ethif, arp)
      | `Error _ -> Lwt.fail (failwith "Arp.connect failed!")

  let start_ip ip_addr (netif, ethif, arp) =
    IPV4.connect ethif arp >>= function
    | `Error e -> Lwt.fail (failwith (Printf.sprintf "error starting ip %s"
                                          (Ipaddr.V4.to_string ip_addr)))
    | `Ok i ->
      IPV4.set_ip i ip_addr >>= fun () -> IPV4.set_ip_netmask i netmask >>= fun () ->
      Lwt.return (netif, ethif, arp, i)

  let arp_and_tcp_listeners netif ethif arp ip tcp () =
    N.listen netif (E.input
                      ~ipv6:(fun buf -> Lwt.return_unit)
                      ~arpv4:(fun buf -> A.input arp buf)
                      ~ipv4:(
                        IPV4.input
                          ~tcp:(TCP.input tcp ~listeners:(fun _ -> None))
                          ~udp:(fun ~src ~dst _buf -> Lwt.return_unit)
                          ~default:(fun ~proto ~src ~dst _ -> Lwt.return_unit)
                          ip
                      )
                      ethif )

  let spawn_listeners (netif, ethif, arp, ip, tcp) =
    (* TODO: an async_hook for error reporting would be nice *)
    Lwt.async (arp_and_tcp_listeners netif ethif arp ip tcp);
    Lwt.return (netif, ethif, arp, ip, tcp)

  let converse c server_ip
      (_, _, client_arp, client_ip, client_tcp)
    =
    (* every second, bother the other end and see whether they have anything to
       say back to us *)
    let dest = server_ip in
    let src = List.hd (IPV4.get_ip client_ip) in
    Log.warn "DEMO: trying connection from %s to %s on port %d"
      (Ipaddr.V4.to_string src) (Ipaddr.V4.to_string dest) echo_port;
    TCP.create_connection client_tcp (dest, echo_port) >>= function
    | `Error _ -> Lwt.fail (failwith "couldn't establish connection between client and server")
    | `Ok flow ->
      let rec pester flow =
        let important_content = Cstruct.of_string "hi I love you I missed you" in
        TCP.write flow important_content >>= ignore_errors c
          (
            Log.warn "%s -> %s: %s" (strip src) (strip dest) (Cstruct.to_string
                                                                important_content);
            fun () -> TCP.read flow >>= ignore_errors c (fun buf ->
                Log.warn "%s -> %s: %s" (strip dest) (strip src) (Cstruct.to_string buf);
                Lwt.return_unit )
          ) >>= fun () ->
        OS.Time.sleep pester_interval >>= fun () -> pester flow
      in
      Log.warn "DEMO: connection established between %s and %s!" (strip src)
        (strip dest);
      pester flow

  let crosstalk ((_, _, _, left_ip, _),
                 (_, _, _, right_ip, _)) : unit Lwt.t =
    let rec gossip dst =
      let (frame, len) = IPV4.allocate_frame left_ip ~dst ~proto:`UDP in
      (* this is a broken packet -- no udp header *)
      let app_data = (Cstruct.shift frame len) in
      let secrets = "CONFIDENTIAL GOSSIP" in
      Cstruct.blit_from_string secrets 0 app_data 0 (String.length secrets);
      IPV4.write left_ip frame app_data >>= fun () ->
      Log.warn "%s -> %s: %s" (strip (List.hd (IPV4.get_ip left_ip))) (strip dst)
        secrets;
      OS.Time.sleep crosstalk_interval >>= fun () -> gossip dst
    in
    let dst = List.hd (IPV4.get_ip right_ip) in
    gossip dst

  let start console netif clock random =
    let client_ip = Ipaddr.V4.of_string_exn "192.168.3.10" in
    let server_ip = Ipaddr.V4.of_string_exn "192.168.3.2" in
    let name_repo ip = Printf.sprintf "client_%s" (Ipaddr.V4.to_string ip) in
    let start_tcp (n, e, a, ip) =
      TCP.connect ip >>= function
      | `Ok tcp -> Lwt.return (n, e, a, ip, tcp)
      | `Error _ -> Lwt.fail (failwith "error connecting TCP")
    in
    get_arp ~netif ~root ~node:(name_repo client_ip) () >>=
    start_ip client_ip >>= start_tcp >>= spawn_listeners >>=
    converse console server_ip >>= fun () ->
    Lwt.return_unit

end

open V1_LWT
open Lwt
open OUnit

module Main (C: CONSOLE) (K: KV_RO) = struct

  let start c k =
    let module P = Netif.Make(K)(OS.Time) in
    let module E = Ethif.Make(P) in
    let module I = Ipv4.Make(E) in
    let module U = Udp.Make(I) in

    let or_error c name fn t =
      fn t >>= function 
      | `Error e -> fail (Failure ("Error starting " ^ name))
      | `Ok t -> return t
    in

    let file = "mirage_dhcp_discover.pcap" in 
    let ip = (Ipaddr.V4.of_string_exn "192.168.2.222") in
    let nm = (Ipaddr.V4.of_string_exn "255.255.255.0") in

    let setup_iface file ip nm =
      let pcap_netif_id = P.id_of_desc ~timing:None ~source:k ~read:file in
      (* build interface on top of netif *)
      or_error c "pcap_netif" P.connect pcap_netif_id >>= fun p ->
      or_error c "ethif" E.connect p >>= fun e ->
      or_error c "ipv4" I.connect e >>= fun i ->

      (* set up ipv4 statically *)
      I.set_ip i (Ipaddr.V4.of_string_exn "192.168.2.222") >>= fun () ->
      I.set_ip_netmask i (Ipaddr.V4.of_string_exn "255.255.255.0") >>= fun () ->

      or_error c "udpv4" U.connect i >>= fun u ->

      Lwt.return (p, e, i, u)
    in

    setup_iface file ip nm >>= fun (p, e, i, u) ->
    C.log_s c "beginning listen...";
    let noop = fun ~src ~dst buf -> Lwt.return_unit in
    P.listen p (
      E.input 
        ~arpv4:( 
          fun buf -> I.input_arpv4 i buf
        ) 
        ~ipv4:(
        I.input ~tcp:noop ~udp:noop ~default:(
            fun ~proto ~src ~dst buf -> Lwt.return_unit
          ) 
          i
      ) ~ipv6:(fun b -> Lwt.return_unit) e
    ) >>= fun () ->
    (* the capture contains a GARP from 192.168.2.7, so we should have an entry
       for that address in the arp cache now *)
    (* send a udp packet to that address, which will result in an attempt to
       resolve the address on the ARP layer.  If the thread returns, all's well. *)
    let target = Ipaddr.V4.of_string_exn "192.168.2.7" in
    let try_connecting _context =
      U.write ~source_port:1000 ~dest_ip:target ~dest_port:1024 u (Cstruct.create
                                                                 0) >>= fun () ->
      Lwt.return `Success
    in
    let timeout_then_fail _context =
      let is_arp_request packet =
        (* E.input only wants things that return unit Lwt.t, so
           we need to signal which branch we took through side effects *)
        let is_arp = ref false in
        let not_arp = fun buf -> (is_arp := false; Lwt.return_unit) in
        (* use E.input to get tcpip to parse this for us *)
        (* don't need to worry about mac filtering in ethif because we're
           looking for something that will have dst = broadcast *)
        E.input ~arpv4:(fun buf -> (is_arp := true; Lwt.return_unit)) 
          ~ipv4:not_arp ~ipv6:not_arp e packet >>= fun () -> Lwt.return !is_arp
      in
      let timeout = 1.0 in
      OS.Time.sleep timeout >>= fun () ->
      (* make sure the failure is because we wrote an arp request packet *)
      match P.get_written p with
      | [] -> Lwt.return (`Failure "Timed out, although we didn't write anything?")
      | p :: _ -> 
        is_arp_request p >>= function
        | true -> Lwt.return (`Failure "Timed out, and we wrote what looks like an arp probe")
        | false -> Lwt.return (`Failure "Timed out and wrote something, but it
        doesn't look like an arp probe")
    in
    Lwt.pick [
        try_connecting ();
        timeout_then_fail ()
    ] >>= fun result ->
    let printer = function
      | `Success -> "Success"
      | `Failure s -> s
    in
(* 
1) we age out arp entries after some amount of time
2) we update arp entries in the presence of new information
3) we send out arp probes when trying to resolve unknown addresses
4) we retry arp probes a predictable number of times, at predictable intervals
5) we stop retrying arp probes once one has succeeded
6) on successful reception of an arp reply, we don't unnecessarily delay our response
*)

    assert_equal ~printer `Success result;
    Lwt.return_unit
end

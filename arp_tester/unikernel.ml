open V1_LWT
open Lwt
open OUnit

module Main (C: CONSOLE) (K: KV_RO) = struct
  module P = Netif.Make(K)(OS.Time)
  module E = Ethif.Make(P)
  module I = Ipv4.Make(E)(Clock)(OS.Time)
  module A = Arpv4.Make(E)
  module U = Udp.Make(I)

      (* unfortunately, arp isn't exposed in wire_structs nor in Arpv4, so
         we reproduce it here, nonoptimally *)
      cstruct arp {
      uint8_t dst[6];
      uint8_t src[6];
      uint16_t ethertype;
      uint16_t htype;
      uint16_t ptype;
      uint8_t hlen;
      uint8_t plen;
      uint16_t op;
      uint8_t sha[6];
      uint32_t spa;
      uint8_t tha[6];
      uint32_t tpa
    } as big_endian

    cenum op {
      Op_request = 1;
      Op_reply
    } as uint16_t

  let file = "mirage_dhcp_discover.pcap"
  (* arbitrary IP on a network matching the one in the pcap *)
  let ip = Ipaddr.V4.of_string_exn "192.168.2.222"
  let nm = Ipaddr.V4.of_string_exn "255.255.255.0"
  (* GARP emitter in pcap *)
  let target = Ipaddr.V4.of_string_exn "192.168.2.7"
  (* one of many hosts for which there is no traffic in the pcap *)
  let silent_host = Ipaddr.V4.of_string_exn "192.168.2.4"

  let printer = function
    | `Success -> "Success"
    | `Failure s -> s

  let is_arp_request e packet =
    (* E.input only wants things that return unit Lwt.t, so
           we need to signal which branch we took through side effects *)
    let is_arp = ref false in
    let not_arp = fun buf -> (is_arp := false; Lwt.return_unit) in
    let is_arp_probe packet =
      match (get_arp_op packet) with
      | 1 -> true
      | _ -> false
    in
    let came_from_us packet =
      match Macaddr.compare (E.mac e) (Macaddr.of_bytes_exn (copy_arp_src
                                                               packet)) with
      | 0 -> true
      | _ -> false
    in
    (* use E.input to get tcpip to parse this for us *)
    (* don't need to worry about mac filtering in ethif because we're
       looking for something that will have dst = broadcast *)
    E.input ~arpv4:(fun buf -> (if is_arp_probe buf && came_from_us buf then
                                  is_arp := true; Lwt.return_unit))
      ~ipv4:not_arp ~ipv6:not_arp e packet >>= fun () -> Lwt.return !is_arp

  let send_traffic u =
    U.write ~source_port:1000 ~dest_ip:target ~dest_port:1024 u
      (Cstruct.create 0) >>= fun () ->
    Lwt.return `Success

  let test_send_arps p e u =
    let try_connecting _context =
      U.write ~source_port:1000 ~dest_ip:silent_host
        ~dest_port:1024 u (Cstruct.create 0) >>= fun () ->
      Lwt.return (`Failure "Sent a UDP packet for a host which can't have been
                      in the ARP cache")
    in
    let timeout_then_succeed _context =
      OS.Time.sleep 1.0 >>= fun () ->
      (* check to make sure we wrote an ARP probe *)
      match (P.get_written p) with
      | [] -> Lwt.return (`Failure "Wrote nothing when should've ARP probed")
      | l -> is_arp_request e (List.hd (List.rev l)) >>= function
        | true -> Lwt.return `Success
        | false -> Lwt.return (`Failure "Waited for something, but the last
          thing we wrote wasn't an ARP request")
    in
    Lwt.pick [
      try_connecting ();
      timeout_then_succeed ();
    ]

  let test_garp_was_read p e u =
    let timeout_then_fail _context =
      let timeout = 1.0 in
      OS.Time.sleep timeout >>= fun () ->
      (* make sure the failure is because we wrote an arp request packet *)
      match P.get_written p with
      | [] -> Lwt.return (`Failure "Timed out, although we didn't write anything?")
      | l ->
        is_arp_request e (List.hd (List.rev l)) >>= function
        | true -> Lwt.return (`Failure "sent an arp probe for something that
          just GARPed")
        | false -> Lwt.return (`Failure "Timed out and wrote something, but it
        doesn't look like an arp probe")
    in
    let try_connecting _context =
      U.write ~source_port:1000 ~dest_ip:target ~dest_port:1024 u (Cstruct.create
                                                                     0) >>= fun () ->
      Lwt.return `Success
    in
    Lwt.pick [
      try_connecting ();
      timeout_then_fail ()
    ]

  let test_arp_aged_out p e u =
    (* attempt a connection, which we expect not to succeed *)
    let arp_age = 65.0 (* set to whatever the arp aging interval is; would be
                          nice to have this visible *) in
    let try_connecting _context =
      (* make sure we were cool to write initially *)
      U.write ~source_port:1000 ~dest_ip:target ~dest_port:1024 u
        (Cstruct.create 0) >>= fun () ->
      OS.Time.sleep arp_age >>= fun () ->
      U.write ~source_port:1000 ~dest_ip:target ~dest_port:1024 u
        (Cstruct.create 0) >>= fun () ->
      (* spoo out the last packet sent for debugging *)
      let last_sent = (List.hd (List.rev (P.get_written p))) in
      match (Cstruct.len last_sent) with
      | 0 ->
        Printf.printf "Okay, this is obviously bananas.";
        List.iter (fun d -> Printf.printf "packet:\n"; Cstruct.hexdump d)
          (P.get_written p);
        Lwt.return (`Failure "Sent a packet with no contents
                                     when we had no ARP entry for the
                                     destination.")
      | n ->
        let b = Buffer.create (Cstruct.len last_sent) in
        Cstruct.hexdump_to_buffer b last_sent;
        Lwt.return (`Failure (Printf.sprintf "Sent a packet %s with length %d
          when we should've had no ARP entry for the destination"
                                (Buffer.contents b) n))
    in
    let timeout_then_succeed _context =
      OS.Time.sleep (arp_age +. 1.0) >>= fun () ->
      (* check to make sure we wrote an ARP probe *)
      match (P.get_written p) with
      | [] -> Lwt.return (`Failure "Wrote nothing when should've ARP probed")
      | l -> is_arp_request e (List.hd (List.rev l)) >>= function
        | true -> Lwt.return `Success
        | false -> Lwt.return (`Failure "Waited for something, but the last
          thing we wrote wasn't an ARP request")
    in
    Lwt.pick [
      try_connecting ();
      timeout_then_succeed ();
    ]

  let test_queries_retried p e u =
    let try_connecting _context =
      try_lwt U.write ~source_port:1000 ~dest_ip:silent_host
        ~dest_port:1024 u (Cstruct.create 0)
      >>= fun () ->
      Lwt.return (`Failure "Sent a UDP packet for a host which can't have been
                      in the ARP cache")
      (* with I.Routing.No_route_to_destination_address silent_host ->
         Lwt.return `Success *)
with Not_found -> Lwt.return `Success
    in
    let timeout_then_succeed _context =
      let rec first_k_are_arp_requests l (k : int) =
        match l, k with
        | _, 0 -> Lwt.return true
        | [], _ -> Lwt.return false
        | p :: more, k when (k > 0) ->
          is_arp_request e p >>= fun this_one ->
          first_k_are_arp_requests more (k - 1) >>= fun others ->
          Lwt.return (this_one && others)
        | _, _ -> Lwt.return false
      in
      let retry_interval = 1.5 in (* would be better to ask Arp for these
                                     directly *)
      let number_of_retries = 3.0 in
      OS.Time.sleep (retry_interval *. number_of_retries +. 0.25) >>= fun () ->
      (* check to make sure we wrote three ARP probes *)
      match (P.get_written p) with
      | [] -> Lwt.return (`Failure "Wrote nothing when should've ARP probed")
      | l -> first_k_are_arp_requests (List.rev l)
               (int_of_float number_of_retries) >>= function
        | true -> Lwt.return `Success
        | false -> Lwt.return
                     (`Failure (Printf.sprintf "Last %d sent packets weren't ARP probes"
                        (int_of_float number_of_retries)))
    in
    Lwt.pick [
      try_connecting ();
      timeout_then_succeed ()
    ]

  let start c k =

    let or_error c name fn t =
      fn t >>= function
      | `Error e -> fail (Failure ("Error starting " ^ name))
      | `Ok t -> return t
    in


    let setup_iface ?(timing=None) file ip nm =

      let pcap_netif_id = P.id_of_desc ~mac:Macaddr.broadcast ~timing ~source:k ~read:file in
      (* build interface on top of netif *)
      or_error c "pcap_netif" P.connect pcap_netif_id >>= fun p ->
      or_error c "ethif" E.connect p >>= fun e ->
      or_error c "ipv4" I.connect e >>= fun i ->
      or_error c "udpv4" U.connect i >>= fun u ->

      (* set up ipv4 statically *)
      I.set_ip i ip >>= fun () -> I.set_ip_netmask i nm >>= fun () ->

      Lwt.return (p, e, i, u)
    in
    let play_pcap (p, e, i, u) =
      P.listen p (E.input
                    ~arpv4:(fun buf -> I.input_arpv4 i buf)
                    ~ipv4:(fun buf -> Lwt.return_unit)
                    ~ipv6:(fun buf -> Lwt.return_unit) e
                 ) >>= fun () ->
      Lwt.return (p, e, i, u)
    in
    (* the capture contains a GARP from 192.168.2.7, so we should have an entry
       for that address in the arp cache now *)
    (* send a udp packet to that address, which will result in an attempt to
       resolve the address on the ARP layer.  If the thread returns, all's well. *)
    (*
x 1) we age out arp entries after some amount of time
x 2) we update arp entries in the presence of new information
x 3) we send out arp probes when trying to resolve unknown addresses
x 4) we retry arp probes a predictable number of timesen
5) we stop retrying arp probes once one has succeeded
x 6) on successful reception of an arp reply, we don't unnecessarily delay our response
*)
    (* we really should be doing each of these with a fresh pcap_netif;
       otherwise we run the risk of contaminating state between runs *)
    C.log c "testing that arp probes are sent for entries that shouldn't be in
    the cache...";
    setup_iface file ip nm >>= fun send_arp_test_stack ->
    play_pcap send_arp_test_stack >>= fun (p, e, i, u) ->
    test_send_arps p e u >>= fun result ->
    assert_equal ~printer `Success result;

    C.log c "testing that once a response is received, a query thread returns
    that response immediately...";
    setup_iface file ip nm >>= fun (p, e, i, u) ->
    Lwt.pick [
      (play_pcap (p, e, i, u) >>= fun query_stack ->
      Lwt.return (`Failure "return_on_reply: playback thread terminated
      first"));
      send_traffic u; (* these don't appear to actually be interleaved *)
    ] >>= fun result ->
    assert_equal ~printer `Success result;

    C.log c "testing that probes are retried...";
    setup_iface file ip nm >>= fun arp_query_retry_stack ->
    play_pcap arp_query_retry_stack >>= fun (p, e, i, u) ->
    test_queries_retried p e u >>= fun result ->
    assert_equal ~printer `Success result;

    C.log c "testing that gratuitous arps are recorded in the cache...";
    setup_iface file ip nm >>= fun garp_reads_test_stack ->
    play_pcap garp_reads_test_stack >>= fun (p, e, i, u) ->
    test_garp_was_read p e u >>= fun result ->
    assert_equal ~printer `Success result;

    C.log c "testing that entries are aged out...";
    setup_iface file ip nm >>= fun arp_aging_test_stack ->
    play_pcap send_arp_test_stack >>= fun (p, e, i, u) ->
    test_arp_aged_out p e u >>= fun result ->
    assert_equal ~printer `Success result;

    Lwt.return_unit
end

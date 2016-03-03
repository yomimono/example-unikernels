open Mirage

let main =
  foreign "Unikernel.Main" (console @-> kv_ro @->  job)

let disk1 = direct_kv_ro "pcaps"

let tracing = mprof_trace ~size:100000 ()

let () =
  add_to_opam_packages["mirage-clock-unix";"pcap-format"; "tcpip"; "mirage-net-pcap";"oUnit"];
  add_to_ocamlfind_libraries["pcap-format"; "mirage-clock-unix";"tcpip.ethif"; "tcpip.ipv4";
"tcpip.udp"; "tcpip.arpv4";"tcpip.dhcpv4"; "mirage-net-pcap";
                             "oUnit"; "cstruct.syntax"; "oUnit"];
  register "test_arp" ~tracing [ main $ default_console $ disk1 ]

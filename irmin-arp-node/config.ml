open Mirage

let main =
  foreign "Unikernel.Client" (console @-> network @-> clock @-> random @-> job)

let netif = (netif "0")

let () =
  add_to_ocamlfind_libraries["irmin.mem";"irmin-arp";"tcpip.tcp";"tcpip.ethif";
                            "bin_prot"];
  add_to_opam_packages["irmin";"irmin-arp";"tcpip";];
  register "irmin-arp-client" [ main 
                                $ default_console
                                $ netif
                                $ default_clock
                                $ default_random ]

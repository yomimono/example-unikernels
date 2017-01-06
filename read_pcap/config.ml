open Mirage

let file_key =
  let doc = Key.Arg.info
      ~doc:"The file to read for pcaps." [ "file" ]
  in
  Key.(create "file" Arg.(opt ~stage:`Both string "packets.pcap" doc))

let main =
  let libraries = [
    "mirage-net-pcap"; "pcap-format"; ] in
  let packages = ["mirage-net-pcap" ] in
  foreign
    ~libraries ~packages ~keys:[Key.abstract file_key]
    "Unikernel.Main" (console @-> kv_ro @-> time @-> job)

let () =
  let disk1 = direct_kv_ro "." in
  register "read_pcap" [
    main $ default_console $ disk1 $ default_time
  ]

open V1_LWT
open Lwt.Infix

module Main (C: CONSOLE) (FS: KV_RO) (Time: TIME) = struct

  module P = Netif.Make(FS)(Time)

  let start c fs _ =
    let pcap_init_error ~fs ~read =
      let pcap_netif_id = P.id_of_desc ~mac:Macaddr.broadcast ~source:fs
          ~timing:None ~read in
      P.connect pcap_netif_id >>= function
    | `Error e -> C.log c "Failed to initialize from given pcap file";
      Lwt.return None
    | `Ok p -> Lwt.return (Some p)
    in

    pcap_init_error ~fs ~read:(Key_gen.file ()) >>= function
    | None -> Lwt.return_unit (* exit cleanly for invalid pcap *)
    | Some net -> P.listen net ( fun _ -> Lwt.return_unit)
end

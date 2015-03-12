open Lwt
open V1_LWT
open OS

module Client (C: V1_LWT.CONSOLE) (CLIENT_STACK: V1_LWT.STACKV4) = struct
    let remote_server="54.69.202.59"
    let port = 22 

  let start c client_stack =
    let construct_request () =
      let buf = Io_page.(to_cstruct (get 1)) in
      let output = (Printf.sprintf "GET / HTTP/1.1\r\n\r\n") in
      Cstruct.blit_from_string output 0 buf 0 (String.length output);
      Cstruct.set_len buf (String.length output)
    in
    
    let rec make_connection c s =
      let my_tcpv4 = (CLIENT_STACK.tcpv4 s) in
      let webserver = remote_server in
        CLIENT_STACK.TCPV4.create_connection my_tcpv4 ((Ipaddr.V4.of_string_exn webserver),
        port) >>=
        fun conn -> (
          C.log c "connection created\n";
      match conn with 
      | `Ok (outbound : CLIENT_STACK.TCPV4.flow) -> 
          C.log c "writing to conn\n";
          let request = construct_request () in
          (* send bogus request *)
          CLIENT_STACK.TCPV4.write outbound request >>
          CLIENT_STACK.TCPV4.close outbound 
      | p -> 
          (* continue retrying until successful or heat death of universe *)
          C.log c (Printf.sprintf "Couldn't initiate connection to %s:%d ;
          retrying\n" webserver port);
          make_connection c s
    ) 
      in

          C.log c "client about to go";
    (make_connection c client_stack)
end

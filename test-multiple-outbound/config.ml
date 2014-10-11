open Mirage

(* foreign string (functor type) -> functor implementation (?)) *)
(* 
 utop # let main = foreign "Ask_thing.Main" (console @-> stackv4 @-> job);;
 val main : (console -> stackv4 -> job) impl = <abstr>       
 *)

let main = foreign "Ask_thing.Client" (console @-> stackv4 @-> job)


(* that (console -> stackv4 -> job) impl is what we need to call `register` with
 * later. *)

(* default_console is defined in Mirage *)
(* not sure why anyone knows what the hell a tap0 is *)
(* huh, also in Mirage.  is there a tap1, tap2, etc? Nope. *)
(* can get our own with val netif: string -> network impl *)

let primary_netif = (netif "0")
let secondary_netif = (netif "1") (*netif actually needs an integer, shoved
into a string, which maps to a device ID number assigned by Xen, to do anything 
helpful when xen is the target.  Stuff that can't be turned into an int
is silently dropped in that case and we just get the first Xen network iface. *)

let primary_stack = direct_stackv4_with_dhcp default_console primary_netif
let secondary_stack = direct_stackv4_with_dhcp default_console secondary_netif

(* primary_stack and secondary_stack are stackv4 impl 's.  Surely we can write a
 * stackv4 impl -> stackv4 impl that implements a mutating fuzzer, right? *)

let () = 
  (* register : string -> job impl list -> unit *)
  (* What actually happens if you have multiple items
   * in this list? *)
  (* they all get `join`'d and run in parallel. *)
  register "ask_thing" (*mirage uses this string for naming*) [
    main $ default_console $ primary_stack;
    main $ default_console $ secondary_stack;
  ]

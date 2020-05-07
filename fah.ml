open Netchannels

module Debug = struct
  let enable = ref false
end

let () =
  Netlog.Debug.register_module "Fah" Debug.enable

exception Protocol_error of string

let tcp_port = 36330

(* Sending Commands *)
let send_command oc line =
  oc # output_string line;
  oc # output_string "\r\n";
  oc # flush ();
;;

(* read lines while `f line` returns false.
   the line where `f line = true` is not returned
 *)
let read_until ic p =
  let rec loop acc =
    let line = ic # input_line () in
    let len = String.length line in
    if len = 0 then raise (Protocol_error "end of stream")
    else if p line then acc
    else loop (acc^line)
  in loop ""

let startswith prefix s =
  let open String in
  let l = length prefix in
  if length s < l then false
  else sub s 0 l = prefix

let read_PyOn ic =
  ignore (read_until ic (startswith "PyON 1")) ;
  let j = read_until ic ((=) "---") in
  try
    Pyon.from_string j
  with _ ->
    raise (Protocol_error ("error parsing PyOn: " ^ j))


class client
  (ic0 : in_obj_channel)
  (oc0 : out_obj_channel) =
object
  val mutable ic = ic0
  val mutable oc = oc0

  method close () =
    oc # close_out();
    ic # close_in();

  (* 'configured' command *)
  method is_configured () =
    send_command oc "configured" ;
    match read_PyOn ic with
    | `Bool x -> x
    | _ -> raise (Protocol_error "unexpected JSON")

  (* 'info' command *)
  method info () =
    send_command oc "info" ;
    read_PyOn ic

  (* 'num-slots' command *)
  method num_slots () =
    send_command oc "num-slots" ;
    match read_PyOn ic with
    | `Int x -> x
    | _ -> raise (Protocol_error "unexpected JSON")

  (* 'slot-info' command *)
  method slot_info () =
    send_command oc "slot-info" ;
    read_PyOn ic

  (* 'queue-info' command *)
  method queue_info () =
    send_command oc "queue-info" ;
    read_PyOn ic

  method simulation_info n =
    send_command oc ("simulation-info " ^ (string_of_int n));
    read_PyOn ic

end

class connect ?proxy addr timeout =
  let st = Uq_client.connect ?proxy addr timeout in
  let bi = Uq_client.client_channel st timeout in
  let ic = Netchannels.lift_in (`Raw (bi :> Netchannels.raw_in_channel)) in
  let oc = Netchannels.lift_out (`Raw (bi :> Netchannels.raw_out_channel)) in
  client ic oc

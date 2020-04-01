open Lwt
(* open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_style *)
open LTerm_key

(* let draw client ui matrix = () *)
let draw _ _ _ = ()

(* in seconds *)
let update_period = 1.0

type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep update_period >>= fun () -> return (LTick)

let rec loop ui event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ] >>= fun e ->
  LTerm_ui.draw ui;
  match e with
  | LEvent (LTerm_event.Key {code = Escape; _}) ->
     return ()
  | LTick ->
     loop ui event_thread (wait_for_tick ())
  | _ ->
     loop ui (wait_for_event ui) tick_thread

let main() =
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 36330),
            Uq_client.default_connect_options) in
  let c = new Fah.connect addr 60.0 in
  let term = Lwt_main.run (Lazy.force LTerm.stdout) in
  let ui = Lwt_main.run (LTerm_ui.create term (draw c)) in
  (loop ui (wait_for_event ui) (wait_for_tick ()))
    [%lwt.finally LTerm_ui.quit ui]

let () = Lwt_main.run (main ())




(*
let () =
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 36330),
            Uq_client.default_connect_options) in
  let c = new Fah.connect addr 60.0 in
  print_endline "=======================";
  print_endline (Yojson.Basic.to_string (c # info ())) ;
  print_endline "=======================";
  print_endline (string_of_int (c # num_slots ())) ;
  print_endline "=======================";
  print_endline (string_of_bool (c # is_configured ())) ;
  print_endline "======================="
 *)

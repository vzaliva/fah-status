open Lwt
open LTerm_text
open LTerm_style
open LTerm_key

let jempty = Yojson.Basic.from_string "[]"
let state = ref jempty

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  let ts = Float.to_string (Unix.time ()) in
  LTerm_draw.draw_styled ctx 0 0 (eval [B_fg yellow ; S ts ; E_fg]);
  if Yojson.Basic.equal state jempty then
    LTerm_draw.draw_styled ctx 1 0 (eval [B_fg red   ; S"Disconnected" ; E_fg])
  else
    LTerm_draw.draw_styled ctx 1 0 (eval [B_fg green ; S"Connected"    ; E_fg])

(* in seconds *)
let update_period = 1.0

type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep update_period >>= fun () -> return (LTick)

let rec loop c ui event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ] >>= fun e ->
  LTerm_ui.draw ui;
  match e with
  | LEvent (LTerm_event.Key {code = Escape; _}) ->
     return ()
  | LTick ->
     state :=
       if (c # is_configured ()) then
         c # info ()
       else
         jempty ;
     loop c ui event_thread (wait_for_tick ())
  | _ ->
     loop c ui (wait_for_event ui) tick_thread

let main () =
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 36330),
            Uq_client.default_connect_options) in
  let c = new Fah.connect addr 60.0 in
  let term = Lwt_main.run (Lazy.force LTerm.stdout) in
  let ui = Lwt_main.run (LTerm_ui.create term
                           (fun matrix size -> draw matrix size !state)) in
  (loop c ui (wait_for_event ui) (wait_for_tick ()))
    [%lwt.finally LTerm_ui.quit ui]

(* debug dump *)
let dump () =
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

let () = Lwt_main.run (main ())

(*
let () = dump ()
 *)

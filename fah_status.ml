open Lwt
open LTerm_text
open LTerm_style
open LTerm_key

let jempty = Yojson.Basic.from_string "[]"

type cstate = 
  | Connecting
  | NotConfigured
  | Connected of Yojson.Basic.t

let state = ref Connecting

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  let ts = Float.to_string (Unix.time ()) in
  LTerm_draw.draw_styled ctx 0 0 (eval [B_fg yellow ; S ts ; E_fg]);
  match state with
  | Connecting ->
     LTerm_draw.draw_styled ctx 1 0 (eval [B_fg red   ; S"Disconnected" ; E_fg])
  | NotConfigured ->
     LTerm_draw.draw_styled ctx 1 0 (eval [B_fg red   ; S"Not Configured" ; E_fg])
  | Connected _ ->
     LTerm_draw.draw_styled ctx 1 0 (eval [B_fg green ; S"Connected"    ; E_fg])

(* in seconds *)
let update_period = 1.0

type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep update_period >>= fun () -> return (LTick)

let rec loop host port c ui event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ] >>= fun e ->
  LTerm_ui.draw ui;
  match e with
  | LEvent (LTerm_event.Key {code = Escape; _}) ->
     begin
       (* TODO: maybe close connection here *)
       return ()
     end
  | LTick ->
     begin
       match c with
       | None ->
          begin
            try
              (* TODO: check for socket leak *)
              let addr =
                `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, host, port),
                        Uq_client.default_connect_options) in
              let c = new Fah.connect addr 60.0 in
              state := Connecting ;
              loop host port (Some c) ui event_thread (wait_for_tick ())
            with
            | _ -> loop host port None ui event_thread (wait_for_tick ())
          end
       | Some c ->
          begin
            try
              if (c # is_configured ()) then
                (state := Connected (c # info ());
                 loop host port (Some c) ui event_thread (wait_for_tick ()))
              else
                (state := NotConfigured;
                 loop host port (Some c) ui event_thread (wait_for_tick ()))
            with
            | _ ->
               c # close () ;
               state := Connecting;
               loop host port None ui event_thread (wait_for_tick ())
          end
     end
  | _ ->
     loop host port c ui (wait_for_event ui) tick_thread

let main () =
  let term = Lwt_main.run (Lazy.force LTerm.stdout) in
  let ui = Lwt_main.run (LTerm_ui.create term
                           (fun matrix size -> draw matrix size !state)) in
  (loop "localhost" 36330 None ui (wait_for_event ui) (wait_for_tick ()))
    [%lwt.finally LTerm_ui.quit ui]

(* debug dump *)
let dump () =
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 36330),
            Uq_client.default_connect_options) in
  let c = new Fah.connect addr 60.0 in
  let nslots = c # num_slots () in
  print_endline "=======================";
  print_endline ("Configured: " ^ string_of_bool (c # is_configured ())) ;
  print_endline ("Slots " ^ string_of_int nslots) ;
  print_endline "=======================";
  print_endline ("info: " ^ Yojson.Basic.to_string (c # info ())) ;
  print_endline "=======================";
  print_endline ("slot-info " ^ Yojson.Basic.to_string (c # slot_info ())) ;
  print_endline "=======================";
  print_endline ("queue-info " ^ Yojson.Basic.to_string (c # queue_info ())) ;

  for i = 0 to (nslots-1) do
    print_endline "=======================";
    print_endline ("simulation-info " ^ Yojson.Basic.to_string (c # simulation_info i)) ;
    print_endline "=======================";
  done


let () = Lwt_main.run (main ())

(*
let () = dump ()
 *)

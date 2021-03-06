open Lwt
open LTerm_text
open LTerm_style
open LTerm_key

open Core

let version="1.0"

type slot_info =
  | Ready of
      {
        description:string (* from slot-info *)
      }
  | Running of
      {
        description:string; (* from slot-info *)
        idle:bool; (* from slot-info *)
        progress: float; (* 0..1. from simulation-info *)
        eta:string
      }

type cstate =
  | Connecting
  | NotConfigured
  | Configured of
      {
        user:string; (* simulation-info *)
        team:int; (* simulation-info *)
        slots: slot_info list;
      }

let state = ref Connecting

let show_slots ctx (size:LTerm_geom.size) slots =
  let rec loop i slots =
    match slots with
    | [] -> ()
    | s::slots ->
       let y = 5*i in
       match s with
       | Ready {description} ->
          LTerm_draw.draw_styled ctx (i+y) 0
            (eval [
                 B_fg lyellow ; S"Slot #"      ; E_fg ;
                 B_fg yellow ; S(string_of_int (i+1) ^ " "); E_fg;
                 B_fg white ; S(description) ; E_fg;
                 B_fg yellow ; S" Ready" ; E_fg;
            ])
       | Running {description;idle;progress;eta} ->
          let is = if idle then " (idle)" else "" in
          LTerm_draw.draw_styled ctx (i+y) 0
            (eval [
                 B_fg lyellow ; S"Slot #"      ; E_fg ;
                 B_fg yellow ; S(string_of_int (i+1) ^ " "); E_fg;
                 B_fg white ; S(description) ; E_fg;
                 B_fg yellow ; S(" Running"^is^":") ; E_fg;
            ]);
          let pl = int_of_float (progress *. float_of_int size.cols) in
          let pr = size.cols - pl in
          let py = i+y+1 in
          let lc = LTerm_draw.sub ctx { row1 = py; col1 = 0; row2 = py+1; col2 = pl } in
          let rc = LTerm_draw.sub ctx { row1 = py; col1 = pl; row2 = py+1; col2 = pl+pr } in
          let pls = {
              bold       = Some false;
              underline  = None;
              blink      = Some false;
              reverse    = Some true;
              foreground = Some white;
              background = Some black;
            } in
          let prs = {pls with reverse = Some false} in
          LTerm_draw.fill lc ?style:(Some pls) (Zed_char.unsafe_of_char ' ');
          LTerm_draw.fill rc ?style:(Some prs) (Zed_char.unsafe_of_char ' ');
          let percent = Printf.sprintf "%.2f" (progress *. 100.) in
          LTerm_draw.draw_styled ctx (i+y+2) 0
            (eval [
                 B_fg lyellow ; S"Progress: " ; E_fg ;
                 B_fg white ; S(percent ^ "%"); E_fg;
            ]);
          LTerm_draw.draw_styled ctx (i+y+3) 0
            (eval [
                 B_fg lyellow ; S"ETA: " ; E_fg ;
                 B_fg white ; S(eta); E_fg;
            ])

  in
  loop 0 slots

let get_user_stats user team =
  (* TODO: encode user *)
  let url = "https://stats.foldingathome.org/api/donor/" ^ user in
  try
    let s = Nethttp_client.Convenience.http_get url in
    let j = Yojson.Basic.from_string s in
    try
      let open Yojson.Basic.Util in
      let teams = j |> member "teams" |> to_list in
      match List.find teams ~f:(fun x -> (x |> member "team" |> to_int) = team) with
      | None -> None
      | Some x -> Some (x |> member "credit" |> to_int)
    with e ->
      Printf.eprintf "Error parsing donor stats: %s. JSON:\n%s" (Exn.to_string e) s;
      None
  with e ->
    Printf.eprintf "Error fetching donor stats: %s\n" (Exn.to_string e);
    None

let show_user_stats tctx user team =
  LTerm_draw.draw_styled tctx 0 0
    (eval [B_fg lyellow ; S"Credits: " ; E_fg]);
  match get_user_stats user team with
  | None ->
     LTerm_draw.draw_styled tctx 0 9 (eval [B_fg red ; S"Unknown" ; E_fg])
  | Some c ->
     LTerm_draw.draw_styled tctx 0 9 (eval [B_fg yellow ; S(string_of_int c) ; E_fg])

let draw host port state ui matrix =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  let open Core.Unix in
  let ts = strftime (localtime (time ())) "%F %T" in
  LTerm_draw.draw_styled ctx 0 0 (eval [B_bg cyan; B_fg black; S ts ;E_bg;E_fg]);
  let tpos = 20 in
  let hp = [B_fg lblack ; S(host^":"^(string_of_int port)) ; E_fg ] in
  match state with
  | Connecting ->
     LTerm_draw.draw_styled ctx 0 tpos (eval ([B_fg red; S"Disconnected: " ; E_fg] @ hp))
  | NotConfigured ->
     LTerm_draw.draw_styled ctx 0 tpos (eval ([B_fg red; S"Not Configured: " ; E_fg] @ hp))
  | Configured {user=user; team=team; slots=slots} ->
     LTerm_draw.draw_styled ctx 0 tpos
       (eval ([B_fg green ; S"Connected: " ; E_fg ] @ hp));
     LTerm_draw.draw_styled ctx 1 0
       (eval [
            B_fg lyellow ; S"User: " ; E_fg ;
            B_fg yellow ; S user ; E_fg ;
            B_fg lyellow ; S", Team: " ; E_fg ;
            B_fg yellow ; S (string_of_int team) ; E_fg
       ]);
     let tctx = LTerm_draw.sub ctx { row1 = 2; col1 = 0; row2 = 3; col2 = size.cols } in
     show_user_stats tctx user team ;
     let sctx = LTerm_draw.sub ctx { row1 = 4; col1 = 0; row2 = size.rows-4; col2 = size.cols } in
     show_slots sctx size slots;
     let legend = "[ESC to exit]" in
     LTerm_draw.draw_styled ctx (size.rows-1) (size.cols - String.length legend)
       (eval [B_fg lblack ; S(legend) ; E_fg])


(* in seconds *)
let update_period = ref 1.0

type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep !update_period >>= fun () -> return (LTick)

let get_slot_info c slots n =
  let open Yojson.Basic.Util in
  let s = index n slots in
  let description = s |> member "description" |> to_string in
  let status = s |> member "status" |> to_string in
  if status = "RUNNING" then
    let si = c # simulation_info n in
    let qi = c # queue_info () in
    Running {
        description = description ;
        idle = s |> member "idle" |> to_bool ;
        progress = si |> member "progress" |> to_float ;
        eta = qi |> index n |> member "eta" |> to_string
      }
  else
    Ready {description=description}

let get_cstate (c:Fah.client) =
  let open Yojson.Basic.Util in
  let n = (c # num_slots ()) in
  if (c # is_configured () && n > 0) then
    (* pick up user and team from slot 0 assuming it is the same
       for all slots *)
    let s0 = c # simulation_info 0 in
    let user = s0 |> member "user" |> to_string in
    (* Team is returned as "string" while it is "int" *)
    let team = int_of_string (s0 |> member "team" |> to_string) in
    let slots = c # slot_info () in
    let ls = List.init n ~f:(get_slot_info c slots) in
    Configured {user=user; team=team; slots=ls}
  else
    NotConfigured

let rec loop host port c ui event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ] >>= fun e ->
  LTerm_ui.draw ui;
  match e with
  | LEvent (LTerm_event.Key {code = Escape; _}) ->
     begin
       (match c with
        | Some c ->  c # close ()
        | None -> ())
     ; return ()
     end
  | LTick ->
     begin
       match c with
       | None ->
          begin
            try
              let addr =
                `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, host, port),
                        Uq_client.default_connect_options) in
              let c = new Fah.connect addr 60.0 in
              state := Connecting ;
              loop host port (Some c) ui event_thread (wait_for_tick ())
            with
            | e ->
               Printf.eprintf "there was an error: %s\n" (Exn.to_string e);
               loop host port None ui event_thread (wait_for_tick ())
          end
       | Some c ->
          begin
            try
              state := get_cstate c ;
              loop host port (Some c) ui event_thread (wait_for_tick ())
            with
            | e ->
               Printf.eprintf "there was an error: %s\n" (Exn.to_string e);
               c # close () ;
               state := Connecting;
               loop host port None ui event_thread (wait_for_tick ())
          end
     end
  | _ ->
     loop host port c ui (wait_for_event ui) tick_thread

let main host port =
  let term = Lwt_main.run (Lazy.force LTerm.stdout) in
  let ui = Lwt_main.run (LTerm_ui.create term
                           (fun m s -> draw host port !state m s)
             ) in
  (loop host port None ui (wait_for_event ui) (wait_for_tick ()))
    [%lwt.finally LTerm_ui.quit ui]

let command =
  let open Command.Let_syntax in
  Command.basic ~summary:"Folding@Home Status Monitor"
    [%map_open
     let host = flag "-h" (optional_with_default "localhost" string)
                  ~doc:"host name or IP"
     and port = flag "-p" (optional_with_default 36330 int)
                  ~doc:"port number"
     and interval = flag "-i" (optional_with_default 1 int)
                      ~doc:"update interval (seconds)" in
         fun () ->
         update_period := float_of_int interval;
         Lwt_main.run (main host port)
    ]

let () =
  Nettls_gnutls.init() ;
  Command.run ~version:version ~build_info:"git" command

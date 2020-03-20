open Netchannels
open Yojson
open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Fah" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Fah" Debug.enable

let () =
  Netlog.Debug.register_module "Fah" Debug.enable

type state =
  [ `Authorization
  | `Connected
  ]

exception Protocol_error
exception Authentication_error
exception Bad_state

let tcp_port = 36330

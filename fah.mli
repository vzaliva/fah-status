(**
 * This is an interface for the FAH Client Protocol
 * per https://github.com/FoldingAtHome/fah-control/wiki/3rd-party-FAHClient-API
 *)

open Netchannels

(** Default TCP port *)
val tcp_port : int

type state =
  [ `Authorization
  | `Connected
  ]

exception Protocol_error
exception Authentication_error
exception Bad_state

(** The class [client] implements the FAH client protocol. Client objects
 * are created by
 * {[ new client in_ch out_ch ]}
 * where [in_ch] is an input channel representing the input direction of
 * the TCP stream, and where [out_ch] is an output channel representing
 * the output direction of the TCP stream.
 *)
class client :
  in_obj_channel -> out_obj_channel ->
object

  (* General Commands *)

  method is_configured: unit -> bool

  method info : unit -> Yojson.t

  method num_slots: unit -> int

  method slot_info : unit -> Yojson.t

  method queue_info : unit -> Yojson.t

  (** Closes the file descriptors *)
  method close : unit -> unit
end


(** [connect addr timeout]: Connects with the server at [addr], and
    configure that I/O operations time out after [timeout] seconds of
    waiting.

  Example:

{[
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "www.domain.com", 36330),
            Uq_client.default_connect_options) in
  let client =
    new Fah.connect addr 60.0
]}
 *)
class connect : ?proxy:#Uq_engines.client_endpoint_connector ->
                Uq_engines.connect_address ->
                float ->
                  client


(** Authenticates the session:

Options:

  - [user]: the user name to authenticate as
  - [pass]: the password

{[
Fah.authenticate
  ~user:"tom"
  ~pass:"secret"
  client
]}
 *)
val authenticate : ?user:string ->
                   ?pass:string ->
                   client -> unit

(** {1 Debugging} *)

module Debug : sig
  (** Enables {!Netlog}-style debugging of this module  By default,
      the exchanged Telnet commands are logged. This can be extended
      by setting the [verbose_input] and [verbose_output] options.
   *)
  val enable : bool ref
end

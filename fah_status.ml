let () =
  let addr =
    `Socket(`Sock_inet_byname(Unix.SOCK_STREAM, "localhost", 36330),
            Uq_client.default_connect_options) in
  let c = new Fah.connect addr 60.0 in
  print_endline (string_of_bool (c # is_configured ())) ;
  print_endline "Hello world!"

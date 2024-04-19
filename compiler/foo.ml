(* This file is in the public domain *)
open Core
open Async_kernel
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server

let handler ~body:body _sock req =
  Stdlib.Printf.eprintf "handler check\n%!";
  let headers = Cohttp.Header.of_list [("Access-Control-Allow-Origin", "*")] in
  let meth = Cohttp.Request.meth req in
  let () = Stdlib.Printf.eprintf "Method: %s\n%!" (Cohttp.Code.string_of_method meth) in
  match meth with
  | `POST ->
    Body.to_string body >>= fun body ->
    Stdlib.Printf.eprintf "Body: %s%!" body;
    Server.respond ~headers:headers `OK
  | `OPTIONS ->
    Server.respond ~headers:headers `OK
  | _ -> 
    Stdlib.Printf.eprintf "Nope%!";
    Server.respond `Method_not_allowed

let start_server port () =
  Stdlib.Printf.eprintf "Listening for HTTP on port %d\n" port;
  Stdlib.Printf.eprintf "Try curl -X POST -d 'foo bar' http://localhost:%d\n%!" port;
  Server.create ~on_handler_error:`Raise
    (Async.Tcp.Where_to_listen.of_port port)
    handler
  >>= fun _ -> 
    Stdlib.Printf.eprintf "Server started\n";
    Deferred.never ()

let () =
  let module Command = Async_command in
  Command.async_spec ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(
      empty
      +> flag "-p"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on")
    start_server
  |> Command_unix.run
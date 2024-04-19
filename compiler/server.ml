(* open Async
open Cohttp
open Cohttp_async
open Core_unix

let handle_request ~body sock req =
  let uri = Request.uri req in
  match Uri.path uri, Request.meth req with
  | "/submit", `POST ->
      Body.to_string body >>= fun body_str ->
      let response_message = "Received: " ^ body_str in
      Server.respond_string ~status:`OK response_message
  | _ -> 
      Server.respond_string ~status:`Not_found "Not Found"

let start_server port =
  eprintf "Listening on port %d\n" port;
  Cohttp_async.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port port)
    handle_request
  >>= fun _ -> Deferred.never ()

let command =
  Command.async
    ~summary:"OCaml HTTP server handling POST requests"
    Command.Param.(
      map (flag "port" (optional_with_default 8080 int) ~doc:"Port to listen on")
          ~f:(fun port -> fun () -> start_server port)
    )

let () =
  Command_unix.run command *)

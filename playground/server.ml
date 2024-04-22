(* This file is in the public domain *)
open Core
open Async_kernel
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server

(* temporary, helps with testing for now, which is why depr is okay *)
let wasm_file_to_binary_string filename =
  (* Open the file in binary mode *)
  let input_channel = open_in_bin filename in
  try
    let file_length = in_channel_length input_channel in
    let file_data = Bytes.create file_length in
    (* Really read the file content into the bytes buffer then close *)
    really_input input_channel file_data 0 file_length;
    close_in input_channel;
    (* Convert the bytes buffer to a _base64_ string *)
    let base64_string = Base64.encode_exn (Bytes.to_string file_data) in
    base64_string
  with e -> close_in_noerr input_channel; raise e
;;

let handler ~body _sock req =
  Stdlib.Printf.eprintf "handler check\n%!";
  let headers =
    Cohttp.Header.of_list
      [("Access-Control-Allow-Origin", "*"); ("Content-Type", "application/json")]
  in
  let meth = Cohttp.Request.meth req in
  let () = Stdlib.Printf.eprintf "Method: %s\n%!" (Cohttp.Code.string_of_method meth) in
  match meth with
  | `POST ->
      (* Receive code data from Client *)
      Body.to_string body
      >>= fun body_string ->
      (* Compile the code (rn just a placeholder): we will be able
         to produce wasm directly instead of needing this extra bit *)
      let wasm_filename = "simple.wasm" in
      let wasm_binary_string = wasm_file_to_binary_string wasm_filename in
      (* Send wasm data (or compile-time error messages) to Client *)
      let json_response =
        `Assoc [("result", `String "success"); ("value", `String wasm_binary_string)]
      in
      let response_body = Yojson.Basic.to_string json_response in
      let body = Body.of_string response_body in
      Server.respond ~headers `OK ~body
  | _ ->
      Stdlib.Printf.eprintf "Nope%!";
      Server.respond `Method_not_allowed
;;

let start_server port () =
  Stdlib.Printf.eprintf "Listening for HTTP on port %d\n" port;
  Server.create ~on_handler_error:`Raise (Async.Tcp.Where_to_listen.of_port port) handler
  >>= fun _ ->
  Stdlib.Printf.eprintf "Server started\n";
  Deferred.never ()
;;

let () =
  let module Command = Async_command in
  Command.async_spec ~summary:"Simple http server that outputs body of POST's"
    Command.Spec.(
      empty +> flag "-p" (optional_with_default 8080 int) ~doc:"int Source port to listen on" )
    start_server
  |> Command_unix.run
;;
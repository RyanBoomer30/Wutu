(* This file is in the public domain *)
open Core
open Async_kernel
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server
open Runner
open Wasm

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
      (* Parse body into compiler *)
      let json_response =
        match compile_string_to_string ~target:Exprs.Wasm Exprs.Naive "" body_string with
        | Error (errs, _) ->
            let errs = ExtString.String.join "\n" (Errors.print_errors errs) in
            `Assoc [("result", `String "failure"); ("value", `String errs)]
        | Ok (wat_string, _) ->
            (* for now, we just write the file as a .wat, convert to .wasm, and read back *)
            let outfile = open_out "temp.wat" in
            fprintf outfile "%s" wat_string;
            close_out outfile;
            let bstdout, bstdout_name, bstderr, bstderr_name, bstdin = make_tmpfiles "build" "" in
            let built_pid =
              Caml_unix.create_process "make" (Array.of_list ["make"; "temp.wasm"]) bstdin bstdout bstderr
            in
            let _, status = Caml_unix.waitpid [] built_pid in
            let out = "temp" in
            let try_running =
              match status with
              | WEXITED 0 -> Ok ()
              | WEXITED n ->
                  Error
                    (sprintf "Finished with error while building %s:\nStderr:\n%s\nStdout:\n%s" out
                       (string_of_file bstderr_name) (string_of_file bstdout_name) )
              | WSIGNALED n -> Error (sprintf "Signalled with %d while building %s." n out)
              | WSTOPPED n -> Error (sprintf "Stopped with signal %d while building %s." n out)
            in
            let result =
              match try_running with
              | Error msg -> failwith msg
              | Ok _ -> let wasm_binary_string = wasm_file_to_binary_string "temp.wasm" in
              `Assoc [("result", `String "success"); ("value", `String wasm_binary_string)]
            in
            result
      in
      (* (* Compile the code (rn just a placeholder): we will be able
         to produce wasm directly instead of needing this extra bit *)
      let wasm_filename = "simple.wasm" in
      let wasm_binary_string = wasm_file_to_binary_string wasm_filename in
      (* Send wasm data (or compile-time error messages) to Client *)
      let json_response =
        `Assoc [("result", `String "success"); ("value", `String wasm_binary_string)]
      in *)
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

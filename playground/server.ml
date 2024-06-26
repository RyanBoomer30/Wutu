(* This file is in the public domain *)
open Core
open Async_kernel
module Body = Cohttp_async.Body
module Server = Cohttp_async.Server
open Runner

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
        match
          compile_string_to_string ~target:Exprs.Wasm Exprs.Naive "user_program" body_string
        with
        | Error (errs, _) ->
            let errs = ExtString.String.join "\n" (Errors.print_errors errs) in
            `Assoc [("did_compile", `Bool false); ("value", `String errs)]
        | Ok (wat_string, _) ->
            (* for now, we just write the file as a .wat, convert to .wasm, and read back *)
            let outfile = Out_channel.create "playground/temp.wat" in
            fprintf outfile "%s" wat_string;
            Out_channel.close outfile;
            let bstdout, bstdout_name, bstderr, bstderr_name, bstdin = make_tmpfiles "build" "" in
            let built_pid =
              Caml_unix.create_process "make"
                (Array.of_list ["make"; "playground/temp.wasm"])
                bstdin bstdout bstderr
            in
            let _, status = Caml_unix.waitpid [] built_pid in
            let out = "temp" in
            let try_running =
              match status with
              | WEXITED 0 -> Ok ()
              | WEXITED _ ->
                  Error
                    (sprintf "Finished with error while building %s:\nStderr:\n%s\nStdout:\n%s" out
                       (string_of_file bstderr_name) (string_of_file bstdout_name) )
              | WSIGNALED n -> Error (sprintf "Signalled with %d while building %s." n out)
              | WSTOPPED n -> Error (sprintf "Stopped with signal %d while building %s." n out)
            in
            let result =
              match try_running with
              | Error msg -> failwith msg
              | Ok _ ->
                  let wasm_binary_string = wasm_file_to_binary_string "playground/temp.wasm" in
                  `Assoc [("did_compile", `Bool true); ("value", `String wasm_binary_string)]
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
  Stdlib.Printf.eprintf "Try curl -X POST -d 'foo bar' http://localhost:%d\n%!" port;
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

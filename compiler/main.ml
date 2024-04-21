open Printf

let input_set = ref false

let input_file : string ref = ref ""

let output_file : string ref = ref ""

let () =
  let speclist = [("-o", Arg.Set_string output_file, "Set output file name")] in
  Arg.parse speclist
    (fun name ->
      if !input_set then
        raise (Arg.Bad "Cannot compile more than one file")
      else (
        input_set := true;
        input_file := name
      ) )
    "Compiler options:";
  (* let sep = "\n=================\n" in *)
  (* match compile_file_to_string with
  | Error _ -> eprintf "ERROR HAPPENED"
  | Ok _ -> printf "OKAY" *)
;;

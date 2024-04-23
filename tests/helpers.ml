open Compile
open Exprs
open Runner
open OUnit2
open Pretty

(* to easily switch between allocation strategies, swap this.
   note that this does not change garbage collection related helpers,
   since some garbage collection tests change behavior (ex: things that
   would be kept on the stack are now overwritten in registers, and are
   garbage earlier than the tests expect) *)

let alloc_strategy = Register

(* this lets us control t and terr, changing between x86_64 and WebAssembly
   For now, this doesn't change tanf, but this can be updated
   This also doesn't change tr, tgc, tvg, etc: because those don't apply to Wasm *)
let target = Wasm

(* for now, everything but tr uses Naive register allocation
   we can revisit these test helpers to make them more flexible later *)

let t ?(no_builtins = false) name program input expected =
  name
  >:: test_run ~no_builtins ~args:[] ~std_input:input ~target alloc_strategy program name expected
;;

let tnb = t ~no_builtins:true

(* always uses register allocation *)
let tr name program input expected =
  name >:: test_run ~args:[] ~std_input:input Register program name expected
;;

let ta name program input expected =
  name >:: test_run_anf ~args:[] ~std_input:input program name expected
;;

let tgc ?(no_builtins = false) name heap_size program input expected =
  name
  >:: test_run ~no_builtins
        ~args:[string_of_int heap_size]
        ~std_input:input Naive program name expected
;;

let tvg name program input expected =
  name >:: test_run_valgrind ~args:[] ~std_input:input Naive program name expected
;;

let tvgc name heap_size program input expected =
  name
  >:: test_run_valgrind ~args:[string_of_int heap_size] ~std_input:input Naive program name expected
;;

let terr ?(no_builtins = false) name program input expected =
  name
  >:: test_err ~no_builtins ~args:[] ~std_input:input ~target alloc_strategy program name expected
;;

let terrnb = terr ~no_builtins:true

let tgcerr ?(no_builtins = false) name heap_size program input expected =
  name
  >:: test_err ~no_builtins
        ~args:[string_of_int heap_size]
        ~std_input:input Naive program name expected
;;

let tanf name program _ expected =
  name >:: fun _ -> assert_equal expected (anf (tag program)) ~printer:string_of_aprogram
;;

let tfvs name program expected =
  name
  >:: fun _ ->
  let ast = parse_string name program in
  let anfed = anf (tag ast) in
  let cached = free_vars_cache anfed in
  match cached with
  | AProgram (_, (_, fvs)) ->
      let vars = StringSet.elements fvs in
      let c = Stdlib.compare in
      let str_list_print strs = "[" ^ ExtString.String.join ", " strs ^ "]" in
      assert_equal (List.sort c vars) (List.sort c expected) ~printer:str_list_print
;;

(* EQUALITY HELPERS *)

let t_eq name actual expected = name >:: fun _ -> assert_equal expected actual ~printer:(fun s -> s)

(* a helper for testing primitive values (won't print datatypes as well) *)
let t_any name value expected = name >:: fun _ -> assert_equal expected value ~printer:ExtLib.dump

(* a helper for testing progs, printing in a nicer form *)
let t_prog name value expected =
  name >:: fun _ -> assert_equal expected value ~printer:string_of_program
;;

(* a helper for parsing input and calling some function on it, for unit tests.
   pattern: we pass in print and fun first, so we can make function-specific
   helpers below (placed next to the unit tests) *)
let t_parse print func name input expected =
  name >:: fun _ -> assert_equal expected (func (parse_string name input)) ~printer:print
;;

(* helper to make pretty-printing functions for lists *)
let list_printer (print : 'a -> string) (lst : 'a list) : string =
  let rec helper (l : 'a list) : string =
    match l with
    | [] -> ""
    (* prevents trailing space *)
    | [x] -> print x
    | x :: rest -> print x ^ "; " ^ helper rest
  in
  "[" ^ helper lst ^ "]"
;;

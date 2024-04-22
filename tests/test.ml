open Compile
open Runner
open Printf
open OUnit2
open Pretty
open Exprs
open Phases
open Errors
open Assembly
open Graph

(* to easily switch between allocation strategies, swap this.
   note that this does not change garbage collection related helpers,
   since some garbage collection tests change behavior (ex: things that
   would be kept on the stack are now overwritten in registers, and are
   garbage earlier than the tests expect) *)

let alloc_strategy = Register

(* for now, everything but tr uses Naive register allocation
   we can revisit these test helpers to make them more flexible later *)

let t name program input expected =
  name >:: test_run ~args:[] ~std_input:input alloc_strategy program name expected
;;

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

let terr name program input expected =
  name >:: test_err ~args:[] ~std_input:input alloc_strategy program name expected
;;

let tgcerr ?(no_builtins = false) name heap_size program input expected =
  name
  >:: test_err ~no_builtins
        ~args:[string_of_int heap_size]
        ~std_input:input Naive program name expected
;;

let tanf name program input expected =
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

let list_to_graph lst =
  Graph.of_seq (List.to_seq (List.map (fun (key, xs) -> (key, NeighborSet.of_list xs)) lst))
;;

(* INTEGRATION TESTS START HERE *)

let prim1_suite =
  "prim1_tests"
  >::: [ t "num" "60" "" "60";
         t "false" "false" "" "false";
         t "true" "true" "" "true";
         t "add1" "add1(11)" "" "12";
         t "sub1" "sub1(11)" "" "10";
         terr "bools_throw_arith_err1" "add1(false)" "" "arithmetic expected a number, got false";
         terr "bools_throw_arith_err2" "sub1(true)" "" "arithmetic expected a number, got true";
         t "not_true" "!(true)" "" "false";
         t "not_false" "!(false)" "" "true";
         terr "num_throws_logic_err" "!(1)" "" "logic expected a boolean, got 1";
         terr "neg_throws_logic_err" "!(-4)" "" "logic expected a boolean, got -4";
         (* typechecking predicates: *)
         (* isbool *)
         t "test_is_bool1" "isbool(true)" "" "true";
         t "test_is_bool2" "isbool(false)" "" "true";
         t "test_is_bool3" "isbool(0)" "" "false";
         t "test_is_bool4" "isbool(123)" "" "false";
         t "test_is_bool5" "isbool((0,123))" "" "false";
         t "test_is_bool6" "isbool((true,123))" "" "false";
         t "test_is_bool7" "isbool((123,123))" "" "false";
         t "test_is_bool8" "isbool((false,123))" "" "false";
         (* istuple *)
         t "test_is_tuple1" "istuple(true)" "" "false";
         t "test_is_tuple2" "istuple(false)" "" "false";
         t "test_is_tuple3" "istuple(0)" "" "false";
         t "test_is_tuple4" "istuple(123)" "" "false";
         t "test_is_tuple5" "istuple((0,123))" "" "true";
         t "test_is_tuple6" "istuple((true,123))" "" "true";
         t "test_is_tuple7" "istuple((123,123))" "" "true";
         t "test_is_tuple8" "istuple((false,123))" "" "true";
         (* isnum *)
         t "test_is_num1" "isnum(true)" "" "false";
         t "test_is_num2" "isnum(false)" "" "false";
         t "test_is_num3" "isnum(0)" "" "true";
         t "test_is_num4" "isnum(123)" "" "true";
         t "test_is_num5" "isnum((0,123))" "" "false";
         t "test_is_num6" "isnum((true,123))" "" "false";
         t "test_is_num7" "isnum((123,123))" "" "false";
         t "test_is_num8" "isnum((false,123))" "" "false" ]
;;

let overflow_suite =
  "overflow_tests"
  >::: [ terr "static_error" "4611686018427387904" "" "The number literal";
         t "static_ok" "4611686018427387903" "" "4611686018427387903";
         terr "static_error_neg" "-4611686018427387905" "" "The number literal";
         t "static_ok_neg" "-4611686018427387904" "" "-4611686018427387904";
         terr "runtime_error_add1" "add1(4611686018427387903)" "" "Error: Integer overflow";
         t "runtime_ok_add1" "add1(4611686018427387902)" "" "4611686018427387903";
         terr "runtime_error_sub1" "sub1(-4611686018427387904)" "" "Error: Integer overflow";
         t "runtime_ok_neg_sub1" "sub1(-4611686018427387903)" "" "-4611686018427387904";
         terr "runtime_error_add" "4611686018427387903 + 69" "" "Error: Integer overflow";
         terr "runtime_error_sub" "-4611686018427387892 - 69" "" "Error: Integer overflow";
         t "runtime_ok_mul1" "1152921504606846979 * -3" "" "-3458764513820540937";
         t "runtime_ok_mul2" "3 * -1152921504606846979" "" "-3458764513820540937";
         terr "runtime_error_mul" "1152921504606846976 * 4" "" "Error: Integer overflow" ]
;;

let prim2_suite =
  "prim2_tests"
  >::: [ t "add" "2 + 3" "" "5";
         t "add_neg" "-2 + 3" "" "1";
         t "add_neg2" "-2 + -3" "" "-5";
         t "sub" "10 - 6" "" "4";
         t "sub_neg" "6 - 10" "" "-4";
         t "sub_neg2" "-6 - 10" "" "-16";
         t "mul" "2 * 3" "" "6";
         t "mul_neg" "-69 * 420" "" "-28980";
         (* type checking *)
         terr "prim2_fail_right" "2 + true" "" "arithmetic expected a number, got true";
         terr "prim2_fail_left" "false - 4" "" "arithmetic expected a number, got false";
         terr "prim2_fail_blame" "true * false" "" "arithmetic expected a number, got true" ]
;;

let comparison_suite =
  "comparison_tests"
  >::: [ (* less than *)
         t "less_is_less" "1 < 2" "" "true";
         t "eq_isnt_less" "1 < 1" "" "false";
         t "more_isnt_less" "2 < 1" "" "false";
         t "less_neg" "-10 < -5" "" "true";
         (* less than or equal to *)
         t "less_is_leq" "3 <= 4" "" "true";
         t "eq_is_leq" "4 <= 4" "" "true";
         t "more_isnt_leq" "4 <= 3" "" "false";
         (* greater than *)
         t "less_isnt_more" "5 > 6" "" "false";
         t "eq_isnt_more" "6 > 6" "" "false";
         t "more_is_more" "6 > 5" "" "true";
         t "more_neg" "-10 > -5" "" "false";
         (* greater than or equal to *)
         t "less_isnt_geq" "7 >= 8" "" "false";
         t "eq_is_geq" "8 >= 8" "" "true";
         t "more_is_geq" "8 >= 7" "" "true";
         (* type checking *)
         terr "bool_throws_compare1" "true < 2" "" "Error: comparison expected a number, got true";
         terr "bool_throws_compare2" "3 <= false" ""
           "Error: comparison expected a number, got false";
         terr "bool_throws_blame" "true > false" "" "Error: comparison expected a number, got true"
       ]
;;

let logic_suite =
  "logic_tests"
  >::: [ (* logical and *)
         t "true_and_true" "true && true" "" "true";
         t "true_and_false" "true && false" "" "false";
         t "false_and_true" "false && true" "" "false";
         t "false_and_false" "false && false" "" "false";
         t "and_short_circuit" "false && print(4)" "" "false";
         terr "and_num_throws1" "4 && false" "" "logic expected a boolean, got 4";
         terr "and_num_throws2" "true && 6" "" "logic expected a boolean, got 6";
         (* logical or *)
         t "true_or_true" "true || true" "" "true";
         t "true_or_false" "true || false" "" "true";
         t "false_or_true" "false || true" "" "true";
         t "false_or_false" "false || false" "" "false";
         t "or_short_circuit" "true || print(4)" "" "true";
         terr "or_num_throws1" "4 || true" "" "logic expected a boolean, got 4";
         terr "or_num_throws2" "false || 6" "" "logic expected a boolean, got 6" ]
;;

let if_suite =
  "if_tests"
  >::: [ t "if_true" "if true: 1 else: 0" "" "1";
         t "if_false" "if false: 1 else: 0" "" "0";
         terr "if_num_throws" "if 54: true else: false" "" "Error: if expected a boolean, got 54";
         t "if_non_imm" "if !(!(1 < 2)): true else: false" "" "true";
         t "if_all_non_imm" "if !(!(1 > 2)): 9 + 10 else: sub1(22)" "" "21";
         terr "if_throws_non_imm" "if add1(add1(9 + 10)): 1 + false else: false - 69" ""
           "Error: if expected a boolean, got 21" ]
;;

(* also has sequencing, since these have clear side-effects
   equality is tested directly below *)
let native_suite =
  "native_function_tests"
  >::: [ t "prints_negs" "print(-500)" "" "-500\n-500";
         t "prints_true" "print(true)" "" "true\ntrue";
         t "prints_false" "print(false)" "" "false\nfalse";
         t "prints_nums" "print(12)" "" "12\n12";
         t "prints_tuples_single" "print((1,))" "" "(1, )\n(1, )";
         t "prints_tuples_simple" "print((1,2))" "" "(1, 2)\n(1, 2)";
         t "prints_tuples_nested" "print((3, ((4, true), 5)))" ""
           "(3, ((4, true), 5))\n(3, ((4, true), 5))";
         t "prints_empty" "print(())" "" "()\n()";
         t "prints_nil" "print(nil)" "" "nil\nnil";
         (* input tests *)
         t "input_nums" "input() + input()" "3\n4" "7";
         t "input_nums_neg" "input() + input()" "0\n-4" "-4";
         (* sequencing tests *)
         t "print_sequence" "print(69); 420" "" "69\n420";
         t "sequence_complex" "let x = input() in print(x); 420" "4" "4\n420";
         t "multi_sequence" "print(1); print(2); 3" "" "1\n2\n3" ]
;;

let equality_suite =
  "equality_tests"
  >::: [ (* pointer equality *)
         t "true_is_true" "true == true" "" "true";
         t "false_is_false" "false == false" "" "true";
         t "bools_are_different" "true == false" "" "false";
         t "two_is_two" "2 == 2" "" "true";
         t "two_isnt_-two" "2 == -2" "" "false";
         t "eq_no_throws1" "true == 1" "" "false";
         t "eq_no_throws2" "false == 0" "" "false";
         t "nil_is_nil" "nil == nil" "" "true";
         t "tuple_isnt_tuple" "(3, 4) == (3, 4)" "" "false";
         t "same_tuple_same" "let x = (1,2) in x == x" "" "true";
         t "empty_isnt_empty" "() == ()" "" "false";
         (* structural equality *)
         t "true_equals_true" "equal(true, true)" "" "true";
         t "false_equals_false" "equal(false, false)" "" "true";
         t "bools_equal_different" "equal(true, false)" "" "false";
         t "two_equal_two" "equal(2, 2)" "" "true";
         t "two_isnteq_-two" "equal(2, -2)" "" "false";
         (* more interesting structural tests *)
         t "nil_equal_nil" "equal(nil, nil)" "" "true";
         t "empty_equal_empty" "equal((), ())" "" "true";
         t "tuple_equal_simple" "equal((3, 4), (3, 4))" "" "true";
         t "tuple_equal_nested" "equal((3, ((4, true), 5)), (3, ((4, true), 5)))" "" "true";
         t "tuple_not_equal" "equal((3, ((4, false), 5)), (3, ((4, true), 5)))" "" "false" ]
;;

(* tuples are tested through the list library too *)
let tuple_suite =
  "tuple_tests"
  >::: [ t "tup1"
           "let t = (4, (5, 6)) in\n\
           \            begin\n\
           \              t[0] := 7;\n\
           \              t\n\
           \            end" "" "(7, (5, 6))";
         t "tup2"
           "let t = (4, (5, nil)) in\n\
           \            begin\n\
           \              t[1] := nil;\n\
           \              t\n\
           \            end" "" "(4, nil)";
         t "tup3" "let t = (4, 6) in (t, t)" "" "((4, 6), (4, 6))";
         t "nil_safe" "let t = (nil,) in t[0]" "" "nil";
         t "access_computation" "(1,2,3,4)[2+1]" "" "4";
         t "tuple_order_of_eval" "(print(1), print(2))" "" "1\n2\n(1, 2)";
         (* errors *)
         terr "nil_deref" "let t = (nil) in t[0][0]" "" "access component of nil";
         terr "nil_mutate" "let n = nil in n[0] := true" "" "access component of nil";
         terr "get_not_tuple" "1[2]" "" "get expected tuple, got 1";
         terr "tuple_get_idx_low" "let x = (1, 2) in x[-1]" "" "index too small to get, got -1";
         terr "tuple_get_idx_high" "let x = (1, 2) in x[2]" "" "index too large to get, got 2";
         terr "tuple_get_idx_bool" "let x = (1, 2) in x[true]" "" "expected numeric index, got true";
         terr "tuple_set_idx_low" "let x = (1, 2) in x[-69] := 10" ""
           "index too small to set, got -69";
         terr "tuple_set_idx_high" "let x = (1, 2) in x[69] := 10" ""
           "index too large to set, got 69";
         terr "tuple_get_idx_bool" "let x = (1, 2) in x[true] := 10" ""
           "expected numeric index, got true";
         terr "set_not_tuple" "1[2] := 3" "" "set expected tuple, got 1" ]
;;

let binding_suite =
  "binding_tests"
  >::: [ t "destructure_simple" "let (x, y) = (3, 4) in x + y" "" "7";
         t "blank_simple" "let _ = print(1) in 2" "" "1\n2";
         t "destructure_tuple_nested"
           "\n  let t = (3, ((4, true), 5)) in\nlet (x, (y, z)) = t in \nx + y[0] + z" "" "12";
         t "function_blank" "def f(_, n):n\nf(print(4),5)" "" "4\n5";
         t "function_destruct"
           "def addpairs((x1, y1), (x2, y2)):\n(x1 + x2, y1 + y2)\naddpairs((1, 2), (3, 4))" ""
           "(4, 6)";
         terr "tuple_bad_pattern" "let (x, y) = 1 in 2" "" "pattern expected a tuple, got 1";
         terr "tuple_bad_pattern_size" "let (x, y) = (1,2,3) in 2" ""
           "pattern expected a tuple of size 2" ]
;;

(* note that we desugar decls into lambdas, so most tests in input/do_pass implicitly
   test lambdas as well. See nestedMutualRec.fdl and mapPrint.fdl especially. *)
let lambda_suite =
  "lambda_tests"
  >::: [ t "just_closure" "(lambda (x): x)" "" "<function>";
         t "curried_add" "(lambda (x): (lambda (y): x + y))(2)(3)" "" "5";
         t "eval_order" "let id = (lambda (x): x) in print(id)(print(1))" "" "<function>\n1\n1";
         t "function_as_input" "def applyToFive(f): f(5)\napplyToFive((lambda (x): x + 1))" "" "6";
         t "function_as_output" "def two(f): (lambda (x): f(f(x)))\ntwo((lambda (x): x + 1))(1)" ""
           "3";
         t "native_as_input" "def applyToFive(f): f(5)\napplyToFive(print)" "" "5\n5";
         t "thunk" "(lambda: true)()" "" "true";
         (* errors *)
         terr "call_not_fun" "1(1)" "" "tried to call a non-closure value, got 1";
         terr "call_wrong_arity" "(lambda (x): x)(1,2)" "" "arity mismatch in call";
         terr "lambda_body_error" "(lambda (x): x + 1)(true)" "" "arithmetic expected a number" ]
;;

(* most of the tests above cover specific behavior,
   these test that all behavior composes correctly.
   most of the larger test cases are present in input/do_pass *)
let compositionality_suite =
  "compositionality_and_let_tests"
  >::: [ t "nested_no_shadow" "let a = 10, c = (let b = add1(a), d = add1(b) in add1(b)) in sub1(c)"
           "" "11";
         t "let_sequential" "let x = 1, y = 2, z = x + y in x + z" "" "4";
         t "if_same_names" "if false: (let x = 1 in x) else: (let x = 2 in x)" "" "2";
         t "no_accidental_shadowing" "(let x = 1 in x) + (let x = 2 in x)" "" "3";
         t "ifs_and_lets"
           "let x = (if (4 < 5): false else: true) in let y = (if x: add1(20) else: 9+10) in y" ""
           "19";
         t "closure_in_tup"
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            garbo();\n\
            let f = (lambda (x): x) in\n\
            let t = (true, f) in\n\
            let trigger = (true, false, true) in\n\
            t[1](42)"
           "" "42";
         terr "error_in_called_fun" "def f(x, y): x + y\nf(true, false)" ""
           "arithmetic expected a number" ]
;;

let tgc_no = tgc ~no_builtins:true

let tgcerr_no = tgcerr ~no_builtins:true

(* helper that ensures that the program passes with the given heap size,
   but fails with a heap size that is just smaller. runs without built-ins *)
let tgc_just name size output prog =
  [tgcerr_no (name ^ "_fails") (size - 2) prog "" "Out of memory"; tgc_no name size prog "" output]
;;

let gc_suite =
  "garbage_collection_tests"
  >::: [ tgc_no "no_alloc_ok" 0 "100" "" "100";
         tgcerr_no "empty_heap_fails" 0 "(1,2)" "" "Allocation error";
         tgcerr_no "heap_too_small_still" 3 "(1,2)" "" "Allocation error";
         tgc_no "perfect_sized_heap" 4 "(1,2)" "" "(1, 2)";
         tgcerr_no "runs_oom" 4 "(1, (2, 3))" "" "Out of memory";
         tgc_no "gc_triggered" 4 "(1, 2, 3); (4, 5, 6)" "" "(4, 5, 6)";
         tgc_no "lambda_triggers_gc" 4 "(1, 2, 3); (lambda (x): x)(5)" "" "5";
         tgcerr "defaults_allocated_not_enough" 10 "5" "" "Out of memory";
         tgc "defaults_allocated_enough" 12 "5" "" "5";
         tgc "defaults_work" 20
           "def garbo(): let a = (1, 2, 3) in 0\ngarbo(); garbo(); equal(print(1), print(2))" ""
           "1\n2\nfalse";
         (* note this will pass with fewer than 20 words, but this tests
            collecting garbage from the body of a function *)
         tgc_no "collect_during_body" 20
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            def aliases(f, t1, t2):\n\
           \  let g = (4, 5, 6) in # <- GC will be triggered right here\n\
           \  f(t1[0] + t2[0] + g[2])\n\
            garbo();\n\
            let foo = (lambda (x): x) in\n\
            let tup = (4, 5) in\n\
            # right now, 20 words taken up, 4 of which are garbage\n\
            aliases(foo, tup, tup)" "" "14" ]
       @ tgc_just "cycles_get_copied" 20 "3"
           "# closure will take up 4 words, and will allocate 4 words when called\n\
            def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            # s, t, and l will be 4 words each\n\
            let s = (1, 2, true) in\n\
            let t = (3, s) in\n\
            let l = (s, nil) in\n\
            # make l be cyclic\n\
            let _ = l[1] := l in\n\
            # second and third garbo calls will trigger GCs\n\
            garbo(); garbo(); garbo();\n\
            l[1][0][0] + t[1][1]"
       @ tgc_just "tuple_from_func" 20 "72"
           "def garbo():\n\
           \  let a = (1, 2, nil) in 0\n\
            def tup():\n\
            let t = (7, 8, 9) in\n\
           \  (true, t, t)\n\
            let tp = tup() in\n\
            # at this point, 16 words have been allocated\n\
            garbo(); garbo(); garbo();\n\
            tp[1][1] * tp[2][2]"
       @ tgc_just "collect_mid_letrec" 12 "4"
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            garbo();\n\
            let rec foo = (lambda (x): x + 1),\n\
            # GC gets triggered here: 4 words for each of garbo, foo, bar\n\
           \  bar = (lambda (y): y + 2) in\n\
            foo(bar(1))"
       @ tgc_just "curried" 12 "5"
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            garbo(); (lambda (x): (lambda (y): x + y))(2)(3)"
       @ tgc_just "many_closed_over" 14 "3"
           "def bigger_garbo():\n\
           \  let a = (1, 2, 3, 4, 5) in 0\n\
            def foo():\n\
           \  let x = 1, y = 2 in\n\
           \  # needs all 6 words to fit\n\
           \  (lambda: x + y)\n\
            bigger_garbo();\n\
            # currently 8 words of closures and 6 words of garbage\n\
            foo()()"
       @ tgc_just "closure_in_tuple" 16 "42"
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            garbo();\n\
            let f = (lambda (x): x) in\n\
            let t = (true, f) in\n\
            # all 16 words used, 4 of which are garbage\n\
            let trigger = (true, false, true) in\n\
            t[1](42)"
       @ tgc_just "tuple_in_closure" 20 "6"
           "def garbo():\n\
           \  let a = (1, 2, 3) in 0\n\
            def tpHere():\n\
           \  let x = (5, 6, 7, 8, 9, 10) in  # takes up 8 words\n\
           \  (lambda (y): y + x[0])\n\
            # tpHere allocates 12 words when called, so we fill 12 words of garbo\n\
            garbo(); garbo(); garbo(); tpHere()(1)"
;;

(* UNIT TESTS *)

(* we erase here, since we test that location information is preserved through t_wf_err *)
let t_desugar = t_parse string_of_program (fun p -> untagP (desugar p))

(* TESTS:
   - desugar case using `and` for decls
   - desugar case that has shadow in a binding and preserves it
     - can probably just add to a current one
     - additional case with shadow before a function
*)
let desugar_suite =
  "desugar_tests"
  >::: [ t_desugar "desugar_and" "true && false"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    EIf
                      ( EPrim1 (Not, EBool (true, ()), ()),
                        EBool (false, ()),
                        EPrim1 (Not, EPrim1 (Not, EBool (false, ()), ()), ()),
                        () ),
                    () ),
                () ) );
         t_desugar "desugar_or" "true || false"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    EIf
                      ( EPrim1 (Not, EBool (true, ()), ()),
                        EPrim1 (Not, EPrim1 (Not, EBool (false, ()), ()), ()),
                        EBool (true, ()),
                        () ),
                    () ),
                () ) );
         t_desugar "desugar_nested_op" "1 + ((true || false) && true)"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    EPrim2
                      ( Plus,
                        ENumber (1L, ()),
                        EIf
                          ( EPrim1
                              ( Not,
                                EIf
                                  ( EPrim1 (Not, EBool (true, ()), ()),
                                    EPrim1 (Not, EPrim1 (Not, EBool (false, ()), ()), ()),
                                    EBool (true, ()),
                                    () ),
                                () ),
                            EBool (false, ()),
                            EPrim1 (Not, EPrim1 (Not, EBool (true, ()), ()), ()),
                            () ),
                        () ),
                    () ),
                () ) );
         t_desugar "desugar_let" "let (x, y) = (1, 2) in x + y"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    ELet
                      ( [ ( BName ("bind_temp_$5", false, ()),
                            ETuple ([ENumber (1L, ()); ENumber (2L, ())], ()),
                            () );
                          ( BBlank (),
                            EPrim2 (CheckSize, EId ("bind_temp_$5", ()), ENumber (2L, ()), ()),
                            () );
                          ( BName ("x", false, ()),
                            EGetItem (EId ("bind_temp_$5", ()), ENumber (0L, ()), ()),
                            () );
                          ( BName ("y", false, ()),
                            EGetItem (EId ("bind_temp_$5", ()), ENumber (1L, ()), ()),
                            () ) ],
                        EPrim2 (Plus, EId ("x", ()), EId ("y", ()), ()),
                        () ),
                    () ),
                () ) );
         t_desugar "nested_tuple"
           "let t = (3, ((4, true), 5)) in let (x, (y, z)) = t in x + y[0] + z"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    ELet
                      ( [ ( BName ("t", false, ()),
                            ETuple
                              ( [ ENumber (3L, ());
                                  ETuple
                                    ( [ ETuple ([ENumber (4L, ()); EBool (true, ())], ());
                                        ENumber (5L, ()) ],
                                      () ) ],
                                () ),
                            () ) ],
                        ELet
                          ( [ (BName ("bind_temp_$6", false, ()), EId ("t", ()), ());
                              ( BBlank (),
                                EPrim2 (CheckSize, EId ("bind_temp_$6", ()), ENumber (2L, ()), ()),
                                () );
                              ( BName ("x", false, ()),
                                EGetItem (EId ("bind_temp_$6", ()), ENumber (0L, ()), ()),
                                () );
                              ( BName ("bind_temp_$7", false, ()),
                                EGetItem (EId ("bind_temp_$6", ()), ENumber (1L, ()), ()),
                                () );
                              ( BBlank (),
                                EPrim2 (CheckSize, EId ("bind_temp_$7", ()), ENumber (2L, ()), ()),
                                () );
                              ( BName ("y", false, ()),
                                EGetItem (EId ("bind_temp_$7", ()), ENumber (0L, ()), ()),
                                () );
                              ( BName ("z", false, ()),
                                EGetItem (EId ("bind_temp_$7", ()), ENumber (1L, ()), ()),
                                () ) ],
                            EPrim2
                              ( Plus,
                                EPrim2
                                  ( Plus,
                                    EId ("x", ()),
                                    EGetItem (EId ("y", ()), ENumber (0L, ()), ()),
                                    () ),
                                EId ("z", ()),
                                () ),
                            () ),
                        () ),
                    () ),
                () ) );
         t_desugar "def_simple" "def f(x): x\nf(1)"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    ELetRec
                      ( [ ( BName ("f", false, ()),
                            ELambda ([BName ("x", false, ())], EId ("x", ()), ()),
                            () ) ],
                        EApp (EId ("f", ()), [ENumber (1L, ())], Unknown, ()),
                        () ),
                    () ),
                () ) );
         t_desugar "def_destruct" "def f((a, b), _, c): a + b + c\nf((1, 2), 3, 4)"
           (Program
              ( [],
                ELetRec
                  ( [ ( BName ("print", false, ()),
                        ELambda
                          ( [BName ("print_$1", false, ())],
                            EApp (EId ("print", ()), [EId ("print_$1", ())], Native "print", ()),
                            () ),
                        () );
                      ( BName ("input", false, ()),
                        ELambda ([], EApp (EId ("input", ()), [], Native "input", ()), ()),
                        () );
                      ( BName ("equal", false, ()),
                        ELambda
                          ( [BName ("equal_$2", false, ()); BName ("equal_$3", false, ())],
                            EApp
                              ( EId ("equal", ()),
                                [EId ("equal_$2", ()); EId ("equal_$3", ())],
                                Native "equal",
                                () ),
                            () ),
                        () ) ],
                    ELetRec
                      ( [ ( BName ("f", false, ()),
                            ELambda
                              ( [ BName ("arg_temp_$4", false, ());
                                  BName ("arg_temp_$5", false, ());
                                  BName ("c", false, ()) ],
                                ELet
                                  ( [ ( BName ("bind_temp_$7", false, ()),
                                        EId ("arg_temp_$4", ()),
                                        () );
                                      ( BBlank (),
                                        EPrim2
                                          (CheckSize, EId ("bind_temp_$7", ()), ENumber (2L, ()), ()),
                                        () );
                                      ( BName ("a", false, ()),
                                        EGetItem (EId ("bind_temp_$7", ()), ENumber (0L, ()), ()),
                                        () );
                                      ( BName ("b", false, ()),
                                        EGetItem (EId ("bind_temp_$7", ()), ENumber (1L, ()), ()),
                                        () ) ],
                                    EPrim2
                                      ( Plus,
                                        EPrim2 (Plus, EId ("a", ()), EId ("b", ()), ()),
                                        EId ("c", ()),
                                        () ),
                                    () ),
                                () ),
                            () ) ],
                        EApp
                          ( EId ("f", ()),
                            [ ETuple ([ENumber (1L, ()); ENumber (2L, ())], ());
                              ENumber (3L, ());
                              ENumber (4L, ()) ],
                            Unknown,
                            () ),
                        () ),
                    () ),
                () ) ) ]
;;

(* if there are no well-formedness errors, it should be the same as desugaring *)
let t_wf name input =
  name
  >:: fun _ ->
  let expected = desugar (parse_string name input) in
  let result = Result.get_ok (is_well_formed (desugar (parse_string name input))) in
  assert_equal expected result ~printer:string_of_program
;;

(* alternatively, use (list_printer print_error) for more readable errors *)
let t_wf_err = t_parse ExtLib.dump (fun p -> Result.get_error (is_well_formed (desugar p)))

let well_formed_suite =
  "well_formed_tests"
  >::: [ (* properly well-formed programs: *)
         t_wf "let_rec_okay" "let rec foo = (lambda (x): foo(x)) in foo(1)";
         t_wf "separate_repeated_names" "def f(x): x\ndef g(x):x\nlet x = 5 in x";
         t_wf "shadow_fun_with_keyword" "def f(x): x\ndef shadow f(x, y): x + y\n4";
         t_wf "shadow_bind_with_keyword" "let x = 5 in let shadow x = 6 in x";
         t_wf "shadow_native_with_keyword" "def shadow equal(x, y): x == y\n4";
         t_wf "shadow_name_arg_with_keyword" "let x = 4, foo = (lambda (shadow x): x) in foo(x)";
         t_wf "letrec_okay" "let rec foo = (lambda (x): foo(x)) in foo(1)";
         (* tests for single errors *)
         t_wf_err "letrec_not_function" "let rec foo = 2 in foo"
           [ LetRecNonFunction
               ( ENumber
                   ( 2L,
                     SourceLoc
                       ( { Lexing.pos_fname= "letrec_not_function";
                           pos_lnum= 1;
                           pos_bol= 0;
                           pos_cnum= 14 },
                         { Lexing.pos_fname= "letrec_not_function";
                           pos_lnum= 1;
                           pos_bol= 0;
                           pos_cnum= 15 } ) ),
                 SourceLoc
                   ( {Lexing.pos_fname= "letrec_not_function"; pos_lnum= 1; pos_bol= 0; pos_cnum= 8},
                     {Lexing.pos_fname= "letrec_not_function"; pos_lnum= 1; pos_bol= 0; pos_cnum= 15}
                   ) ) ];
         t_wf_err "let_no_rec" "let foo = (lambda (x): foo(x)) in foo(1)"
           [ UnboundId
               ( "foo",
                 SourceLoc
                   ( {Lexing.pos_fname= "let_no_rec"; pos_lnum= 1; pos_bol= 0; pos_cnum= 23},
                     {Lexing.pos_fname= "let_no_rec"; pos_lnum= 1; pos_bol= 0; pos_cnum= 26} ) ) ];
         t_wf_err "unbound_fun" "def foo(x): x\nbar(1)"
           [ UnboundId
               ( "bar",
                 SourceLoc
                   ( {Lexing.pos_fname= "unbound_fun"; pos_lnum= 2; pos_bol= 14; pos_cnum= 14},
                     {Lexing.pos_fname= "unbound_fun"; pos_lnum= 2; pos_bol= 14; pos_cnum= 17} ) )
           ];
         t_wf_err "unbound_id_fun" "def f(x): y\n4"
           [ UnboundId
               ( "y",
                 SourceLoc
                   ( {Lexing.pos_fname= "unbound_id_fun"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10},
                     {Lexing.pos_fname= "unbound_id_fun"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11} )
               ) ];
         t_wf_err "unbound_id_body" "x"
           [ UnboundId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "unbound_id_body"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "unbound_id_body"; pos_lnum= 1; pos_bol= 0; pos_cnum= 1} )
               ) ];
         t_wf_err "let_dupes" "let x = 4, x = 5 in x"
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "let_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11},
                     {Lexing.pos_fname= "let_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 12} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "let_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 4},
                     {Lexing.pos_fname= "let_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 5} ) ) ];
         t_wf_err "shadow_no_keyword" "let x = 5 in let x = 6 in x"
           [ ShadowId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "shadow_no_keyword"; pos_lnum= 1; pos_bol= 0; pos_cnum= 17},
                     {Lexing.pos_fname= "shadow_no_keyword"; pos_lnum= 1; pos_bol= 0; pos_cnum= 18}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "shadow_no_keyword"; pos_lnum= 1; pos_bol= 0; pos_cnum= 4},
                     {Lexing.pos_fname= "shadow_no_keyword"; pos_lnum= 1; pos_bol= 0; pos_cnum= 5}
                   ) ) ];
         t_wf_err "shadow_mixed" "let x = 4, foo = (lambda (x): x) in foo(x)"
           [ ShadowId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "shadow_mixed"; pos_lnum= 1; pos_bol= 0; pos_cnum= 26},
                     {Lexing.pos_fname= "shadow_mixed"; pos_lnum= 1; pos_bol= 0; pos_cnum= 27} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "shadow_mixed"; pos_lnum= 1; pos_bol= 0; pos_cnum= 4},
                     {Lexing.pos_fname= "shadow_mixed"; pos_lnum= 1; pos_bol= 0; pos_cnum= 5} ) ) ];
         t_wf_err "nested_tuple_names" "(lambda((x), (shadow x)): 1)"
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "nested_tuple_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 14},
                     {Lexing.pos_fname= "nested_tuple_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 22}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "nested_tuple_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9},
                     {Lexing.pos_fname= "nested_tuple_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10}
                   ) ) ];
         t_wf_err "fun_dupe_ids" "def f(x, x): x\n4"
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "fun_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9},
                     {Lexing.pos_fname= "fun_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "fun_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 6},
                     {Lexing.pos_fname= "fun_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 7} ) ) ];
         t_wf_err "dupe_fun_names" "def f(x): x\ndef f(x, y): x + y\n4"
           [ ShadowId
               ( "f",
                 SourceLoc
                   ( {Lexing.pos_fname= "dupe_fun_names"; pos_lnum= 2; pos_bol= 12; pos_cnum= 12},
                     {Lexing.pos_fname= "dupe_fun_names"; pos_lnum= 2; pos_bol= 12; pos_cnum= 30} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "dupe_fun_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "dupe_fun_names"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11} )
               ) ];
         t_wf_err "static_overflow" "4611686018427387904"
           [ Overflow
               ( 4611686018427387904L,
                 SourceLoc
                   ( {Lexing.pos_fname= "static_overflow"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "static_overflow"; pos_lnum= 1; pos_bol= 0; pos_cnum= 19} )
               ) ];
         (* tests for multiple duplicates: in all cases, we should report errors
            based on the most recent id seen as the "where defined" location.
            also, note that even with `shadow` this is not allowed! *)
         t_wf_err "many_dupe_ids" "def f(y, y, shadow y): y\n4"
           [ DuplicateId
               ( "y",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9},
                     {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 6},
                     {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 7} ) );
             DuplicateId
               ( "y",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 12},
                     {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 20} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9},
                     {Lexing.pos_fname= "many_dupe_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10} ) )
           ];
         t_wf_err "many_dupe_fun_ids" "def f(x): x\ndef f(x): x\ndef f(x): x\n4"
           [ ShadowId
               ( "f",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 2; pos_bol= 12; pos_cnum= 12},
                     {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 2; pos_bol= 12; pos_cnum= 23}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11}
                   ) );
             ShadowId
               ( "f",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 3; pos_bol= 24; pos_cnum= 24},
                     {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 3; pos_bol= 24; pos_cnum= 35}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 2; pos_bol= 12; pos_cnum= 12},
                     {Lexing.pos_fname= "many_dupe_fun_ids"; pos_lnum= 2; pos_bol= 12; pos_cnum= 23}
                   ) ) ];
         t_wf_err "many_dupe_let_ids" "let x = 4, x = 5, x = 6 in 1"
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11},
                     {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 12}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 4},
                     {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 5}
                   ) );
             DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 18},
                     {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 19}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 11},
                     {Lexing.pos_fname= "many_dupe_let_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 12}
                   ) ) ];
         (* tests with multiple different errors *)
         t_wf_err "tons_of_errs" "def f(x, x, y, y): a\ndef f(): b\n4611686018427387904"
           (* intended error: signature errors -> decl errors -> body errors, in order*)
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 10} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 6},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 7} ) );
             DuplicateId
               ( "y",
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 15},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 16} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 12},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 13} ) );
             UnboundId
               ( "a",
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 19},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 20} ) );
             ShadowId
               ( "f",
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 2; pos_bol= 21; pos_cnum= 21},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 2; pos_bol= 21; pos_cnum= 31} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 1; pos_bol= 0; pos_cnum= 20} ) );
             UnboundId
               ( "b",
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 2; pos_bol= 21; pos_cnum= 30},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 2; pos_bol= 21; pos_cnum= 31} ) );
             Overflow
               ( 4611686018427387904L,
                 SourceLoc
                   ( {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 3; pos_bol= 32; pos_cnum= 32},
                     {Lexing.pos_fname= "tons_of_errs"; pos_lnum= 3; pos_bol= 32; pos_cnum= 51} ) )
           ];
         t_wf_err "duplicate_bind_ids" "let (x, x) = (1, 2) in 3"
           [ DuplicateId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "duplicate_bind_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 8},
                     {Lexing.pos_fname= "duplicate_bind_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 9}
                   ),
                 SourceLoc
                   ( {Lexing.pos_fname= "duplicate_bind_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 5},
                     {Lexing.pos_fname= "duplicate_bind_ids"; pos_lnum= 1; pos_bol= 0; pos_cnum= 6}
                   ) ) ];
         t_wf_err "decl_bind_dupes" "def f(x, (_, y, x)): 3\n4"
           [ ShadowId
               ( "x",
                 SourceLoc
                   ( {Lexing.pos_fname= "decl_bind_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 16},
                     {Lexing.pos_fname= "decl_bind_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 17} ),
                 SourceLoc
                   ( {Lexing.pos_fname= "decl_bind_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 6},
                     {Lexing.pos_fname= "decl_bind_dupes"; pos_lnum= 1; pos_bol= 0; pos_cnum= 7} )
               ) ];
         t_wf_err "shadow_native" "def equal(x, y): x == y\n4"
           [ ShadowId
               ( "equal",
                 SourceLoc
                   ( {Lexing.pos_fname= "shadow_native"; pos_lnum= 1; pos_bol= 0; pos_cnum= 0},
                     {Lexing.pos_fname= "shadow_native"; pos_lnum= 1; pos_bol= 0; pos_cnum= 23} ),
                 NativeLoc "equal" ) ] ]
;;

let t_anf =
  t_parse string_of_aprogram (fun p ->
      anf (rename_and_tag (tag (Result.get_ok (is_well_formed (desugar p))))) )
;;

(* UTOP: (anf (rename_and_tag (tag (Result.get_ok (is_well_formed (desugar (parse_string "" str))))) ));; *)
(* most of this code was provided to us, so we just test the extra cases we implemented *)

let anf_suite =
  "anf_tests"
  >::: [ t_anf "func_return_lambda"
           "def foo(w, x, y, z):\n(lambda (a): a + x + z)\nfoo(1, 2, 3, 4)(5)"
           (AProgram
              ( ALetRec
                  ( [ ( "print_4",
                        CLambda
                          ( ["print_$1_9"],
                            ACExpr
                              (CApp
                                 ( ImmId ("print_4", ()),
                                   [ImmId ("print_$1_9", ())],
                                   Native "print",
                                   () ) ),
                            () ) );
                      ( "input_11",
                        CLambda
                          ([], ACExpr (CApp (ImmId ("input_11", ()), [], Native "input", ())), ())
                      );
                      ( "equal_16",
                        CLambda
                          ( ["equal_$2_22"; "equal_$3_23"],
                            ACExpr
                              (CApp
                                 ( ImmId ("equal_16", ()),
                                   [ImmId ("equal_$2_22", ()); ImmId ("equal_$3_23", ())],
                                   Native "equal",
                                   () ) ),
                            () ) ) ],
                    ALetRec
                      ( [ ( "foo_26",
                            CLambda
                              ( ["w_35"; "x_36"; "y_37"; "z_38"],
                                ACExpr
                                  (CLambda
                                     ( ["a_34"],
                                       ALet
                                         ( "binop_30",
                                           CPrim2 (Plus, ImmId ("a_34", ()), ImmId ("x_36", ()), ()),
                                           ACExpr
                                             (CPrim2
                                                ( Plus,
                                                  ImmId ("binop_30", ()),
                                                  ImmId ("z_38", ()),
                                                  () ) ),
                                           () ),
                                       () ) ),
                                () ) ) ],
                        ALet
                          ( "app_41",
                            CApp
                              ( ImmId ("foo_26", ()),
                                [ImmNum (1L, ()); ImmNum (2L, ()); ImmNum (3L, ()); ImmNum (4L, ())],
                                Snake,
                                () ),
                            ACExpr (CApp (ImmId ("app_41", ()), [ImmNum (5L, ())], Snake, ())),
                            () ),
                        () ),
                    () ),
                () ) );
         t_anf "func_anf_blank" "def foo(x, _ , y): x[0] + x[1] + y\nfoo((2, 3), 4, 5)"
           (AProgram
              ( ALetRec
                  ( [ ( "print_4",
                        CLambda
                          ( ["print_$1_9"],
                            ACExpr
                              (CApp
                                 ( ImmId ("print_4", ()),
                                   [ImmId ("print_$1_9", ())],
                                   Native "print",
                                   () ) ),
                            () ) );
                      ( "input_11",
                        CLambda
                          ([], ACExpr (CApp (ImmId ("input_11", ()), [], Native "input", ())), ())
                      );
                      ( "equal_16",
                        CLambda
                          ( ["equal_$2_22"; "equal_$3_23"],
                            ACExpr
                              (CApp
                                 ( ImmId ("equal_16", ()),
                                   [ImmId ("equal_$2_22", ()); ImmId ("equal_$3_23", ())],
                                   Native "equal",
                                   () ) ),
                            () ) ) ],
                    ALetRec
                      ( [ ( "foo_26",
                            CLambda
                              ( ["x_37"; "arg_temp_$4_38"; "y_39"],
                                ALet
                                  ( "get_30",
                                    CGetItem (ImmId ("x_37", ()), ImmNum (0L, ()), ()),
                                    ALet
                                      ( "get_33",
                                        CGetItem (ImmId ("x_37", ()), ImmNum (1L, ()), ()),
                                        ALet
                                          ( "binop_29",
                                            CPrim2
                                              (Plus, ImmId ("get_30", ()), ImmId ("get_33", ()), ()),
                                            ACExpr
                                              (CPrim2
                                                 ( Plus,
                                                   ImmId ("binop_29", ()),
                                                   ImmId ("y_39", ()),
                                                   () ) ),
                                            () ),
                                        () ),
                                    () ),
                                () ) ) ],
                        ALet
                          ( "tuple_41",
                            CTuple ([ImmNum (2L, ()); ImmNum (3L, ())], ()),
                            ACExpr
                              (CApp
                                 ( ImmId ("foo_26", ()),
                                   [ImmId ("tuple_41", ()); ImmNum (4L, ()); ImmNum (5L, ())],
                                   Snake,
                                   () ) ),
                            () ),
                        () ),
                    () ),
                () ) );
         t_anf "func_anf" "def foo(x, y, z): x + y + z\nfoo(1+2, 3+4, 5+6)"
           (AProgram
              ( ALetRec
                  ( [ ( "print_4",
                        CLambda
                          ( ["print_$1_9"],
                            ACExpr
                              (CApp
                                 ( ImmId ("print_4", ()),
                                   [ImmId ("print_$1_9", ())],
                                   Native "print",
                                   () ) ),
                            () ) );
                      ( "input_11",
                        CLambda
                          ([], ACExpr (CApp (ImmId ("input_11", ()), [], Native "input", ())), ())
                      );
                      ( "equal_16",
                        CLambda
                          ( ["equal_$2_22"; "equal_$3_23"],
                            ACExpr
                              (CApp
                                 ( ImmId ("equal_16", ()),
                                   [ImmId ("equal_$2_22", ()); ImmId ("equal_$3_23", ())],
                                   Native "equal",
                                   () ) ),
                            () ) ) ],
                    ALetRec
                      ( [ ( "foo_26",
                            CLambda
                              ( ["x_33"; "y_34"; "z_35"],
                                ALet
                                  ( "binop_29",
                                    CPrim2 (Plus, ImmId ("x_33", ()), ImmId ("y_34", ()), ()),
                                    ACExpr
                                      (CPrim2 (Plus, ImmId ("binop_29", ()), ImmId ("z_35", ()), ())),
                                    () ),
                                () ) ) ],
                        ALet
                          ( "binop_37",
                            CPrim2 (Plus, ImmNum (1L, ()), ImmNum (2L, ()), ()),
                            ALet
                              ( "binop_40",
                                CPrim2 (Plus, ImmNum (3L, ()), ImmNum (4L, ()), ()),
                                ALet
                                  ( "binop_43",
                                    CPrim2 (Plus, ImmNum (5L, ()), ImmNum (6L, ()), ()),
                                    ACExpr
                                      (CApp
                                         ( ImmId ("foo_26", ()),
                                           [ ImmId ("binop_37", ());
                                             ImmId ("binop_40", ());
                                             ImmId ("binop_43", ()) ],
                                           Snake,
                                           () ) ),
                                    () ),
                                () ),
                            () ),
                        () ),
                    () ),
                () ) ) ]
;;

let fv_suite =
  "free_vars_tests"
  >::: [ tfvs "one_free" "x" ["x"];
         tfvs "add_free" "x + y" ["x"; "y"];
         tfvs "seq_free" "x ; y" ["x"; "y"];
         tfvs "tuple_free" "(x,y,(a,b))" ["x"; "y"; "a"; "b"];
         tfvs "lambda_with_free" "(lambda (y): x + y)" ["x"];
         tfvs "lambda_no_free" "(lambda (x): (lambda(y): x + y))" [];
         tfvs "lambda_not_curried" "(lambda (x,y): x + y)" [];
         tfvs "let_binds" "let x = 4 in x + y" ["y"];
         tfvs "mutual_recursion"
           "let rec foo = (lambda (x): foo(bar(x))), bar = (lambda (y): foo(bar(y))) in 1" [];
         tfvs "mutual_recursion_subexpr" "(lambda (x): foo(bar(x)))" ["foo"; "bar"] ]
;;

(* for ease of writing tests, does not rename: SO be careful about names *)
let t_nsa =
  t_parse print_nested_env (fun p ->
      snd (naive_stack_allocation (free_vars_cache (atag (anf (tag (desugar p)))))) )
;;

let t_nsa_no =
  t_parse print_nested_env (fun p ->
      snd
        (naive_stack_allocation (free_vars_cache (atag (anf (tag (desugar ~no_builtins:true p)))))) )
;;

(* this will always be common, since we add this at the start. we take tags as arguments for modularity *)
let anfed_outer_env a c =
  [ (a, [("print_$1", RegOffset (-16, RBP))]);
    (c, [("equal_$3", RegOffset (-24, RBP)); ("equal_$2", RegOffset (-16, RBP))]) ]
;;

let anfed_inner_env =
  [("equal", RegOffset (-24, RBP)); ("input", RegOffset (-16, RBP)); ("print", RegOffset (-8, RBP))]
;;

let stack_allocation_suite =
  "stack_allocation_tests"
  >::: [ t_nsa "default_nsa" "1" ([(0, anfed_inner_env)] @ anfed_outer_env 3 10);
         t_nsa "one_let" "let x = 1 in x"
           ([(0, [("x", RegOffset (-32, RBP))] @ anfed_inner_env)] @ anfed_outer_env 5 12);
         t_nsa "curried" "(lambda (x): (lambda (y): x + y))"
           ( [(0, anfed_inner_env)]
           @ anfed_outer_env 7 14
           @ [ (2, [("x", RegOffset (-16, RBP))]);
               (3, [("x", RegOffset (-24, RBP)); ("y", RegOffset (-16, RBP))]) ] );
         t_nsa "nested_stack_alloc"
           "let rec fact = (lambda (n): let tmp = (fact(n - 1)) in n * tmp) in (let x = 4 in x)"
           ( [(0, [("x", RegOffset (-40, RBP)); ("fact", RegOffset (-32, RBP))] @ anfed_inner_env)]
           @ anfed_outer_env 18 25
           @ [ ( 6,
                 [ ("tmp", RegOffset (-40, RBP));
                   ("binop_32", RegOffset (-32, RBP));
                   ("fact", RegOffset (-24, RBP));
                   ("n", RegOffset (-16, RBP)) ] ) ] );
         t_nsa_no "no_default_env"
           "let rec fact = (lambda (n): let tmp = (fact(n - 1)) in n * tmp) in (let x = 4 in x)"
           [ (0, [("x", RegOffset (-16, RBP)); ("fact", RegOffset (-8, RBP))]);
             ( 5,
               [ ("tmp", RegOffset (-40, RBP));
                 ("binop_10", RegOffset (-32, RBP));
                 ("fact", RegOffset (-24, RBP));
                 ("n", RegOffset (-16, RBP)) ] ) ] ]
;;

(* interference test helper
   DOES NOT RENAME THINGS... so manually rename to avoid conflicts *)
let t_interfere bi out name input_prog expected_list =
  let ap =
    free_vars_cache (atag (anf (tag (desugar ~no_builtins:bi (parse_string name input_prog)))))
  in
  match ap with
  | AProgram (axpr, _) ->
      let res = interfere axpr out in
      let expected = list_to_graph expected_list in
      name
      >:: fun _ ->
      assert_equal true (Graph.equal NeighborSet.equal res expected) ~printer:(fun _ ->
          sprintf "Expected %s, but got %s" (string_of_graph expected) (string_of_graph res) )
;;

(* interference test helper: no built-ins, and starting with live_out = [] *)
let t_interfere_out = t_interfere true StringSet.empty

let interfere_suite =
  "interference_tests"
  >::: [ t_interfere_out "boring_interfere" "let x = 1 in x" [("x", [])];
         t_interfere_out "piazza_interfere" "let x = 1 in let y = 2 in x + y"
           [("x", ["y"]); ("y", ["x"])];
         t_interfere_out "interfere_unused" "let x = 1 in let y = 2 in x"
           [("x", ["y"]); ("y", ["x"])];
         t_interfere_out "let_in_let_interf" "let x = (let y = 2 in y) in x" [("x", []); ("y", [])];
         t_interfere_out "interfere_if"
           "let x = true in\nlet y = if true: let b = 5 in b else: 6 in\nx"
           [("x", ["b"; "y"]); ("y", ["x"]); ("b", ["x"])];
         t_interfere_out "interfere_seq"
           "(let x = 1 in let y = 2 in x + y); (let a = 3 in let b = 4 in a + b)"
           [("x", ["y"]); ("y", ["x"]); ("a", ["b"]); ("b", ["a"])];
         t_interfere_out "interefere_rec"
           "let f = (lambda (x) : x) in \n\
            let rec foo = (lambda (y): f(y)),\n\
            bar = (lambda (z): foo(z))\n\
            in bar(5)"
           [("f", ["foo"; "bar"]); ("foo", ["f"; "bar"]); ("bar", ["f"; "foo"])];
         (* these can all go in the same register, since we are basically just moving it along *)
         t_interfere_out "lots_of_lets" "let a = 1 in let b = a in let c = b in let d = c in d"
           [("a", []); ("b", []); ("c", []); ("d", [])];
         (* x and y both in closure, so must have an interference edge *)
         t_interfere_out "closure_not_body" "(lambda (z): x + y + z)" [("x", ["y"]); ("y", ["x"])];
         t_interfere_out "interference_many_nested"
           "let x = (if true: (let rec foo = (lambda (z): z) in foo) else: (let y = 12 in y)) in x"
           [("x", []); ("y", []); ("foo", [])];
         t_interfere_out "interference_many_nested2"
           "let n = 4 in let x = (if true: (let rec foo = (lambda (z): z + n) in foo) else: (let y \
            = 12 in y)) in x + n"
           [("foo", ["n"]); ("x", ["n"]); ("y", ["n"]); ("n", ["x"; "y"; "foo"])] ]
;;

(* for now, this compares list equality (so order matters),
   which is a little annoying to work with but oh well *)
let t_color_list init_env name lst expected =
  name
  >:: fun _ -> assert_equal expected (color_graph (list_to_graph lst) init_env) ~printer:ExtLib.dump
;;

(* no init_env (pre-coloring) here *)
let t_color_no = t_color_list []

let t_color_graph init_env name graph expected =
  name
  >:: fun _ ->
  assert_equal expected (color_graph graph init_env)
    ~printer:(list_printer (fun (name, arg) -> sprintf "%s |-> %s\n" name (arg_to_asm arg)))
;;

(* coloring is tested implicitly by the integration tests above.
   we explicitly test some interesting cases about stack spilling
   that may be rare to occur in practice (requires tons of vars) *)
let color_suite =
  "coloring_tests"
  >::: [ t_color_no "three_clique"
           [("a", ["b"; "c"]); ("b", ["a"; "c"]); ("c", ["a"; "b"])]
           [("a", Reg R14); ("b", Reg R13); ("c", Reg R12)];
         t_color_no "two_components"
           [("x", ["y"]); ("y", ["x"]); ("a", ["b"]); ("b", ["a"])]
           [("a", Reg R13); ("b", Reg R12); ("x", Reg R13); ("y", Reg R12)];
         t_color_no "three_tree_clique"
           [("a", ["b"; "c"]); ("b", ["a"]); ("c", ["a"])]
           [("b", Reg R12); ("a", Reg R13); ("c", Reg R12)];
         t_color_graph [] "k11_uses_stack" (complete_graph 11)
           [ ("0", RegOffset (-8, RBP));
             ("1", Reg R9);
             ("10", Reg R8);
             ("2", Reg RCX);
             ("3", Reg RDX);
             ("4", Reg RSI);
             ("5", Reg RDI);
             ("6", Reg RBX);
             ("7", Reg R14);
             ("8", Reg R13);
             ("9", Reg R12) ];
         t_color_graph
           [("foo", RegOffset (-8, RBP)); ("bar", RegOffset (-16, RBP))]
           "spill_correct_with_precolor" (complete_graph 12)
           [ ("0", RegOffset (-32, RBP));
             ("1", RegOffset (-24, RBP));
             ("10", Reg R9);
             ("11", Reg R8);
             ("2", Reg RCX);
             ("3", Reg RDX);
             ("4", Reg RSI);
             ("5", Reg RDI);
             ("6", Reg RBX);
             ("7", Reg R14);
             ("8", Reg R13);
             ("9", Reg R12);
             ("foo", RegOffset (-8, RBP));
             ("bar", RegOffset (-16, RBP)) ] ]
;;

let t_reg bi =
  t_parse print_nested_env (fun p ->
      snd (register_allocation (free_vars_cache (atag (anf (tag (desugar ~no_builtins:bi p)))))) )
;;

let t_reg_no = t_reg true

let t_reg_bi = t_reg false

(* this will always be common, since we add this at the start. we take tags as arguments for modularity *)
let reg_outer_env a b c =
  [ ( a,
      [ ("$$closure$$", RegOffset (-8, RBP));
        ("equal_$2", RegOffset (-16, RBP));
        ("equal_$3", RegOffset (-24, RBP)) ] );
    (b, [("$$closure$$", RegOffset (-8, RBP))]);
    (c, [("$$closure$$", RegOffset (-8, RBP)); ("print_$1", RegOffset (-16, RBP))]) ]
;;

let reg_inner_env = [("equal", Reg R14); ("input", Reg R13); ("print", Reg R12)]

(* the correctness of this function is mostly implied by the correctness
   of interfere and color, which are test above. the interesting bits come with
   varieties of lambdas in interesting places, which we try to test here.
   this is also implicitly tested by all of the integration tests above
*)

let reg_alloc_suite =
  "register_allocation_tests"
  >::: [ t_reg_bi "simple_reg" "1" (reg_outer_env 10 7 3 @ [(0, reg_inner_env)]);
         t_reg_bi "bi_no_conflict" "let x = 4 in x"
           (reg_outer_env 12 9 5 @ [(0, ("x", Reg R12) :: reg_inner_env)]);
         t_reg_bi "bi_some_conflict" "let x = 4 in print(x)"
           (reg_outer_env 14 11 7 @ [(0, ("x", Reg R13) :: reg_inner_env)]);
         t_reg_no "nested_reg_alloc"
           (* the lambda inner env is the interesting part here *)
           "let rec fact = (lambda (n): let tmp = (fact(n - 1)) in n * tmp) in (let x = 4 in x)"
           [ ( 5,
               [ ("tmp", Reg R12);
                 ("binop_10", Reg R13);
                 ("fact", Reg R12);
                 ("$$closure$$", RegOffset (-8, RBP));
                 ("n", RegOffset (-16, RBP)) ] );
             (0, [("fact", Reg R12); ("x", Reg R12)]) ];
         t_reg_no "reg_weird_nesting"
           "let n = 4 in let x = (if true: (let rec foo = (lambda (z): z + n) in foo) else: (let y \
            = 12 in y)) in x + n"
           [ ( 12,
               [("n", Reg R12); ("$$closure$$", RegOffset (-8, RBP)); ("z", RegOffset (-16, RBP))]
             );
             (0, [("foo", Reg R12); ("x", Reg R12); ("n", Reg R13); ("y", Reg R12)]) ];
         t_reg_no "reg_curried_add" "(lambda (x): (lambda (y): x + y))(2)(3)"
           [ ( 10,
               [("x", Reg R12); ("$$closure$$", RegOffset (-8, RBP)); ("y", RegOffset (-16, RBP))]
             );
             (9, [("$$closure$$", RegOffset (-8, RBP)); ("x", RegOffset (-16, RBP))]);
             (0, [("app_4", Reg R12); ("lambda_6", Reg R12)]) ];
         t_reg_no "reg_letrec_lots"
           "let f = (lambda (x) : x) in \n\
            let rec foo = (lambda (y): f(y)),\n\
            bar = (lambda (z): foo(z))\n\
            in bar(5)"
           [ ( 10,
               [("foo", Reg R12); ("$$closure$$", RegOffset (-8, RBP)); ("z", RegOffset (-16, RBP))]
             );
             (6, [("f", Reg R12); ("$$closure$$", RegOffset (-8, RBP)); ("y", RegOffset (-16, RBP))]);
             (14, [("$$closure$$", RegOffset (-8, RBP)); ("x", RegOffset (-16, RBP))]);
             (0, [("bar", Reg R14); ("f", Reg R13); ("foo", Reg R12)]) ] ]
;;

let () =
  run_test_tt_main
    ( "integration_tests"
    >::: [ prim1_suite;
           overflow_suite;
           prim2_suite;
           comparison_suite;
           logic_suite;
           if_suite;
           native_suite;
           equality_suite;
           tuple_suite;
           binding_suite;
           lambda_suite;
           compositionality_suite;
           gc_suite;
           input_file_test_suite () ] )
;;

let () =
  run_test_tt_main
    ( "unit_tests"
    >::: [ well_formed_suite;
           desugar_suite;
           anf_suite;
           fv_suite;
           stack_allocation_suite;
           interfere_suite;
           color_suite;
           reg_alloc_suite ] )
;;

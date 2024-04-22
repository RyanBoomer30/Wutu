open Helpers
open OUnit2

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
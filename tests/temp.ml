open Helpers
open OUnit2

let errors = 
  "tuple_tests"
  >::: [ (* logical and *)
  terrnb "bools_throw_arith_err1" "add1(false)" "" "arithmetic expected a number, got false";
  terrnb "bools_throw_arith_err2" "sub1(true)" "" "arithmetic expected a number, got true";
  terrnb "num_throws_logic_err" "!(1)" "" "logic expected a boolean, got 1";
  terrnb "neg_throws_logic_err" "!(-4)" "" "logic expected a boolean, got -4";
  terrnb "prim2_fail_right" "2 + true" "" "arithmetic expected a number, got true";
  terrnb "prim2_fail_left" "false - 4" "" "arithmetic expected a number, got false";
  terrnb "prim2_fail_blame" "true * false" "" "arithmetic expected a number, got true";
  terrnb "bool_throws_compare1" "true < 2" "" "Error: comparison expected a number, got true";
  terrnb "bool_throws_compare2" "3 <= false" ""
    "Error: comparison expected a number, got false";
  terrnb "bool_throws_blame" "true > false" "" "Error: comparison expected a number, got true";
  terrnb "and_num_throws1" "4 && false" "" "logic expected a boolean, got 4";
  terrnb "and_num_throws2" "true && 6" "" "logic expected a boolean, got 6";
  terrnb "if_throws_non_imm" "if add1(add1(9 + 10)): 1 + false else: false - 69" ""
    "Error: if expected a boolean, got 21";
      terrnb "or_num_throws1" "4 || true" "" "logic expected a boolean, got 4";
  terrnb "or_num_throws2" "false || 6" "" "logic expected a boolean, got 6";
  terrnb "if_num_throws" "if 54: true else: false" "" "Error: if expected a boolean, got 54";
    terrnb "if_throws_non_imm" "if add1(add1(9 + 10)): 1 + false else: false - 69" ""
      "Error: if expected a boolean, got 21"
  ]

let tuple_suite =
  "tuple_tests"
  >::: [ (* logical and *)
         tnb "access_1" "(1,2,3,4)[0]" "" "1";
         tnb "access_2" "(1,2,3,4)[1]" "" "2";
         tnb "access_3" "(1,2,3,4)[2]" "" "3";
         tnb "access_4" "(1,2,3,4)[3]" "" "4";
         tnb "set" "let t = (1,2,3,4) in t[0] := 50; t[0]" "" "50";
         tnb "access_computation" "(1,2,3,4)[2+1]" "" "4";
         terrnb "get_not_tuple" "1[2]" "" "get expected tuple, got 1";
         terrnb "tuple_get_idx_low" "let x = (1, 2) in x[-1]" "" "index too small to get, got -1";
         terrnb "tuple_get_idx_high" "let x = (1, 2) in x[2]" "" "index too large to get, got 2";
         terrnb "tuple_get_idx_bool" "let x = (1, 2) in x[true]" ""
           "expected numeric index, got true";
         terrnb "tuple_set_idx_low" "let x = (1, 2) in x[-69] := 10" ""
           "index too small to set, got -69";
         terrnb "tuple_set_idx_high" "let x = (1, 2) in x[69] := 10" ""
           "index too large to set, got 69";
         terrnb "tuple_get_idx_bool" "let x = (1, 2) in x[true] := 10" ""
           "expected numeric index, got true";
         terrnb "set_not_tuple" "1[2] := 3" "" "set expected tuple, got 1" ]
;;

(*
   t "nil_safe" "let t = (nil,) in t[0]" "" "nil";
   terr "nil_deref" "let t = (nil) in t[0][0]" "" "access component of nil";
   terr "nil_mutate" "let n = nil in n[0] := true" "" "access component of nil"; *)

open Helpers
open OUnit2

let prim1_suite =
  "prim1_tests"
  >::: [ tnb "num" "60" "" "60";
         tnb "false" "false" "" "false";
         tnb "true" "true" "" "true";
         tnb "add1" "add1(11)" "" "12";
         tnb "sub1" "sub1(11)" "" "10";
         terrnb "bools_throw_arith_err1" "add1(false)" "" "arithmetic expected a number, got false";
         terrnb "bools_throw_arith_err2" "sub1(true)" "" "arithmetic expected a number, got true" ]
;;

let prim2_suite =
  "prim2_tests"
  >::: [ tnb "add" "2 + 3" "" "5";
         tnb "add_neg" "-2 + 3" "" "1";
         tnb "add_neg2" "-2 + -3" "" "-5";
         tnb "sub" "10 - 6" "" "4";
         tnb "sub_neg" "6 - 10" "" "-4";
         tnb "sub_neg2" "-6 - 10" "" "-16";
         tnb "mul" "2 * 3" "" "6";
         tnb "mul_neg" "-69 * 420" "" "-28980";
         (* type checking *)
         terrnb "prim2_fail_right" "2 + true" "" "arithmetic expected a number, got true";
         terrnb "prim2_fail_left" "false - 4" "" "arithmetic expected a number, got false";
         terrnb "prim2_fail_blame" "true * false" "" "arithmetic expected a number, got true" ]
;;

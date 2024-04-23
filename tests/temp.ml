open Helpers
open OUnit2

let prim1_suite =
  "prim1_tests"
  >::: [ tnb "num" "60" "" "60";
         tnb "false" "false" "" "false";
         tnb "true" "true" "" "true";
         tnb "add1" "add1(11)" "" "12";
         tnb "sub1" "sub1(11)" "" "10" ]
;;

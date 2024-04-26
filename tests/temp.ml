open Helpers
open OUnit2

let funcs =
  "function"
  >::: [ tnb "simple_func" "let id = (lambda (x): x) in 1" "" "1";
         tnb "curried_add" "(lambda (x): (lambda (y): x + y))(2)(3)" "" "5";
         tnb "function_as_input"
           "let applyToFive = (lambda (f): f(5)) in applyToFive((lambda (x): x + 1))" "" "6";
         tnb "function_as_output"
           "let two = (lambda (f): (lambda (x): f(f(x)))) in two((lambda (x): x + 1))(1)" "" "3";
         tnb "thunk" "(lambda: true)()" "" "true";
         terrnb "call_not_fun" "1(1)" "" "tried to call a non-closure value, got 1";
         terrnb "call_wrong_arity" "(lambda (x): x)(1,2)" "" "arity mismatch in call";
         terrnb "lambda_body_error" "(lambda (x): x + 1)(true)" "" "arithmetic expected a number";
         tnb "function_as_input" "def applyToFive(f): f(5)\napplyToFive((lambda (x): x + 1))" "" "6";
         tnb "bigger" "let foo = (lambda: (lambda (x): 2)) in foo()(1)" "" "2";
         tnb "func_lambda" "def foo(w, x, y, z):\n  (lambda (a): a + x + z)\n\nfoo(1, 2, 3, 4)(5)"
           "" "11";
         tnb "even_odd"
           "def evenNat(n):\n\
           \         if n == 0: true else: oddNat(n - 1)\n\
           \       \n\
           \       and def oddNat(n):\n\
           \         if n == 0: false else: evenNat(n - 1)\n\
           \       \n\
           \       evenNat(21)" "" "false";
         tnb "nil_is_nil" "nil == nil" "" "true" ]
;;

(*
   t "nil_safe" "let t = (nil,) in t[0]" "" "nil";
   terr "nil_deref" "let t = (nil) in t[0][0]" "" "access component of nil";
   terr "nil_mutate" "let n = nil in n[0] := true" "" "access component of nil"; *)

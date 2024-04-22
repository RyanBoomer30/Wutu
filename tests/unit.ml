open Assembly
open Compile
open Errors
open Exprs
open Graph
open Helpers
open OUnit2
open Pretty
open Printf
open Runner

let list_to_graph lst =
  Graph.of_seq (List.to_seq (List.map (fun (key, xs) -> (key, NeighborSet.of_list xs)) lst))
;;

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

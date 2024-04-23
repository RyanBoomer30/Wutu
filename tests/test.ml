open Integration
open OUnit2
open Runner
open Unit

let () = run_test_tt_main Temp.prim1_suite

(* let () =
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
   ;; *)

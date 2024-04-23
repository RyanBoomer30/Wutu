open Exprs
open Compile
open Phases
open Errors

let compile_wasm_prog _ =
  "(module\n\
  \  (func $i (import \"imports\" \"imported_func\") (param i64))\n\
  \  (func (export \"exported_func\")\n\
  \    i64.const 84\n\
  \    call $i\n\
  \  )\n\
   )"
;;

let pick_alloc_strategy (strat : alloc_strategy) =
  match strat with
  | Naive -> naive_stack_allocation
  | Register -> register_allocation
;;

let compile_to_wat_string
    ?(no_builtins = false)
    (alloc_strat : alloc_strategy)
    (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_phase desugared (desugar ~no_builtins)
  |> add_err_phase well_formed is_well_formed
  |> add_phase tagged tag |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase cached free_vars_cache
  |> add_phase locate_bindings (pick_alloc_strategy alloc_strat)
  |> add_phase result compile_wasm_prog
;;

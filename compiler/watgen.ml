open Exprs
open Compile
open Phases
open Errors
open Constants
open Printf
open Wast

(* names of safe arithmetic operations in the JS runtime that can handle overflow.
   we don't have to wrap these up, for the same reason why we don't wrap up normal adds *)
let safe_add = "safe_add"

let safe_sub = "safe_sub"

let safe_mul = "safe_mul"

let error_fun = "error"

let unwrapped_fun_env =
  [ (safe_add, Native safe_add, 2);
    (safe_sub, Native safe_sub, 2);
    (safe_mul, Native safe_mul, 2);
    (error_fun, Native error_fun, 2) ]
;;

(* helpers *)

let deepest_index_stack env = List.fold_left max ~-1 (List.map snd env)

(* we accumulate the wfuns seen so far in order to do lambda lifting
   we need to have them in the proper order, to fill in funtbl idx in closures
   (which we store as funtbl_ptr * 2, to prevent our BFS GC from
    thinking it is a tuple/closure/etc) *)
let rec compile_aexpr
    (e : (tag * StringSet.t) aexpr)
    (env_tag : tag)
    (env : int envt tag_envt)
    (wfuns_so_far : wfunc list)
    (is_tail : bool) : wfunc list * winstr list =
  match e with
  | ALet (id, bind, body, _) ->
      let wfuns_with_bind, bind_instrs = compile_cexpr bind env_tag env wfuns_so_far false in
      let wfuns, body_instrs = compile_aexpr body env_tag env wfuns_with_bind is_tail in
      (wfuns, bind_instrs @ [WLocalSet (find (find env env_tag) id)] @ body_instrs)
  | ASeq (c, a, _) ->
      let wfuns_with_left, left_compiled = compile_cexpr c env_tag env wfuns_so_far false in
      let wfuns, right_compiled = compile_aexpr a env_tag env wfuns_with_left is_tail in
      (wfuns, left_compiled @ [WDrop] @ right_compiled)
  | ACExpr cexpr -> compile_cexpr cexpr env_tag env wfuns_so_far is_tail
  | ALetRec (binds, body, _) -> raise (NotYetImplemented "Unimplemented")

and compile_cexpr
    (e : (tag * StringSet.t) cexpr)
    (env_tag : tag)
    (env : int envt tag_envt)
    (wfuns_so_far : wfunc list)
    (is_tail : bool) : wfunc list * winstr list =
  (* we define Wasm functions to do the untagging (which could actually be faster
     than inlining, bc JS engines be weird), which we call here.
     The actual functions are defined later in compile_prog *)
  let load_num imm ec = [imm; WI64Const ec; WCall "load_num"] in
  let load_bool imm ec = [imm; WI64Const ec; WCall "load_bool"] in
  let load_tuple imm ec = [imm; WI64Const ec; WCall "load_tuple"] in
  let load_closure imm ec = [imm; WI64Const ec; WCall "load_closure"] in
  match e with
  | CIf (cond, thn, els, _) ->
      let wfuns_with_thn, thn_instrs = compile_aexpr thn env_tag env wfuns_so_far is_tail in
      let wfuns, els_instrs = compile_aexpr els env_tag env wfuns_with_thn is_tail in
      let cond_imm = compile_imm cond env_tag env in
      ( wfuns,
        load_bool cond_imm (err_code err_IF_NOT_BOOL)
        @ [WI64HexConst const_true; WEq; WIfThenElse (([], I64), thn_instrs, els_instrs)] )
  | CPrim1 (op, e, _) -> (
      let imm = compile_imm e env_tag env in
      let make_is tag mask =
        [ WI64HexConst mask;
          WAnd;
          WI64HexConst tag;
          WSub I64;
          WI32WrapI64;
          WIfThenElse (([], I64), [WI64HexConst const_false], [WI64HexConst const_true]) ]
      in
      match op with
      (* safe operations from runtime do tag checks! *)
      | Add1 -> (wfuns_so_far, [imm; WI64Const (err_code err_ARITH_NOT_NUM); WCall safe_add])
      | Sub1 -> (wfuns_so_far, [imm; WI64Const (err_code err_ARITH_NOT_NUM); WCall safe_sub])
      | Not -> (wfuns_so_far, load_bool imm (err_code err_LOGIC_NOT_BOOL) @ [WI64HexConst bool_mask; WXor])
      | IsBool -> (wfuns_so_far, imm :: make_is bool_tag bool_tag_mask)
      | IsNum -> (wfuns_so_far, imm :: make_is num_tag num_tag_mask)
      | IsTuple -> (wfuns_so_far, imm :: make_is tuple_tag tuple_tag_mask)
      | PrintStack -> raise (InternalCompilerError "Wasm compilation does not support PrintStack") )
  | CPrim2 (op, left, right, _) -> (
      let imm_left = compile_imm left env_tag env in
      let imm_right = compile_imm right env_tag env in
      (* builds a comparison function, abstracts out ge/gt/le/lt *)
      let cmp_help comp =
        load_num imm_left (err_code err_COMP_NOT_NUM)
        @ load_num imm_right (err_code err_COMP_NOT_NUM)
        @ [comp; WIfThenElse (([], I64), [WI64HexConst const_true], [WI64HexConst const_false])]
      in
      match op with
      | Plus -> (wfuns_so_far, [imm_left; imm_right; WCall safe_add])
      | Minus -> (wfuns_so_far, [imm_left; imm_right; WCall safe_sub])
      | Times -> (wfuns_so_far, [imm_left; imm_right; WCall safe_mul])
      | Greater -> (wfuns_so_far, cmp_help WGt)
      | GreaterEq -> (wfuns_so_far, cmp_help WGe)
      | Less -> (wfuns_so_far, cmp_help WLt)
      | LessEq -> (wfuns_so_far, cmp_help WLe)
      | Eq ->
          ( wfuns_so_far,
            [ imm_left;
              imm_right;
              WEq;
              (* WEq returns an i32 0 or 1, which we need to convert into a snake boolean *)
              WIfThenElse (([], I64), [WI64HexConst const_true], [WI64HexConst const_false]) ] )
      | And -> raise (InternalCompilerError "Error: missed desugar of &&")
      | Or -> raise (InternalCompilerError "Error: missed desugar of ||")
      | CheckSize -> raise (NotYetImplemented "Unimplemented") )
  | CApp (func, args, ct, (tag, _)) -> raise (NotYetImplemented "Unimplemented")
  | CTuple (elems, _) ->
      let arity = List.length elems in
      let size_unpadded = arity + 1 in
      (* store the arity of the tuple as a SNAKEVAL, meaning we have to shift *)
      let write_arity =
        [WGlobalGet "r15"; WI64Const (Int64.shift_left (Int64.of_int arity) 1); WStore 0]
      in
      let load_elems =
        List.flatten
          (List.mapi
             (fun i imm ->
               [WGlobalGet "r15"; compile_imm imm env_tag env; WStore ((i + 1) * Assembly.word_size)]
               )
             elems )
      in
      let maybe_pad_heap =
        if size_unpadded mod 2 = 1 then
          [WGlobalGet "r15"; WI64Const 0L; WStore (size_unpadded * Assembly.word_size)]
        else
          []
      in
      let size_padded = size_unpadded + (size_unpadded mod 2) in
      (* we will place the tagged pointer into the linear memory on the stack, then... *)
      let push_ret_value = [WGlobalGet "r15"; WI64ExtendI32; WI64HexConst tuple_tag; WAdd I64] in
      (* without touching it, update r15 *)
      let update_r15 =
        [WGlobalGet "r15"; WI32Const (size_padded * Assembly.word_size); WAdd I32; WGlobalSet "r15"]
      in
      (wfuns_so_far, write_arity @ load_elems @ maybe_pad_heap @ push_ret_value @ update_r15)
  | CGetItem (tup, idx, _) ->
      let tup_imm = compile_imm tup env_tag env in
      let idx_imm = compile_imm idx env_tag env in
      let load_arity = load_tuple tup_imm (err_code err_GET_NOT_TUPLE) @ [WI32WrapI64; WLoad 0] in
      let check_idx_too_big =
        load_num idx_imm (err_code err_GET_NOT_NUM)
        @ [WLe; WIfThen [idx_imm; WI64Const (err_code err_GET_HIGH_INDEX); WCall error_fun; WDrop]]
      in
      let check_idx_too_small =
        [ idx_imm;
          WI64Const 0L;
          WLt;
          WIfThen [idx_imm; WI64Const (err_code err_GET_LOW_INDEX); WCall error_fun; WDrop] ]
      in
      let access_elem =
        [ tup_imm;
          WI64HexConst tuple_tag;
          WSub I64;
          idx_imm;
          (* we multiply by word_size / 2 because we already
             multiplied by 2 (the index is a SNAKEVAL) *)
          WI64Const (Int64.of_int (Assembly.word_size / 2));
          WMul I64;
          WAdd I64;
          WI32WrapI64;
          (* we skip over the first word, since that stores the tuple *)
          WLoad Assembly.word_size ]
      in
      (wfuns_so_far, load_arity @ check_idx_too_big @ check_idx_too_small @ access_elem)
  | CSetItem (tup, idx, new_val, _) ->
      let tup_imm = compile_imm tup env_tag env in
      let idx_imm = compile_imm idx env_tag env in
      let val_imm = compile_imm new_val env_tag env in
      (* must of this code is basically the same as the case above, with different error codes *)
      let load_arity = load_tuple tup_imm (err_code err_SET_NOT_TUPLE) @ [WI32WrapI64; WLoad 0] in
      let check_idx_too_big =
        load_num idx_imm (err_code err_SET_NOT_NUM)
        @ [WLe; WIfThen [idx_imm; WI64Const (err_code err_SET_HIGH_INDEX); WCall error_fun; WDrop]]
      in
      let check_idx_too_small =
        [ idx_imm;
          WI64Const 0L;
          WLt;
          WIfThen [idx_imm; WI64Const (err_code err_SET_LOW_INDEX); WCall error_fun; WDrop] ]
      in
      let load_elem =
        [ tup_imm;
          WI64HexConst tuple_tag;
          WSub I64;
          idx_imm;
          (* we multiply by word_size / 2 because we already
             multiplied by 2 (the index is a SNAKEVAL) *)
          WI64Const (Int64.of_int (Assembly.word_size / 2));
          WMul I64;
          WAdd I64;
          WI32WrapI64;
          (* we skip over the first word, since that stores the tuple *)
          val_imm;
          WStore Assembly.word_size;
          (* returns the new value as the result *)
          val_imm ]
      in
      (wfuns_so_far, load_arity @ check_idx_too_big @ check_idx_too_small @ load_elem)
  | CImmExpr e ->
      (* no functions reachable from immexprs *)
      (wfuns_so_far, [compile_imm e env_tag env])
  | CLambda (args, body, (lam_tag, fvs)) -> raise (NotYetImplemented "Unimplemented")

and compile_imm e (env_tag : tag) env : winstr =
  match e with
  (* All SNAKEVALs are shifted left at compilation and shifted back in C for intepretation
     We must tag here, and untag later *)
  | ImmNum (n, _) -> WI64Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> WI64HexConst const_true
  | ImmBool (false, _) -> WI64HexConst const_false
  | ImmId (x, _) -> WLocalGet (find (find env env_tag) x)
  | ImmNil _ -> WI64HexConst const_nil
;;

let build_load_fun name mask tag to_untag : wfunc =
  let instrs_no_untag =
    [ WI64HexConst mask;
      WLocalGet 0;
      WAnd;
      WI64HexConst tag;
      WSub I64;
      WI32WrapI64;
      (* we add in the WDrop because error_fun is annotated as returning an i64. it
         doesn't actually return that i64, but we annotate it as such to fit with our AST *)
      WIfThen [WLocalGet 0; WLocalGet 1; WCall error_fun; WDrop] ]
  in
  let instrs_ret =
    if to_untag then
      [WLocalGet 0; WI64HexConst tag; WSub I64]
    else
      [WLocalGet 0]
  in
  (Some name, None, 2, 0, instrs_no_untag @ instrs_ret)
;;

let compile_prog ((anfed : (tag * StringSet.t) aprogram), (env : int envt tag_envt)) : wmodule =
  let imported_funs = (* initial_fun_env @ *) unwrapped_fun_env in
  (* wrap up the imported runtime functions as imports *)
  let fun_imports =
    List.map
      (fun (_, ct, arity) ->
        match ct with
        | Native s -> ("runtime", s, FunctionImport (s, arity))
        | _ -> raise (InternalCompilerError "Only native functions can be local") )
      imported_funs
  in
  let max_import_arity =
    List.fold_left max 0 (List.map (fun (_, _, arity) -> arity) imported_funs)
  in
  let load_num = build_load_fun "load_num" num_tag_mask num_tag false in
  let load_bool = build_load_fun "load_bool" bool_tag_mask bool_tag false in
  let load_tuple = build_load_fun "load_tuple" tuple_tag_mask tuple_tag true in
  let load_closure = build_load_fun "load_closure" closure_tag_mask closure_tag true in
  let load_funs = [load_num; load_bool; load_tuple; load_closure] in
  match anfed with
  | AProgram (body, (tag, _)) ->
      (* start off with wfuns_so_far as empty *)
      let funs, ocsh_instrs = compile_aexpr body tag env [] false in
      (* if the deepest local is 2, this means we have 3 locals in total
         if there are no locals, we will return -1 -> ocsh_fun will have 0 locals *)
      let deepest_local =
        try deepest_index_stack (find env tag) with InternalCompilerError _ -> ~-1
      in
      let ocsh_fun = (None, Some "our_code_starts_here", 0, deepest_local + 1, ocsh_instrs) in
      let all_funs = funs @ [ocsh_fun] @ load_funs in
      let max_arity =
        List.fold_left max max_import_arity (List.map (fun (_, _, arity, _, _) -> arity) all_funs)
      in
      let type_list =
        (* we subtract 1 since build_list starts at 1. here, we want to get 0, 1, 2, ... max_arity *)
        build_list (fun n -> (replicate I64 (n - 1), I64)) (max_arity + 1)
      in
      let imports =
        ("runtime", "table", FuncTableImport (List.length funs))
        :: (* placeholder: start with one page of memory *)
           ("runtime", "memory", MemoryImport 1)
        :: fun_imports
      in
      { imports;
        globals= [("r15", Mut I32, 0)];
        funtypes= type_list;
        (* subtract one, since build_list starts at 1 *)
        elems= (0, build_list (fun x -> x - 1) (List.length funs));
        funcs= all_funs }
;;

let compile_to_wat_string
    ?(no_builtins = false)
    (_ : alloc_strategy) (* unused, but needed for runner signatures to match *)
    (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_phase desugared (desugar ~no_builtins)
  |> add_err_phase well_formed is_well_formed
  |> add_phase tagged tag |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase cached free_vars_cache
  |> add_phase locate_indices stack_slot_allocation
  |> add_phase wmodule compile_prog
  |> add_phase result watstring_of_wmodule
;;

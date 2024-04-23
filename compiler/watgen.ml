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

let deepest_index_stack env = List.fold_left max 0 (List.map snd env)

(* size may be odd; we add the word of padding in here. size is in words
   NOTE: this clobbers RAX and the argument registers, don't store anything in there *)
let reserve size tag = raise (InternalCompilerError "Unimplemented")

let rec compile_aexpr
    (e : (tag * StringSet.t) aexpr)
    (env_tag : tag)
    (env : int envt tag_envt)
    (num_args : int)
    (is_tail : bool) : wfunc list * winstr list =
  match e with
  | ALet (id, bind, body, _) ->
      let bind_funs, bind_instrs = compile_cexpr bind env_tag env num_args false in
      let body_funs, body_instrs = compile_aexpr body env_tag env num_args is_tail in
      (bind_funs @ body_funs, bind_instrs @ [WLocalSet (find (find env env_tag) id)] @ body_instrs)
  | ASeq (c, a, _) -> raise (InternalCompilerError "Unimplemented")
  | ACExpr cexpr -> compile_cexpr cexpr env_tag env num_args is_tail
  | ALetRec (binds, body, _) -> raise (InternalCompilerError "Unimplemented")

and compile_cexpr
    (e : (tag * StringSet.t) cexpr)
    (env_tag : tag)
    (env : int envt tag_envt)
    (num_args : int)
    (is_tail : bool) : wfunc list * winstr list =
  (* we define Wasm functions to do the untagging (which could actually be faster
     than inlining, bc JS engines be weird), which we call here.
     The actual functions are defined later in compile_prog *)
  let load_num ec = [WI64Const ec; WCall "load_num"] in
  let load_bool ec = [WI64Const ec; WCall "load_bool"] in
  let load_tuple ec = [WI64Const ec; WCall "load_tuple"] in
  let load_closure ec = [WI64Const ec; WCall "load_closure"] in
  match e with
  | CIf (cond, thn, els, (tag, _)) -> raise (InternalCompilerError "Unimplemented")
  | CPrim1 (op, e, _) -> (
      let imm = compile_imm e env_tag env in
      match op with
      (* safe operations from runtime do tag checks! *)
      | Add1 -> ([], [imm; WI64Const 2L; WCall safe_add])
      | Sub1 -> ([], [imm; WI64Const 2L; WCall safe_sub])
      | Not -> raise (InternalCompilerError "Unimplemented")
      | IsBool -> raise (InternalCompilerError "Unimplemented")
      | IsNum -> raise (InternalCompilerError "Unimplemented")
      | IsTuple -> raise (InternalCompilerError "Unimplemented")
      | PrintStack -> raise (InternalCompilerError "Wasm compilation does not support PrintStack") )
  | CPrim2 (op, left, right, _) -> (
      let imm_left = compile_imm left env_tag env in
      let imm_right = compile_imm right env_tag env in
      match op with
      | Plus -> ([], [imm_left; imm_right; WCall safe_add])
      | Minus -> ([], [imm_left; imm_right; WCall safe_sub])
      | Times -> ([], [imm_left; imm_right; WCall safe_mul])
      | Greater -> raise (InternalCompilerError "Unimplemented")
      | GreaterEq -> raise (InternalCompilerError "Unimplemented")
      | Less -> raise (InternalCompilerError "Unimplemented")
      | LessEq -> raise (InternalCompilerError "Unimplemented")
      | Eq -> raise (InternalCompilerError "Unimplemented")
      | And -> raise (InternalCompilerError "Error: missed desugar of &&")
      | Or -> raise (InternalCompilerError "Error: missed desugar of ||")
      | CheckSize -> raise (InternalCompilerError "Unimplemented") )
  | CApp (func, args, ct, (tag, _)) -> raise (InternalCompilerError "Unimplemented")
  | CTuple (elems, (tag, _)) -> raise (InternalCompilerError "Unimplemented")
  | CGetItem (tup, idx, _) -> raise (InternalCompilerError "Unimplemented")
  | CSetItem (tup, idx, new_val, _) -> raise (InternalCompilerError "Unimplemented")
  | CImmExpr e ->
      (* no functions reachable from immexprs *)
      ([], [compile_imm e env_tag env])
  | CLambda (args, body, (lam_tag, fvs)) -> raise (InternalCompilerError "Unimplemented")

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
      WIfThen [WLocalGet 0; WLocalGet 1; WCall error_fun; WDrop]
    ]
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
      let funs, ocsh_instrs = compile_aexpr body tag env 1 false in
      let locals = try deepest_index_stack (find env tag) with InternalCompilerError _ -> 0 in
      let ocsh_fun = (None, Some "our_code_starts_here", 0, locals, ocsh_instrs) in
      let all_funs = funs @ [ocsh_fun] @ load_funs in
      let max_arity =
        List.fold_left max max_import_arity (List.map (fun (_, _, arity, _, _) -> arity) all_funs)
      in
      let type_list =
        (* we subtract 1 since build_list starts at 1. here, we want to get 0, 1, 2, ... max_arity *)
        build_list (fun n -> (replicate I64 (n - 1), I64)) (max_arity + 1)
      in
      let imports = ("runtime", "table", FuncTableImport (List.length funs)) :: fun_imports in
      { imports;
        globals= [];
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

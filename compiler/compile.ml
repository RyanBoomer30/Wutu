open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors
open Graph

(* Documentation can be found at https://v2.ocaml.org/api/Set.S.html *)
module StringSet = Set.Make (String)

(* Documentation can be found at https://v2.ocaml.org/api/Map.S.html *)
module StringMap = Map.Make (String)

(* You might also find
     type 'a envt = 'a StringMap.t
   to be useful instead of a simple list of pairs. *)

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (binds, body, _) -> List.for_all (fun (_, e, _) -> is_anf e) binds && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e

and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false
;;

(* SNAKE CONSTANTS *)

let max_snake_val = Int64.div Int64.max_int 2L

let min_snake_val = Int64.div Int64.min_int 2L

let const_true = HexConst 0xFFFFFFFFFFFFFFFFL

let const_false = HexConst 0x7FFFFFFFFFFFFFFFL

let const_nil = HexConst 0x0000000000000001L

(* TAGS AND MASKS *)

let bool_mask = HexConst 0x8000000000000000L

let bool_tag = HexConst 0x000000000000000FL

let bool_tag_mask = HexConst 0x000000000000000FL

let num_tag = HexConst 0x0000000000000000L

let num_tag_mask = HexConst 0x0000000000000001L

let closure_tag = HexConst 0x0000000000000005L

let closure_tag_mask = HexConst 0x0000000000000007L

let tuple_tag = HexConst 0x0000000000000001L

let tuple_tag_mask = HexConst 0x0000000000000007L

(* ERROR CODES AND LABELS *)

type err = int64 * string

let err_label = snd

let err_label_arg t = Label (snd t)

let err_COMP_NOT_NUM = (1L, "err_comp_not_num")

let err_ARITH_NOT_NUM = (2L, "err_arith_not_num")

let err_LOGIC_NOT_BOOL = (3L, "err_logic_not_bool")

let err_IF_NOT_BOOL = (4L, "err_if_not_bool")

let err_OVERFLOW = (5L, "err_overflow")

let err_GET_NOT_TUPLE = (6L, "err_get_not_tuple")

let err_GET_LOW_INDEX = (7L, "err_get_low_index")

let err_GET_HIGH_INDEX = (8L, "err_get_high_index")

let err_GET_NOT_NUM = (9L, "err_get_not_num")

let err_NIL_DEREF = (10L, "err_nil_deref")

let err_OUT_OF_MEMORY = (11L, "err_out_of_memory")

let err_SET_NOT_TUPLE = (12L, "err_set_not_tuple")

let err_SET_LOW_INDEX = (13L, "err_set_low_index")

let err_SET_NOT_NUM = (14L, "err_set_not_num")

let err_SET_HIGH_INDEX = (15L, "err_set_high_index")

let err_CALL_NOT_CLOSURE = (16L, "err_call_not_closure")

let err_CALL_ARITY_ERR = (17L, "err_call_arity_err")

let err_TUPLE_SIZE_MISMATCH = (18L, "err_tuple_size_mismatch")

let err_PATTERN_NOT_TUPLE = (19L, "err_pattern_not_tuple")

let all_errors =
  [ err_COMP_NOT_NUM;
    err_ARITH_NOT_NUM;
    err_LOGIC_NOT_BOOL;
    err_IF_NOT_BOOL;
    err_OVERFLOW;
    err_GET_NOT_TUPLE;
    err_GET_LOW_INDEX;
    err_GET_HIGH_INDEX;
    err_GET_NOT_NUM;
    err_NIL_DEREF;
    err_OUT_OF_MEMORY;
    err_SET_NOT_TUPLE;
    err_SET_LOW_INDEX;
    err_SET_NOT_NUM;
    err_SET_HIGH_INDEX;
    err_CALL_NOT_CLOSURE;
    err_CALL_ARITY_ERR;
    err_TUPLE_SIZE_MISMATCH;
    err_PATTERN_NOT_TUPLE ]
;;

(* ASM CONSTANTS *)

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]

let heap_reg = R15

let scratch_reg = R11

let scratch_reg2 = R10

let unused_registers = [R12; R13; R14; RBX]

let allocatable_registers = unused_registers @ first_six_args_registers

let callee_saved_registers = [RBX; R12; R13; R14; R15]

let caller_saved_registers = R10 :: R11 :: first_six_args_registers

(* You may find some of these helpers useful *)

(* [f(n); f(n-1); ... f(1)] *)
let rec build_list_rev f n =
  if n == 0 then
    []
  else
    f n :: build_list_rev f (n - 1)
;;

(* [f(1); f(2); ... f(n)] *)
let build_list f n = List.rev (build_list_rev f n)

(* if n > the length, takes all of the list *)
let rec take n l =
  if n = 0 then
    []
  else
    match l with
    | [] -> []
    | x :: xs -> x :: take (n - 1) xs
;;

(* if n > then length, returns an empty list *)
let rec drop n l =
  if n = 0 then
    l
  else
    match l with
    | [] -> []
    | _ :: xs -> drop (n - 1) xs
;;

let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" (ExtLib.dump x)))
  | (y, v) :: rest ->
      if y = x then
        v
      else
        find rest x
;;

let count_vars e =
  let rec helpA e =
    match e with
    | ASeq (e1, e2, _) -> max (helpC e1) (helpA e2)
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ALetRec (binds, body, _) ->
        List.length binds
        + List.fold_left max (helpA body) (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ACExpr e -> helpC e
  and helpC e =
    match e with
    | CIf (_, t, f, _) -> max (helpA t) (helpA f)
    | _ -> 0
  in
  helpA e
;;

let rec replicate x i =
  if i = 0 then
    []
  else
    x :: replicate x (i - 1)
;;

let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
  | [] -> None
  | (DFun (fname, _, _, _, _) as d) :: ds_rest ->
      if name = fname then
        Some d
      else
        find_decl ds_rest name
;;

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
  | [] -> false
  | x :: xs -> elt = x || find_one xs elt
;;

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | [] | [_] -> None
  | x :: xs ->
      if find_one xs x then
        Some x
      else
        find_dup xs
;;

let combine_sourcespan s1 s2 =
  match (s1, s2) with
  | SourceLoc (l, _), SourceLoc (_, r) -> SourceLoc (l, r)
  | _ -> raise (InternalCompilerError "cannot combine native sourcespans")
;;

(* stores the concrete syntax name, the call type, and the arity *)
type funenvt = (string * call_type * int) list

let initial_fun_env : funenvt =
  [("print", Native "print", 1); ("input", Native "input", 0); ("equal", Native "equal", 2)]
;;

(* INVARIANT: every EApp with Native call_type is currently Native;
   everything else is still Unknown *)
let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program (decls, body, tag) ->
        Program (List.map (fun group -> List.map (helpD env) group) decls, helpE env body, tag)
  and helpD env decl =
    match decl with
    | DFun (name, args, body, allow_shadow, tag) ->
        let newArgs, env' = helpBS env args in
        DFun (name, newArgs, helpE env' body, allow_shadow, tag)
  and helpB env b =
    match b with
    | BBlank _ -> (b, env)
    | BName (name, allow_shadow, tag) ->
        let name' = sprintf "%s_%d" name tag in
        (BName (name', allow_shadow, tag), (name, name') :: env)
    | BTuple (binds, tag) ->
        let binds', env' = helpBS env binds in
        (BTuple (binds', tag), env')
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b :: bs ->
        let b', env' = helpB env b in
        let bs', env'' = helpBS env' bs in
        (b' :: bs', env'')
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a) :: bindings ->
        let b', env' = helpB env b in
        let e' = helpE env e in
        let bindings', env'' = helpBG env' bindings in
        ((b', e', a) :: bindings', env'')
  and helpE env e =
    match e with
    | ESeq (e1, e2, tag) -> ESeq (helpE env e1, helpE env e2, tag)
    | ETuple (es, tag) -> ETuple (List.map (helpE env) es, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE env e, helpE env idx, tag)
    | ESetItem (e, idx, newval, tag) -> ESetItem (helpE env e, helpE env idx, helpE env newval, tag)
    | EPrim1 (op, arg, tag) -> EPrim1 (op, helpE env arg, tag)
    | EPrim2 (op, left, right, tag) -> EPrim2 (op, helpE env left, helpE env right, tag)
    | EIf (c, t, f, tag) -> EIf (helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EId (name, tag) -> ( try EId (find env name, tag) with InternalCompilerError _ -> e )
    | EApp (func, args, native, tag) ->
        let func = helpE env func in
        (* As per invariant above, if we are native, we keep it;
           otherwise, we turn into Snake call (only other actual option) *)
        let call_type =
          match native with
          | Native _ -> native
          | _ -> Snake
        in
        EApp (func, List.map (helpE env) args, call_type, tag)
    | ELet (binds, body, tag) ->
        let binds', env' = helpBG env binds in
        let body' = helpE env' body in
        ELet (binds', body', tag)
    | ELetRec (bindings, body, tag) ->
        let revbinds, env =
          List.fold_left
            (fun (revbinds, env) (b, e, t) ->
              let b, env = helpB env b in
              ((b, e, t) :: revbinds, env) )
            ([], env) bindings
        in
        let bindings' =
          List.fold_left (fun bindings (b, e, tag) -> (b, helpE env e, tag) :: bindings) [] revbinds
        in
        let body' = helpE env body in
        ELetRec (bindings', body', tag)
    | ELambda (binds, body, tag) ->
        let binds', env' = helpBS env binds in
        let body' = helpE env' body in
        ELambda (binds', body', tag)
  in
  rename [] p
;;

(* returns the stack slot of the deepest point in the stack.
   This DOES take args and closure into account, so we would
   want to subtract before using this to reserve stack space *)
let deepest_stack (env : arg envt) : int =
  let get_slot (_, arg) =
    match arg with
    | RegOffset (bytes, RBP) -> bytes / (-1 * word_size)
    | _ -> 0 (* register, not a stack slot *)
  in
  List.fold_left max 0 (List.map get_slot env)
;;

(* IMPLEMENT EVERYTHING BELOW *)

(* SYNTACTIC INVARIANTS:
   After desugaring, there will be no more BTuples, ESeqs, and Prim2 -> And / Or,
   since they all get desugared away. Also, in the argument list of a CLambda,
   there will be no BBlank either.

   All ADecls will be desugared into (potentially nested) `let rec`s.
   All native functions will be wrapped and added as an outermost `let rec`,
   with the call_type (and sourcespan) of the relevant native CApps being Native.
   All other EApps will still be Unknown *)
let desugar ?(no_builtins = false) (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    fun name ->
      next := !next + 1;
      (* extra $ added to ensure no name conflicts *)
      sprintf "%s_$%d" name !next
  in
  let rec desugar_bindings bindings =
    match bindings with
    | [] -> []
    | (binding, bound, loc) :: rest -> (
      match binding with
      | BBlank _ | BName _ -> (binding, helpE bound, loc) :: desugar_bindings rest
      | BTuple (binds, l) ->
          let temp = gensym "bind_temp" in
          let new_binds =
            List.mapi
              (* associates the new binding with the inner bind's location,
                 while all new code gets associated with the whole BTuple's location *)
                (fun i b ->
                (b, EGetItem (EId (temp, l), ENumber (Int64.of_int i, l), l), get_tag_B b) )
              binds
          in
          let check_size =
            ( BBlank loc,
              EPrim2
                (CheckSize, EId (temp, loc), ENumber (Int64.of_int (List.length binds), loc), loc),
              loc )
          in
          (* note that we give the BName the *inner* location! *)
          (BName (temp, false, l), helpE bound, loc)
          :: check_size
          :: desugar_bindings (new_binds @ rest) )
  and helpE (e : sourcespan expr) =
    match e with
    | EBool _ | ENumber _ | EId _ | ENil _ -> e
    | EPrim1 (op, e, loc) -> EPrim1 (op, helpE e, loc)
    (* we propogate the old tag through when desugaring, for error reporting;
       although this case should not ever introduce new errors *)
    | EPrim2 (op, l, r, loc) -> (
      match op with
      (* a && b is equivalent to `if a: b else: false` BUT this won't check the types
          of `a` and `b` / give the right error messages. therefore, we desugar `a && b`
          into `if !(a): false else: !(!(b)), swapping the order of the branches
          (so !a -> correct error message) and using !(!(b)) for b type check *)
      | And ->
          (* new stuff we desugar to get the loc of the full Prim2 *)
          EIf
            ( EPrim1 (Not, helpE l, loc),
              EBool (false, loc),
              EPrim1 (Not, EPrim1 (Not, helpE r, loc), loc),
              loc )
      (* a || b desugars to 'if !(a): !(!(b)) else: true' following the same
          logic as And, adding !(a), swapping branches, and !(!(b)) *)
      | Or ->
          EIf
            ( EPrim1 (Not, helpE l, loc),
              EPrim1 (Not, EPrim1 (Not, helpE r, loc), loc),
              EBool (true, loc),
              loc )
      | _ -> EPrim2 (op, helpE l, helpE r, loc) )
    | EIf (c, t, f, loc) -> EIf (helpE c, helpE t, helpE f, loc)
    | EApp (func, args, ct, loc) -> EApp (helpE func, List.map helpE args, ct, loc)
    | ETuple (es, loc) -> ETuple (List.map helpE es, loc)
    | EGetItem (e, idx, loc) -> EGetItem (helpE e, helpE idx, loc)
    | ESetItem (e, idx, newval, loc) -> ESetItem (helpE e, helpE idx, helpE newval, loc)
    | ESeq (e1, e2, loc) -> ELet ([(BBlank loc, helpE e1, loc)], helpE e2, loc)
    | ELet (bindings, body, loc) ->
        let new_bindings = desugar_bindings bindings in
        if new_bindings = [] then
          helpE body
        else
          ELet (desugar_bindings bindings, helpE body, loc)
    | ELambda (args, body, loc) ->
        (* IDEA: for the args, keep BNames the same, rename BBlanks into BNames
           as an invariant, and rename BTuples into BNames -> let bind in the body *)
        (* takes in a bind and returns a possibly renamed bind, along with
           any context needed at the start of the body (for BTuples) *)
        let help_bind (b : 'a bind) : 'a bind * 'a binding list =
          match b with
          (* we don't need a binding here, since the argument was already evaluated when
              the function was called (as per call by value); we keep the name for arity *)
          | BBlank loc -> (BName (gensym "arg_temp", false, loc), [])
          | BName _ -> (b, [])
          | BTuple (_, loc) ->
              let new_name = gensym "arg_temp" in
              (* just push the BTuple inwards, for later desugaring *)
              (BName (new_name, false, loc), [(b, EId (new_name, loc), loc)])
        in
        let pairlist = List.map help_bind args in
        let new_args, lo_binds = List.split pairlist in
        ELambda (new_args, helpE (ELet (List.flatten lo_binds, body, loc)), loc)
    | ELetRec (bindings, body, loc) ->
        (* invariant from parsing and desugaring: all binds will just be BNames,
           meaning we don't have to deal with desugaring those binds more *)
        ELetRec
          (List.map (fun (bind, expr, loc) -> (bind, helpE expr, loc)) bindings, helpE body, loc)
  in
  let helpD_fold (d : 'a decl list) (acc : sourcespan expr) =
    let helpD (d : 'a decl) : 'a binding =
      match d with
      | DFun (name, binds, body, allow_shadow, loc) ->
          (BName (name, allow_shadow, loc), ELambda (binds, body, loc), loc)
    in
    let bindings = List.map helpD d in
    (* to accumulate outer locations, take the earliest loc from the new bindings
       and pair it with the last loc from the block so far (nested letrecs) *)
    let start_loc =
      match bindings with
      | [] -> get_tag_E acc
      | (_, _, loc) :: _ -> loc
    in
    ELetRec (bindings, acc, combine_sourcespan start_loc (get_tag_E acc))
  in
  let wrap (name, call_type, arity) : 'a binding =
    (* for good error reporting, we use a Native sourcespan *)
    let temp_names = build_list (fun _ -> gensym name) arity in
    let loc = NativeLoc name in
    ( BName (name, false, loc),
      ELambda
        ( List.map (fun id -> BName (id, false, loc)) temp_names,
          EApp (EId (name, loc), List.map (fun id -> EId (id, loc)) temp_names, call_type, loc),
          loc ),
      loc )
  in
  match p with
  | Program (decls, body, loc) ->
      let letreced = List.fold_right helpD_fold decls body in
      let wrapped =
        if no_builtins then
          letreced
        else
          ELetRec (List.map wrap initial_fun_env, letreced, get_tag_E letreced)
      in
      Program ([], helpE wrapped, loc)
;;

(* desugaring has already occurred, so we can use the syntactic invariants to
   simplify the well-formedness check *)
let is_well_formed (p : sourcespan program) : sourcespan program fallible =
  (* NOTE: in all functions, errs is built up in reverse order, since consing onto
     the front is faster than appending. At the very end, we reverse *)

  (* the env here is the environment of *all seen names* thusfar,
     that way we can check if we shadow without using the keyword *)
  let fold_dupe_bind env (bind_env, errs) bind =
    match bind with
    | BBlank _ -> (bind_env, errs)
    | BName (id, allow_shadow, used_loc) ->
        let shadow_errs =
          if allow_shadow then
            errs
          else
            match List.assoc_opt id env with
            | Some defined_loc -> ShadowId (id, used_loc, defined_loc) :: errs
            | None -> errs
        in
        let new_errs =
          match List.assoc_opt id bind_env with
          | Some defined_loc -> DuplicateId (id, used_loc, defined_loc) :: shadow_errs
          | None -> shadow_errs
        in
        ((id, used_loc) :: bind_env, new_errs)
    | BTuple _ -> raise (InternalCompilerError "tuple bindings should have been desugared")
  in
  (* detects duplicate bind names in a list of binds or bindings,
     updating a list of errors and returning all of the names bound,
     where the most recently bound name comes first *)
  let detect_dupe_binds
      (bs : sourcespan bind list)
      (env : (string * sourcespan) list)
      (errs : exn list) : (string * sourcespan) list * exn list =
    List.fold_left (fold_dupe_bind env) ([], errs) bs
  in
  let detect_dupe_bindings
      (bs : sourcespan binding list)
      (env : (string * sourcespan) list)
      (errs : exn list) : (string * sourcespan) list * exn list =
    detect_dupe_binds (List.map (fun (b, _, _) -> b) bs) env errs
  in
  let rec wf_E (e : sourcespan expr) (env : (string * sourcespan) list) (errs : exn list) : exn list
      =
    match e with
    | EBool _ | ENil _ -> errs
    | ENumber (n, loc) ->
        if n > max_snake_val || n < min_snake_val then
          Overflow (n, loc) :: errs
        else
          errs
    | EId (x, loc) -> (
      (* if x is found, then it was bound; if not, then it was free *)
      match List.assoc_opt x env with
      | Some _ -> errs
      | None -> UnboundId (x, loc) :: errs )
    | EPrim1 (_, e, _) -> wf_E e env errs
    | EPrim2 (_, l, r, _) ->
        let left_errs = wf_E l env errs in
        wf_E r env left_errs
    | EIf (c, t, f, _) ->
        let c_errs = wf_E c env errs in
        let ct_errs = wf_E t env c_errs in
        wf_E f env ct_errs
    | EApp (func, args, _, _) ->
        let func_errs = wf_E func env errs in
        List.fold_left (fun errors e -> wf_E e env errors) func_errs args
    | ELet (bindings, body, _) ->
        let _, dupe_errs = detect_dupe_bindings bindings env errs in
        let new_env, bound_errs =
          List.fold_left
            (fun (env_so_far, errors) (bind, bound, loc) ->
              let next_errs = wf_E bound env_so_far errors in
              match bind with
              | BBlank _ -> (env_so_far, next_errs)
              | BName (id, _, loc) -> ((id, loc) :: env_so_far, next_errs)
              | BTuple _ -> raise (InternalCompilerError "tuple bindings should have been desugared")
              )
            (env, dupe_errs) bindings
        in
        wf_E body new_env bound_errs
    | EGetItem (e1, e2, _) ->
        let e1_errs = wf_E e1 env errs in
        wf_E e2 env e1_errs
    | ESetItem (e, idx, newval, _) ->
        let e_errs = wf_E e env errs in
        let idx_errs = wf_E idx env e_errs in
        wf_E newval env idx_errs
    | ESeq (e1, e2, _) ->
        let e1_errs = wf_E e1 env errs in
        wf_E e2 env e1_errs
    | ETuple (elems, _) -> List.fold_left (fun errors e -> wf_E e env errors) errs elems
    | ELetRec (bindings, body, _) ->
        let bound_names, dupe_errs = detect_dupe_bindings bindings env errs in
        let new_env = bound_names @ env in
        let new_errs =
          List.fold_left
            (fun err_so_far (_, bound, loc) ->
              let letrec_errs =
                match bound with
                | ELambda _ -> err_so_far
                | _ -> LetRecNonFunction (bound, loc) :: err_so_far
              in
              wf_E bound new_env letrec_errs )
            dupe_errs bindings
        in
        wf_E body new_env new_errs
    | ELambda (args, body, _) ->
        let bound_names, dupe_errs = detect_dupe_binds args env errs in
        wf_E body (bound_names @ env) dupe_errs
  in
  match p with
  | Program ([], body, _) -> (
      let all_errs = wf_E body [] [] in
      match all_errs with
      | [] -> Ok p
      | _ -> Error (List.rev all_errs) )
  | _ -> raise (InternalCompilerError "decls should have been desugared away")
;;

(* This data type lets us keep track of how a binding was introduced.
   We'll use it to discard unnecessary Seq bindings, and to distinguish
   letrec from let. Essentially, it accumulates just enough information
   in our binding list to tell us how to reconstruct an appropriate aexpr. *)
type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  let name_of_bind err bind =
    match bind with
    | BName (name, _, _) -> name
    | _ -> raise (InternalCompilerError err)
  in
  let rec helpB (bind, bound, _) =
    let name = name_of_bind "ELetRec should only bind BNames" bind in
    let bound_ans, bound_setup = helpC bound in
    (* We already know that a let rec will only contains lambda in its bound,
       as enforced by our well formedness check *)
    match bound_setup with
    | [] -> (name, bound_ans)
    | _ -> raise (InternalCompilerError "ELetRec should only bind lambdas")
  and helpP (p : tag program) : unit aprogram =
    match p with
    | Program ([], body, _) -> AProgram (helpA body, ())
    | _ -> raise (InternalCompilerError "Top-level declarations should have been desugared away")
  and helpC (e : tag expr) : unit cexpr * unit anf_bind list =
    match e with
    | EPrim1 (op, arg, _) ->
        let arg_imm, arg_setup = helpI arg in
        (CPrim1 (op, arg_imm, ()), arg_setup)
    | EPrim2 (op, left, right, _) ->
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf (cond, _then, _else, _) ->
        let cond_imm, cond_setup = helpI cond in
        (CIf (cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet ([], body, _) -> helpC body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [BSeq exp_ans] @ body_setup)
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELet ((BTuple _, _, _) :: _, _, _) ->
        raise (InternalCompilerError "Tuple bindings should have been desugared away")
    | ESeq (e1, e2, _) ->
        let e1_ans, e1_setup = helpC e1 in
        let e2_ans, e2_setup = helpC e2 in
        (e2_ans, e1_setup @ [BSeq e1_ans] @ e2_setup)
    | EApp (func, args, ct, _) ->
        let func_ans, func_setup = helpI func in
        let new_args, new_setup = List.split (List.map helpI args) in
        (* function setup comes first -> function is evaluated before its args *)
        (CApp (func_ans, new_args, ct, ()), func_setup @ List.concat new_setup)
    | ETuple (args, _) ->
        let new_args, new_setup = List.split (List.map helpI args) in
        (CTuple (new_args, ()), List.concat new_setup)
    | EGetItem (tup, idx, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        (CGetItem (tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem (tup, idx, newval, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let nv_imm, nv_setup = helpI newval in
        (CSetItem (tup_imm, idx_imm, nv_imm, ()), tup_setup @ idx_setup @ nv_setup)
    | ELambda (binds, body, _) ->
        let bind_names = List.map (name_of_bind "ELambda should only bind BNames") binds in
        (CLambda (bind_names, helpA body, ()), [])
    | ELetRec (binds, body, _) ->
        let body_ans, body_setup = helpC body in
        (body_ans, BLetRec (List.map helpB binds) :: body_setup)
    | _ ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
  and helpI (e : tag expr) : unit immexpr * unit anf_bind list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
    | ENil _ -> (ImmNil (), [])
    | ESeq (e1, e2, _) ->
        let e1_imm, e1_setup = helpI e1 in
        let e2_imm, e2_setup = helpI e2 in
        (e2_imm, e1_setup @ e2_setup)
    | ETuple (args, tag) ->
        let tmp = sprintf "tuple_%d" tag in
        let new_args, new_setup = List.split (List.map helpI args) in
        (ImmId (tmp, ()), List.concat new_setup @ [BLet (tmp, CTuple (new_args, ()))])
    | EGetItem (tup, idx, tag) ->
        let tmp = sprintf "get_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        (ImmId (tmp, ()), tup_setup @ idx_setup @ [BLet (tmp, CGetItem (tup_imm, idx_imm, ()))])
    | ESetItem (tup, idx, newval, tag) ->
        let tmp = sprintf "set_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let nv_imm, nv_setup = helpI newval in
        ( ImmId (tmp, ()),
          tup_setup @ idx_setup @ nv_setup @ [BLet (tmp, CSetItem (tup_imm, idx_imm, nv_imm, ()))]
        )
    | EPrim1 (op, arg, tag) ->
        let tmp = sprintf "unary_%d" tag in
        let arg_imm, arg_setup = helpI arg in
        (ImmId (tmp, ()), arg_setup @ [BLet (tmp, CPrim1 (op, arg_imm, ()))])
    | EPrim2 (op, left, right, tag) ->
        let tmp = sprintf "binop_%d" tag in
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        ( ImmId (tmp, ()),
          left_setup @ right_setup @ [BLet (tmp, CPrim2 (op, left_imm, right_imm, ()))] )
    | EIf (cond, _then, _else, tag) ->
        let tmp = sprintf "if_%d" tag in
        let cond_imm, cond_setup = helpI cond in
        (ImmId (tmp, ()), cond_setup @ [BLet (tmp, CIf (cond_imm, helpA _then, helpA _else, ()))])
    | EApp (func, args, ct, tag) ->
        let tmp = sprintf "app_%d" tag in
        let func_ans, func_setup = helpI func in
        let new_args, new_setup = List.split (List.map helpI args) in
        ( ImmId (tmp, ()),
          func_setup @ List.concat new_setup @ [BLet (tmp, CApp (func_ans, new_args, ct, ()))] )
    | ELet ([], body, _) -> helpI body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpI exp in
        (* MUST BE helpI, to avoid any missing final steps *)
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ body_setup)
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [BLet (bind, exp_ans)] @ body_setup)
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
        raise (InternalCompilerError "Tuple bindings should have been desugared away")
    | ELambda (binds, body, tag) ->
        let tmp = sprintf "lambda_%d" tag in
        let bind_names = List.map (name_of_bind "ELambda should only bind BNames") binds in
        (ImmId (tmp, ()), [BLet (tmp, CLambda (bind_names, helpA body, ()))])
    | ELetRec (binds, body, tag) ->
        (* Maybe helpI body would be sufficient here *)
        let tmp = sprintf "letrec_%d" tag in
        let body_ans, body_setup = helpC body in
        (ImmId (tmp, ()), (BLetRec (List.map helpB binds) :: body_setup) @ [BLet (tmp, body_ans)])
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right
      (fun bind body ->
        (* Here's where the anf_bind datatype becomes most useful:
             BSeq binds get dropped, and turned into ASeq aexprs.
             BLet binds get wrapped back into ALet aexprs.
             BLetRec binds get wrapped back into ALetRec aexprs.
           Syntactically it looks like we're just replacing Bwhatever with Awhatever,
           but that's exactly the information needed to know which aexpr to build. *)
        match bind with
        | BSeq exp -> ASeq (exp, body, ())
        | BLet (name, exp) -> ALet (name, exp, body, ())
        | BLetRec names -> ALetRec (names, body, ()) )
      ans_setup (ACExpr ans)
  in
  helpP p
;;

let get_fv_A a = snd (get_tag_A a)

let get_fv_C a = snd (get_tag_C a)

let get_fv_I a = snd (get_tag_imm a)

(* Takes a tagged aprog and walks the AST, computing the free variables at every expression,
   and produces a new aprog that stores those free variable sets (along with the old tags,
   for tag_env purposes) *)
let free_vars_cache (prog : 'a aprogram) : ('a * StringSet.t) aprogram =
  let rec helpA (a : 'a aexpr) : ('a * StringSet.t) aexpr =
    match a with
    | ACExpr c -> ACExpr (helpC c)
    | ASeq (c, a, t) ->
        let c_fv = helpC c in
        let a_fv = helpA a in
        let new_fv = StringSet.union (get_fv_C c_fv) (get_fv_A a_fv) in
        ASeq (c_fv, a_fv, (t, new_fv))
    | ALet (name, bind, body, t) ->
        let bind_fv = helpC bind in
        let body_fv = helpA body in
        let new_fvs =
          StringSet.union (get_fv_C bind_fv) (StringSet.remove name (get_fv_A body_fv))
        in
        ALet (name, bind_fv, body_fv, (t, new_fvs))
    | ALetRec (binds, body, t) ->
        let bound_names = List.map (fun (name, _) -> name) binds in
        let binds_fv = List.map (fun (s, c) -> (s, helpC c)) binds in
        (* collects all free variables associated with all binds *)
        let binds_fvs =
          List.fold_left
            (fun set (_, c) -> StringSet.union set (get_fv_C c))
            StringSet.empty binds_fv
        in
        let body_fv = helpA body in
        let new_fvs =
          StringSet.diff
            (StringSet.union binds_fvs (get_fv_A body_fv))
            (StringSet.of_list bound_names)
        in
        ALetRec (binds_fv, body_fv, (t, new_fvs))
  and helpC (c : 'a cexpr) : ('a * StringSet.t) cexpr =
    match c with
    | CIf (cnd, thn, els, t) ->
        let cnd_fv = helpI cnd in
        let thn_fv = helpA thn in
        let els_fv = helpA els in
        let new_fvs =
          StringSet.union (get_fv_I cnd_fv) (StringSet.union (get_fv_A thn_fv) (get_fv_A els_fv))
        in
        CIf (cnd_fv, thn_fv, els_fv, (t, new_fvs))
    | CPrim1 (prim, i, t) ->
        let i_fv = helpI i in
        let new_fvs = get_fv_I i_fv in
        CPrim1 (prim, i_fv, (t, new_fvs))
    | CPrim2 (prim, i1, i2, t) ->
        let i1_fv = helpI i1 in
        let i2_fv = helpI i2 in
        let new_fvs = StringSet.union (get_fv_I i1_fv) (get_fv_I i2_fv) in
        CPrim2 (prim, i1_fv, i2_fv, (t, new_fvs))
    | CApp (id, args, ct, t) -> (
        let id_fv = helpI id in
        let args_fv = List.map helpI args in
        (* collect all of the free vars from all of the args into one set *)
        let args_fvs =
          List.fold_left (fun acc i -> StringSet.union acc (get_fv_I i)) StringSet.empty args_fv
        in
        match ct with
        (* if we have a native call, then as an invariant, the id will be
           some name that is "bound" *by our C runtime*, so we don't want
           to say that it is free *)
        | Native _ -> CApp (id_fv, args_fv, ct, (t, args_fvs))
        | _ -> CApp (id_fv, args_fv, ct, (t, StringSet.union args_fvs (get_fv_I id_fv))) )
    | CTuple (vals, t) ->
        let vals_fv = List.map helpI vals in
        let new_fvs =
          List.fold_left (fun acc i -> StringSet.union acc (get_fv_I i)) StringSet.empty vals_fv
        in
        CTuple (vals_fv, (t, new_fvs))
    | CGetItem (tup, idx, t) ->
        let tup_fv = helpI tup in
        let idx_fv = helpI idx in
        let new_fvs = StringSet.union (get_fv_I tup_fv) (get_fv_I idx_fv) in
        CGetItem (tup_fv, idx_fv, (t, new_fvs))
    | CSetItem (tup, idx, v, t) ->
        let tup_fv = helpI tup in
        let idx_fv = helpI idx in
        let val_fv = helpI v in
        let new_fvs =
          StringSet.union (get_fv_I tup_fv) (StringSet.union (get_fv_I idx_fv) (get_fv_I val_fv))
        in
        CSetItem (tup_fv, idx_fv, val_fv, (t, new_fvs))
    | CLambda (args, body, t) ->
        let body_fv = helpA body in
        let new_fv = StringSet.diff (get_fv_A body_fv) (StringSet.of_list args) in
        CLambda (args, body_fv, (t, new_fv))
    | CImmExpr i -> CImmExpr (helpI i)
  and helpI (i : 'a immexpr) : ('a * StringSet.t) immexpr =
    match i with
    | ImmId (id, t) -> ImmId (id, (t, StringSet.singleton id))
    | ImmNil t -> ImmNil (t, StringSet.empty)
    | ImmNum (n, t) -> ImmNum (n, (t, StringSet.empty))
    | ImmBool (b, t) -> ImmBool (b, (t, StringSet.empty))
  in
  match prog with
  | AProgram (body, tag) ->
      let a_fv = helpA body in
      AProgram (a_fv, (tag, get_fv_A a_fv))
;;

(* we return a map from tags to (maps from names to where they can be found on the stack)
   we use nested environments, since a name can now be found in multiple different places.
   we use a tag environment to avoid having to change ANF to give names to all functions *)
let naive_stack_allocation (prog : (tag * StringSet.t) aprogram) :
    (tag * StringSet.t) aprogram * arg envt tag_envt =
  (* adds [name |-> arg] to the env at tag, creating a new outer env for tag if necessary *)
  let rec add_to_env (tag : tag) (name : string) (arg : arg) (env : arg envt tag_envt) :
      arg envt tag_envt =
    match env with
    | [] -> [(tag, [(name, arg)])]
    | (t, sub_env) :: rest ->
        if t = tag then
          (t, (name, arg) :: sub_env) :: rest
        else
          (t, sub_env) :: add_to_env tag name arg rest
  in
  (* offset stores the next _available_ stack slot (ex: offset 1 initially -> RBP - 8, as intended)
     env_tag stores which sub-map in env to add to (only updated when we enter a lambda) *)
  let rec help_aexpr
      (exp : (tag * StringSet.t) aexpr)
      (offset : int)
      (env : arg envt tag_envt)
      (env_tag : tag) : arg envt tag_envt =
    match exp with
    | ALet (id, bound, body, _) ->
        (* get the environment for the bound variable, which may use the current
           stack slot since we have not stored id in it yet *)
        let env_after_bound = help_cexpr bound offset env env_tag in
        let env_with_id =
          add_to_env env_tag id (RegOffset (~-word_size * offset, RBP)) env_after_bound
        in
        help_aexpr body (offset + 1) env_with_id env_tag
    | ASeq (c, a, _) ->
        let env_after_c = help_cexpr c offset env env_tag in
        (* no need to change offset since we put nothing additional on the stack
           (the point of sequencing is that we evaluate but never bind it) *)
        help_aexpr a offset env_after_c env_tag
    | ALetRec (binds, body, _) ->
        (* first, reserve stack space for everything being bound *)
        let env_with_binds, next_usable_offset =
          List.fold_left
            (fun (env, off) (name, _) ->
              (add_to_env env_tag name (RegOffset (~-word_size * off, RBP)) env, off + 1) )
            (env, offset) binds
        in
        (* then process each thing being bound. the bound names are in scope, so we start with
           next_usable_offset, although we will always enter a lambda, so this shouldn't matter *)
        let env_with_bound =
          List.fold_left
            (fun env (_, bound) -> help_cexpr bound next_usable_offset env env_tag)
            env_with_binds binds
        in
        (* and finally process the body, with the next offset since all bound names are in scope *)
        help_aexpr body next_usable_offset env_with_bound env_tag
    | ACExpr cexp -> help_cexpr cexp offset env env_tag
  and help_cexpr
      (cexp : (tag * StringSet.t) cexpr)
      (offset : int)
      (env : arg envt tag_envt)
      (env_tag : tag) : arg envt tag_envt =
    match cexp with
    | CIf (_, t, e, _) ->
        let env_then = help_aexpr t offset env env_tag in
        help_aexpr e offset env_then env_tag
    | CLambda (args, body, (tag, fvs)) ->
        (* we will add to a *new* section of our environment, corresponding
           to where things can be found when we *call* this lambda *)
        (* by our calling convention, the first stack slot will be _saved_ for
           the lambda itself, followed by all arguments, followed by the
           unpacked closure, followed by all locals (handled by recursive call) *)
        let env_with_args, next_arg_offset =
          List.fold_left
            (fun (env, off) name ->
              (* first arg will start at RSP - 16 because RSP - 8 is reserved for the function itself *)
              (add_to_env tag name (RegOffset (~-word_size * off, RBP)) env, off + 1) )
            (env, 2) args
        in
        let env_with_closure, next_offset =
          List.fold_left
            (fun (env, off) free_name ->
              (add_to_env tag free_name (RegOffset (~-word_size * off, RBP)) env, off + 1) )
            (env_with_args, next_arg_offset)
            (* we will end up unpacking everything that was free
               (note that we have to wrap cexp in ACExpr for the signature of free_vars) *)
            (StringSet.elements fvs)
        in
        (* now we process the body starting from next_offset, using this lambda's new tag *)
        help_aexpr body next_offset env_with_closure tag
    (* NOTE: add more cases if we have more aexprs as cexpr subexpressions,
       or if more cexpr binding constructs are added *)
    | _ -> env
  in
  match prog with
  | AProgram (body, (tag, _)) ->
      (* our body has zero arguments currently, so we start offset at 1
         we also start at the outermost tag as the "general environment"
         not specific to any function *)
      let body_env = help_aexpr body 1 [] tag in
      (prog, body_env)
;;

(* computes the interference graph of a given expression, given the everything
   that must be live at the end (usually the empty set)
   for lambda bodies, call on the body. BUT, we will handle coloring of
   arguments separately (always found in the same spot on the stack),
   so we can just subtract it out from the resulting graph, then add it back in *)
let interfere (e : (tag * StringSet.t) aexpr) (live_out : StringSet.t) : grapht =
  (* computes the live_in of the expr, given the relevant live_out,
     and adds every relevant conflict edge (pairs in calculated live_in, plus some extras)
     to the graph_so_far *)
  let rec helpA (a : (tag * StringSet.t) aexpr) (live_out : StringSet.t) (graph_so_far : grapht) :
      StringSet.t * grapht =
    match a with
    | ASeq (cx, ax, (_, fvs)) ->
        let live_in_ax, graph_ax = helpA ax live_out graph_so_far in
        let live_in_cx, graph_cx = helpC cx live_in_ax graph_ax in
        let live_in = StringSet.union fvs live_in_cx in
        let new_graph = add_clique graph_cx live_in in
        (live_in, new_graph)
    | ALet (id, e, b, (_, fvs)) ->
        let live_in_b, graph_b = helpA b live_out graph_so_far in
        (* the id we bind must have interference edges with everything that
           is live going into the body. this is extra from the live-in conflict edges *)
        let graph_with_id = add_node graph_b id in
        let id_interf_graph =
          Seq.fold_left
            (fun graph_so_far name ->
              if name = id then
                graph_so_far
              else
                add_edge graph_so_far id name )
            graph_with_id (StringSet.to_seq live_in_b)
        in
        (* also, even if the id is used in the body, we don't want it
           to appear in the live_out of the bound expression yet *)
        let live_out_e = StringSet.remove id live_in_b in
        let live_in_e, graph_e = helpC e live_out_e id_interf_graph in
        (* we have already subtracted out the defined id... *)
        let live_in = StringSet.union fvs live_in_e in
        (* ... and we have already added it into the graph above. note that
           we add nodes to the graph ONLY when we bind something, not in graph helpers *)
        let new_graph = add_clique graph_e live_in in
        (live_in, new_graph)
    | ALetRec (binds, b, (_, fvs)) ->
        let live_in_b, graph_b = helpA b live_out graph_so_far in
        (* we know everything on the RHS of a bind is a lambda,
           so we collect FVs of lambdas and add to live_in (since we
           need them to load up closures) *)
        let names = List.map fst binds in
        let bound_set = StringSet.of_list (List.map fst binds) in
        let live_in = StringSet.union fvs (StringSet.diff live_in_b bound_set) in
        (* we have to add all bound names (see comment above) to add edges between them *)
        let graph_with_nodes = List.fold_left (fun gsf name -> add_node gsf name) graph_b names in
        (* overapproximation: we add interference edges with ALL bound names *)
        let new_graph = add_clique graph_with_nodes (StringSet.union live_in bound_set) in
        (live_in, new_graph)
    | ACExpr c -> helpC c live_out graph_so_far
  and helpC (c : (tag * StringSet.t) cexpr) (live_out : StringSet.t) (graph_so_far : grapht) :
      StringSet.t * grapht =
    match c with
    | CIf (_, thn, els, (_, fvs)) ->
        let live_in_els, graph_els = helpA els live_out graph_so_far in
        let live_in_thn, graph_thn = helpA thn live_out graph_els in
        let live_in = StringSet.union fvs (StringSet.union live_in_thn live_in_els) in
        (* we know that all cnd could contribute to the interference graph
           is adding a single node (since it is an immexpr), which will get
           added anyways when we add_clique (since it will be in live_in) *)
        let new_graph = add_clique graph_thn live_in in
        (live_in, new_graph)
    | _ ->
        (* all imm cases are boring, just returning free vars in all *)
        (* in the lambda case, we just need to get the free vars to know
           what gets put in the closure. we don't make a recursive call
           for the body, since that isn't a part of this function's environment.
           that is exactly this general case's behavior :) *)
        let live_in = StringSet.union live_out (get_fv_C c) in
        let new_graph = add_clique graph_so_far live_in in
        (live_in, new_graph)
  in
  snd (helpA e live_out Graph.empty)
;;

(* transforms a graph, given an initial environment (representing "pre-coloring")
   and turn it into a map from names to registers / stack slots *)
let color_graph (g : grapht) (init_env : arg envt) : arg envt =
  (* we overapproximate and generate at most max_degree + 1 slots to use,
     adding in stack slots if needed at the *end* of the list *)
  let all_slots =
    let reg_len = List.length allocatable_registers in
    let reg_as_arg = List.map (fun x -> Reg x) allocatable_registers in
    let max_degree = try highest_degree_num g with _ -> 0 in
    if max_degree + 1 > reg_len then
      let num_stack_slots = highest_degree_num g + 1 - reg_len in
      let first_available = deepest_stack init_env in
      let added_stack_slots =
        build_list (fun i -> RegOffset (-word_size * (first_available + i), RBP)) num_stack_slots
      in
      reg_as_arg @ added_stack_slots
    else
      reg_as_arg
  in
  (* errors if all candidates are used, which won't be a problem
     since we over-approximate with all_slots above *)
  let rec find_first_unused (candidates : 'a list) (used : 'a list) =
    match candidates with
    | [] -> raise (InternalCompilerError "no unused candidates!")
    | c :: rest ->
        if List.mem c used then
          find_first_unused rest used
        else
          c
  in
  (* Find the node in the graph of smallest degree. Push it onto the worklist,
     and remove that node (and all its edges) from the interference graph.
     Then, recur until we are done. The result has the smallest last *)
  let rec make_worklist (g : grapht) (acc : string list) : string list =
    if Graph.is_empty g then
      acc
    else
      let min_degree_node = smallest_degree_node g in
      (* this is slow for now and sorts every time but oh well... *)
      let graph_without_node = remove_node g min_degree_node in
      make_worklist graph_without_node (min_degree_node :: acc)
  in
  (* actually color the graph, iterating through the worklist *)
  let rec graph_coloring (worklist : string list) (acc : arg envt) : arg envt =
    match worklist with
    | [] -> acc
    | node :: rest ->
        let neighbors = get_neighbors g node in
        let regs_of_neighbors =
          List.fold_left
            (fun lst name ->
              match List.assoc_opt name acc with
              | None -> lst
              | Some arg -> arg :: lst )
            [] neighbors
        in
        let min_color = find_first_unused all_slots regs_of_neighbors in
        graph_coloring rest ((node, min_color) :: acc)
  in
  graph_coloring (make_worklist g []) init_env
;;

let register_allocation (prog : (tag * StringSet.t) aprogram) :
    (tag * StringSet.t) aprogram * arg envt tag_envt =
  let rec helpA a env_so_far : arg envt tag_envt =
    match a with
    | ASeq (cx, ax, _) ->
        let env_cx = helpC cx env_so_far in
        helpA ax env_cx
    | ALet (_, e, b, _) ->
        let env_e = helpC e env_so_far in
        helpA b env_e
    | ALetRec (binds, b, _) ->
        let env_binds = List.fold_left (fun acc (_, bind) -> helpC bind acc) env_so_far binds in
        helpA b env_binds
    | ACExpr c -> helpC c env_so_far
  and helpC c env_so_far : arg envt tag_envt =
    match c with
    | CIf (_, t, e, _) ->
        let env_t = helpA t env_so_far in
        helpA e env_t
    | CLambda (args, body, (tag, _)) ->
        let body_graph = interfere body StringSet.empty in
        (* we handle args in the interference graph by just removing all of them,
           then re-adding by pre-coloring them *)
        let body_graph_no_args =
          List.fold_left (fun acc arg -> remove_node acc arg) body_graph args
        in
        let args_pre_colored =
          (* we will always reserve the spot for the closure itself, as per calling convention,
             as well as all other arguments *)
          ("$$closure$$", RegOffset (-8, RBP))
          :: List.mapi (fun i arg -> (arg, RegOffset (-word_size * (i + 2), RBP))) args
        in
        let new_env = (tag, color_graph body_graph_no_args args_pre_colored) :: env_so_far in
        helpA body new_env
    (* guaranteed to be no lambdas found here, so we don't need to walk *)
    | _ -> env_so_far
  in
  match prog with
  | AProgram (a, (tag, _)) ->
      let prog_graph = interfere a StringSet.empty in
      (prog, helpA a [(tag, color_graph prog_graph [])])
;;

(* size may be odd; we add the word of padding in here. size is in words
   NOTE: this clobbers RAX and the argument registers, don't store anything in there *)
let reserve size tag =
  let ok = sprintf "$memcheck_%d" tag in
  let padded_size = size + (size mod 2) in
  let bytes_size = padded_size * word_size in
  (* original code was to `mov RAX, [?HEAP_END]`, which is broken with macho
     instead, we use lea, like previously, but have to dereference as well...
     maybe there is a better way to do this (pro tip: use a better OS...) *)
  [ IInstrComment (ILea (Reg RAX, RelLabel "?HEAP_END"), sprintf "Reserving %d words" padded_size);
    IInstrComment (IMov (Reg RAX, RegOffset (0, RAX)), "Dereference the pointer to HEAP_END");
    ISub (Reg RAX, Const (Int64.of_int bytes_size));
    ICmp (Reg RAX, Reg heap_reg);
    IJge (Label ok) ]
  (* before we move in our arguments, we stash everything from our allocatable registers *)
  @ List.map (fun reg -> IPush (Reg reg)) allocatable_registers
  (* alloc_ptr in C *)
  @ [ IMov (Reg RDI, Sized (QWORD_PTR, Reg heap_reg));
      (* bytes_needed in C *)
      IMov (Reg RSI, Sized (QWORD_PTR, Const (Int64.of_int bytes_size)));
      (* first_frame in C *)
      IMov (Reg RDX, Sized (QWORD_PTR, Reg RBP));
      (* stack_top in C *)
      IMov (Reg RCX, Sized (QWORD_PTR, Reg RSP));
      ICall (Label "?try_gc");
      IInstrComment
        ( IMov (Reg heap_reg, Reg RAX),
          "assume gc success if returning here, so RAX holds the new heap_reg value" ) ]
  @ List.map (fun reg -> IPop (Reg reg)) (List.rev allocatable_registers)
  @ [ILabel (ok, None)]
;;

let rec compile_aexpr
    (e : (tag * StringSet.t) aexpr)
    (env_tag : tag)
    (env : arg envt tag_envt)
    (num_args : int)
    (is_tail : bool) : instruction list =
  match e with
  | ALet (id, bind, body, _) ->
      let bind_compiled = compile_cexpr bind env_tag env num_args false in
      let body_compiled = compile_aexpr body env_tag env num_args is_tail in
      (* env already binds id to the proper offset *)
      bind_compiled @ [IMov (find (find env env_tag) id, Reg RAX)] @ body_compiled
  | ASeq (c, a, _) ->
      let left_compiled = compile_cexpr c env_tag env num_args false in
      let right_compiled = compile_aexpr a env_tag env num_args is_tail in
      left_compiled @ right_compiled
  | ACExpr cexpr -> compile_cexpr cexpr env_tag env num_args is_tail
  | ALetRec (binds, body, _) ->
      (* reserve space for lambda and move the tagged pointer onto the stack *)
      let allocate_lambda (name, lam) =
        let closure_size = List.length (StringSet.elements (get_fv_C lam)) + 3 in
        let inc_size = closure_size + (closure_size mod 2) in
        reserve closure_size (fst (get_tag_C lam))
        @ [ IMov (Reg RAX, Reg heap_reg);
            IAdd (Reg RAX, closure_tag);
            IAdd (Reg heap_reg, Const (Int64.of_int (word_size * inc_size)));
            IMov (find (find env env_tag) name, Reg RAX) ]
      in
      let allocate_lambdas = List.concat (List.map allocate_lambda binds) in
      (* at this point in time, we have pointers on the stack to uninitialized heap.
         we could fill with 0s for garbage collection, or be careful about collections *)
      let compile_lambda (name, lam) =
        match lam with
        | CLambda (args, body, (lam_tag, fvs)) ->
            let free_vars = StringSet.elements fvs in
            let body_with_jumps = compile_lambda_body free_vars args body lam_tag env in
            let closure_load =
              compile_closure_load free_vars (List.length args) (sprintf "lambda_%d" lam_tag) RAX
                (find env env_tag)
            in
            body_with_jumps
            @ [IMov (Reg RAX, find (find env env_tag) name); ISub (Reg RAX, closure_tag)]
            @ closure_load
        | _ -> raise (InternalCompilerError "Expected lambda in letrec")
      in
      let compile_lambdas = List.concat (List.map compile_lambda binds) in
      let body_compiled = compile_aexpr body env_tag env num_args is_tail in
      allocate_lambdas @ compile_lambdas @ body_compiled

and compile_cexpr
    (e : (tag * StringSet.t) cexpr)
    (env_tag : tag)
    (env : arg envt tag_envt)
    (num_args : int)
    (is_tail : bool) =
  (* generates instructions that will load the argument into RAX and jump to the given
     label if it does not correspond with the given tag (using the mask to shave off)
     ASSUMES that tag_mask starts with 0s (and gets sign-extended to 64 bits)
     and also loads into scratch_reg as well *)
  let tag_check (tag_mask : arg) (tag : arg) (imm : arg) (label : string) =
    [ IMov (Reg RAX, imm);
      (* we have to do the tag check in scratch_reg, since we are masking off,
         and want THE right value to be in RAX (for returning and for errors) *)
      IMov (Reg scratch_reg, Reg RAX);
      (* tag_mask will get sign-extended with all 0s *)
      IAnd (Reg scratch_reg, tag_mask);
      (* if this cmp is 0, we had exactly the tag -> no jump *)
      ICmp (Reg scratch_reg, tag);
      IJnz (Label label) ]
  in
  let load_num = tag_check num_tag_mask num_tag in
  let load_bool = tag_check bool_tag_mask bool_tag in
  (* also untags the tuple here, and checks if it is nil *)
  let load_tuple (imm : arg) (label : string) =
    tag_check tuple_tag_mask tuple_tag imm label
    @ [ ISub (Reg RAX, tuple_tag);
        (* if we have a null pointer after untagging, error *)
        ICmp (Reg RAX, Const 0L);
        IJz (err_label_arg err_NIL_DEREF) ]
  in
  let load_closure (imm : arg) (label : string) =
    tag_check closure_tag_mask closure_tag imm label @ [ISub (Reg RAX, closure_tag)]
  in
  (* emits code that compares `left <cmp> right`, enforcing both are numbers.
     to determine how to jump, we use generate jump instruction to the given
     label using make_jump. true is loaded if we jump, and false is otherwise. *)
  let compile_cmp (left : arg) (right : arg) (make_jump : string -> instruction) (label : string) =
    let jump = make_jump label in
    ( match jump with
    | IJo _ | IJe _ | IJne _ | IJl _ | IJle _ | IJg _ | IJge _ | IJmp _ | IJz _ | IJnz _ -> ()
    | _ -> raise (InternalCompilerError "Error: expected to be given a function producing a jump")
    );
    (* now, compare and load true if we jump and false otherwise *)
    load_num left (err_label err_COMP_NOT_NUM)
    @ load_num right (err_label err_COMP_NOT_NUM)
    @ [ IMov (Reg RAX, left);
        IMov (Reg scratch_reg, right);
        ICmp (Reg RAX, Reg scratch_reg);
        IMov (Reg RAX, const_true) ]
    @ [jump]
    @ [IMov (Reg RAX, const_false); ILabel (label, None)]
  in
  match e with
  | CIf (cond, thn, els, (tag, _)) ->
      let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      let cond_imm = compile_imm cond env_tag env in
      load_bool cond_imm (err_label err_IF_NOT_BOOL)
      @ [ IMov (Reg scratch_reg, const_false);
          ICmp (Reg RAX, Reg scratch_reg);
          IJe (Label else_label) ]
      @ compile_aexpr thn env_tag env num_args is_tail
      @ [IJmp (Label done_label); ILabel (else_label, None)]
      @ compile_aexpr els env_tag env num_args is_tail
      @ [ILabel (done_label, None)]
  | CPrim1 (op, e, (tag, _)) -> (
      let imm = compile_imm e env_tag env in
      match op with
      | Add1 ->
          load_num imm (err_label err_ARITH_NOT_NUM)
          @ [IAdd (Reg RAX, Const 2L); IJo (err_label_arg err_OVERFLOW)]
      | Sub1 ->
          load_num imm (err_label err_ARITH_NOT_NUM)
          @ [ISub (Reg RAX, Const 2L); IJo (err_label_arg err_OVERFLOW)]
      | Not ->
          load_bool imm (err_label err_LOGIC_NOT_BOOL)
          @ [IMov (Reg scratch_reg, bool_mask); IXor (Reg RAX, Reg scratch_reg)]
      (* NOTE: we could probably abstract out differences between IsTuple and IsNum,
         but next week we are going to use more bit magic to check more efficiently,
         so that optimization will be thrown away anyways *)
      | IsBool ->
          let temp = sprintf "isbool_%d" tag in
          [ IMov (Reg RAX, imm);
            IMov (Reg scratch_reg, bool_tag_mask);
            IAnd (Reg RAX, Reg scratch_reg);
            (* if this cmp is 0, we had exactly the tag -> jump over false to keep true *)
            ICmp (Reg RAX, bool_tag);
            IMov (Reg RAX, const_true);
            IJz (Label temp);
            IMov (Reg RAX, const_false);
            ILabel (temp, None) ]
      | IsNum ->
          (* we can keep the shift, since only the LSB matters for this tag *)
          [ IMov (Reg RAX, imm);
            (* swaps tag bit and shifts into the T/F position, enforcing canonicity *)
            IXor (Reg RAX, Const 1L);
            IShl (Reg RAX, Const 63L);
            IMov (Reg scratch_reg, const_false);
            IOr (Reg RAX, Reg scratch_reg) ]
      | IsTuple ->
          let temp = sprintf "istuple_%d" tag in
          [ IMov (Reg RAX, imm);
            IMov (Reg scratch_reg, tuple_tag_mask);
            IAnd (Reg RAX, Reg scratch_reg);
            (* if this cmp is 0, we had exactly the tag -> jump over false to keep true *)
            ICmp (Reg RAX, tuple_tag);
            IMov (Reg RAX, const_true);
            IJz (Label temp);
            IMov (Reg RAX, const_false);
            ILabel (temp, None) ]
      | PrintStack ->
          (* this is needed so our registers don't get trashed, even though
             it does make actually using print_stack to debug much less convenient... *)
          let stash_caller_saved = List.map (fun reg -> IPush (Reg reg)) caller_saved_registers in
          let restore_caller_saved =
            List.map (fun reg -> IPop (Reg reg)) (List.rev caller_saved_registers)
          in
          stash_caller_saved
          @ [ IMov (Reg RDI, imm);
              IMov (Reg RSI, Reg RSP);
              IMov (Reg RDX, Reg RBP);
              IMov (Reg RCX, Const (Int64.of_int num_args));
              ICall (Label "print_stack") ]
          @ restore_caller_saved )
  | CPrim2 (op, left, right, (tag, _)) -> (
      let left_imm = compile_imm left env_tag env in
      let right_imm = compile_imm right env_tag env in
      match op with
      | Plus ->
          load_num left_imm (err_label err_ARITH_NOT_NUM)
          @ load_num right_imm (err_label err_ARITH_NOT_NUM)
          @ [ IMov (Reg RAX, left_imm);
              IMov (Reg scratch_reg, right_imm);
              IAdd (Reg RAX, Reg scratch_reg);
              IJo (err_label_arg err_OVERFLOW) ]
      | Minus ->
          load_num left_imm (err_label err_ARITH_NOT_NUM)
          @ load_num right_imm (err_label err_ARITH_NOT_NUM)
          @ [ IMov (Reg RAX, left_imm);
              IMov (Reg scratch_reg, right_imm);
              ISub (Reg RAX, Reg scratch_reg);
              IJo (err_label_arg err_OVERFLOW) ]
      | Times ->
          load_num left_imm (err_label err_ARITH_NOT_NUM)
          @ load_num right_imm (err_label err_ARITH_NOT_NUM)
          (* we shift right before we multiply, to prevent avoidable overflow *)
          @ [ IMov (Reg RAX, left_imm);
              ISar (Reg RAX, Const 1L);
              IMov (Reg scratch_reg, right_imm);
              IMul (Reg RAX, Reg scratch_reg);
              IJo (err_label_arg err_OVERFLOW) ]
      | Greater ->
          compile_cmp left_imm right_imm (fun x -> IJg (Label x)) (sprintf "greater_%d" tag)
      | GreaterEq ->
          compile_cmp left_imm right_imm (fun x -> IJge (Label x)) (sprintf "greater_eq_%d" tag)
      | Less -> compile_cmp left_imm right_imm (fun x -> IJl (Label x)) (sprintf "less_%d" tag)
      | LessEq ->
          compile_cmp left_imm right_imm (fun x -> IJle (Label x)) (sprintf "less_eq_%d" tag)
      | Eq ->
          (* handled differently than comparisons, since we don't enforce types *)
          let eq_label = sprintf "eq_%d" tag in
          [ IMov (Reg RAX, left_imm);
            IMov (Reg scratch_reg, right_imm);
            ICmp (Reg RAX, Reg scratch_reg);
            IMov (Reg RAX, const_true);
            IJe (Label eq_label);
            IMov (Reg RAX, const_false);
            ILabel (eq_label, None) ]
      | And -> raise (InternalCompilerError "Error: missed desugar of &&")
      | Or -> raise (InternalCompilerError "Error: missed desugar of ||")
      | CheckSize ->
          load_tuple left_imm (err_label err_PATTERN_NOT_TUPLE)
          (* stash away the tuple pointer in scratch_reg2 *)
          @ [IMov (Reg scratch_reg2, Reg RAX)]
          @ load_num right_imm (err_label err_PATTERN_NOT_TUPLE)
          @ [ ICmp (Reg RAX, RegOffset (0, scratch_reg2));
              IJne (err_label_arg err_TUPLE_SIZE_MISMATCH) ] )
  | CApp (func, args, ct, (tag, _)) -> (
      let push_args args =
        List.flatten
          (List.map
             (fun arg ->
               [ IMov (Reg scratch_reg, compile_imm arg env_tag env);
                 IPush (Sized (QWORD_PTR, Reg scratch_reg)) ] )
             args )
      in
      let arity = List.length args in
      match ct with
      | Snake ->
          let func_imm = compile_imm func env_tag env in
          (* tail calls do not have to save our registers, because whatever call
             the thing making the tail calls will have already taken care of that
             storage (and will restore once we return). this is okay, since OCSH
             is not in tail position now *)
          if is_tail then
            let loaded_closure = load_closure func_imm (err_label err_CALL_NOT_CLOSURE) in
            let arity_check =
              [ (* make sure to convert from machine arity to SNAKEVAL
                   Also, note the arity we store does not include the function itself *)
                ICmp (Sized (QWORD_PTR, RegOffset (0, RAX)), Const (Int64.of_int (arity * 2)));
                IJne (err_label_arg err_CALL_ARITY_ERR) ]
            in
            (* adjust RSP, in case we are trying to make a tail call to a function
               whose arity is greater than the current number of args + locals we have *)
            let adjust_rsp =
              [ IMov (Reg scratch_reg, Reg RBP);
                (* we add 1 to the arity, since we have to push itself now *)
                ISub (Reg scratch_reg, Const (Int64.of_int ((arity + 1) * word_size)));
                ICmp (Reg scratch_reg, Reg RSP);
                (* only update RSP in the edge case where it matters *)
                ICmovl (Reg RSP, Reg scratch_reg) ]
            in
            let pushed_args =
              IMov (Reg scratch_reg, func_imm)
              :: IPush (Sized (QWORD_PTR, Reg scratch_reg))
              :: push_args args
            in
            (* push on s.t. last arg is at top of stack -> pop into last slot *)
            let pop_args =
              build_list_rev
                (fun n -> IPop (Sized (QWORD_PTR, RegOffset (~-word_size * n, RBP))))
                (arity + 1)
            in
            (* set RSP to be above the args, since the first thing a function
               does when we enter is claim space for its args *)
            let reset_rsp =
              [ IMov (Reg scratch_reg, Reg RBP);
                ISub (Reg scratch_reg, Const (Int64.of_int ((arity + 1) * word_size)));
                IMov (Reg RSP, Reg scratch_reg);
                IJmp (RegOffset (word_size, RAX)) ]
            in
            loaded_closure @ arity_check @ adjust_rsp @ pushed_args @ pop_args @ reset_rsp
          else
            let loaded_closure = load_closure func_imm (err_label err_CALL_NOT_CLOSURE) in
            let arity_check =
              [ (* make sure to convert from machine arity to SNAKEVAL
                   Also, note the arity we store does not include the function itself *)
                ICmp (Sized (QWORD_PTR, RegOffset (0, RAX)), Const (Int64.of_int (arity * 2)));
                IJne (err_label_arg err_CALL_ARITY_ERR) ]
            in
            (* first, push on the function itself, then everything else *)
            let pushed_args =
              IMov (Reg scratch_reg, func_imm)
              :: IPush (Sized (QWORD_PTR, Reg scratch_reg))
              :: push_args args
            in
            (* due to new calling convention, we cannot use `call` keyword
               we push on return address (really a label), old RBP, args
               then we update RBP (after args, since they may be offsets from RBP) *)
            let ret_addr = sprintf "ret_addr_%d" tag in
            (* Push label content, stashing away the RSP that we will want to change RBP to
               in scratch_reg2 temporarily, since we use scratch_reg to bounce arguments off of *)
            loaded_closure @ arity_check
            (* before we put on the ret addr and stack pointer, we save everything
               that is allocatable, since our internal CC is that all are caller saved *)
            @ List.map (fun reg -> IPush (Reg reg)) allocatable_registers
            @ [ ILea (Reg scratch_reg, RelLabel ret_addr);
                IPush (Reg scratch_reg);
                IPush (Reg RBP);
                IMov (Reg scratch_reg2, Reg RSP) ]
            @ pushed_args
            (* we already pushed the old RBP on, but didn't update the current RBP yet.
               By new calling convention, we want it to be under all of the argument.
               Note that we maintain the untagged pointer to closure in RAX *)
            @ [ IMov (Reg RBP, Reg scratch_reg2);
                IJmp (RegOffset (word_size, RAX));
                ILabel (ret_addr, Some 2) ]
            @ List.map (fun reg -> IPop (Reg reg)) (List.rev allocatable_registers)
      | Native label ->
          let stash_caller_saved = List.map (fun reg -> IPush (Reg reg)) caller_saved_registers in
          let reg_loads =
            List.flatten
              (List.map2
                 (fun reg arg -> [IMov (Reg reg, compile_imm arg env_tag env)])
                 (take arity first_six_args_registers)
                 (take 6 args) )
          in
          let stack_loads = push_args (drop 6 args) in
          (* if we are odd and loaded anything on to the stack, 16-byte align.
             we have 8 caller-saved registers that we add, so they don't change parity *)
          let maybe_pad =
            if arity > 6 && arity mod 2 == 0 then
              [IPush (Const 0L)]
            else
              []
          in
          let restore_caller_saved =
            List.map (fun reg -> IPop (Reg reg)) (List.rev caller_saved_registers)
          in
          stash_caller_saved @ reg_loads @ stack_loads @ maybe_pad @ [ICall (Label label)]
          @ restore_caller_saved
      | Prim -> raise (InternalCompilerError "Prim call type not supported")
      | Unknown -> raise (InternalCompilerError "Unknown call type") )
  | CTuple (elems, (tag, _)) ->
      let num_elem = List.length elems in
      let size_unpadded = num_elem + 1 in
      let load_elems =
        List.flatten
          (List.mapi
             (fun i imm ->
               [ IMov (Reg scratch_reg, compile_imm imm env_tag env);
                 IMov (RegOffset (word_size * (i + 1), heap_reg), Reg scratch_reg) ] )
             elems )
      in
      let maybe_pad_heap =
        if size_unpadded mod 2 = 1 then
          [ IMov (Sized (QWORD_PTR, RegOffset (0, heap_reg)), Const Int64.zero);
            IAdd (Reg heap_reg, Const (Int64.of_int word_size)) ]
        else
          []
      in
      (* store as a SNAKEVAL in memory so our index errors later have SNAKEVALs to use *)
      (* reserve itself will reserve space for the padding word if needed *)
      reserve size_unpadded tag
      @ IMov (RegOffset (0, heap_reg), Sized (DWORD_PTR, Const (Int64.of_int (num_elem * 2))))
        :: load_elems
      @ [ IMov (Reg RAX, Reg heap_reg);
          IAdd (Reg RAX, tuple_tag);
          IAdd (Reg heap_reg, Const (Int64.of_int (word_size * (num_elem + 1)))) ]
      @ maybe_pad_heap
  | CGetItem (tup, idx, _) ->
      let tup_imm = compile_imm tup env_tag env in
      let idx_imm = compile_imm idx env_tag env in
      load_tuple tup_imm (err_label err_GET_NOT_TUPLE)
      (* stash away the tuple pointer in scratch_reg2 *)
      @ [IMov (Reg scratch_reg2, Reg RAX)]
      @ load_num idx_imm (err_label err_GET_NOT_NUM)
      @ [ ICmp (Reg RAX, Const 0L);
          IJl (err_label_arg err_GET_LOW_INDEX);
          ICmp (Reg RAX, RegOffset (0, scratch_reg2));
          IJge (err_label_arg err_GET_HIGH_INDEX);
          (* we multiply RAX by word_size / 2 and not word_size, since it is a SNAKEVAL
             and therefore has already been multipled by 2 *)
          IMov (Reg RAX, RegOffsetReg (scratch_reg2, RAX, word_size / 2, word_size)) ]
  | CSetItem (tup, idx, new_val, _) ->
      let tup_imm = compile_imm tup env_tag env in
      let idx_imm = compile_imm idx env_tag env in
      let val_imm = compile_imm new_val env_tag env in
      load_tuple tup_imm (err_label err_SET_NOT_TUPLE)
      (* stash away the tuple pointer in scratch_reg2 *)
      @ [IMov (Reg scratch_reg2, Reg RAX)]
      @ load_num idx_imm (err_label err_SET_NOT_NUM)
      @ [ ICmp (Reg RAX, Const 0L);
          IJl (err_label_arg err_SET_LOW_INDEX);
          ICmp (Reg RAX, RegOffset (0, scratch_reg2));
          IJge (err_label_arg err_SET_HIGH_INDEX);
          (* Right now, the index (as a SNAKEVAL) is in RAX;
             we therefore multiply RAX by word_size / 2 to access
             We bounce the new_val off of scratch_reg, and remember to load
             it into RAX at the end (since that is what the set returns) *)
          IMov (Reg scratch_reg, val_imm);
          IMov (RegOffsetReg (scratch_reg2, RAX, word_size / 2, word_size), Reg scratch_reg);
          IMov (Reg RAX, Reg scratch_reg) ]
  | CImmExpr e ->
      let imm = compile_imm e env_tag env in
      [IMov (Reg RAX, imm)]
  | CLambda (args, body, (lam_tag, fvs)) ->
      let free_vars = StringSet.elements fvs in
      let body_with_jumps = compile_lambda_body free_vars args body lam_tag env in
      let closure_load =
        compile_closure_load free_vars (List.length args) (sprintf "lambda_%d" lam_tag) heap_reg
          (* the closure we are looking up in may not have anything there,
             if there are no arguments or locals. *)
          ( try find env env_tag with InternalCompilerError _ -> [] )
      in
      let closure_size = List.length free_vars + 3 in
      let inc_size = closure_size + (closure_size mod 2) in
      body_with_jumps
      (* we reserve with the closure_size, since it will add the padding for us *)
      (* also, closure_load itself handles filling the padding word *)
      @ reserve closure_size lam_tag
      @ closure_load
      @ [ IMov (Reg RAX, Reg heap_reg);
          IAdd (Reg RAX, closure_tag);
          IAdd (Reg heap_reg, Const (Int64.of_int (word_size * inc_size))) ]

(* jump over lambda body, body itself, but no closure loading after.
   note that the label to jump to will be `lambda_tag` *)
and compile_lambda_body fvs args body lam_tag env =
  let body_label = sprintf "lambda_%d" lam_tag in
  let done_label = sprintf "lambda_%d_end" lam_tag in
  let lam_env = try find env lam_tag with _ -> [] in
  (* Prologue *)
  let num_args = List.length args + 1 in
  (* if we had zero args, zero locals, we won't find anything in env for tag.
     we set that case to 1, so when we subtract (args + 1) = (0 + 1), we get 0
     intuition: even with zero args, zero locals, we still save the slot for itself *)
  let stack_depth = try deepest_stack (find env lam_tag) with InternalCompilerError _ -> 1 in
  (* this reserves space for closure unpacking AND locals.
     this may be zero or negative, since we aren't necessarily
     unpacking the closure onto the stack (register allocation!) *)
  let to_reserve = max 0 (stack_depth - num_args) in
  let to_reserve_padded = to_reserve + (to_reserve mod 2) in
  let reserve_insts = replicate (IPush (Const 0L)) to_reserve_padded in
  (* Unpacking *)
  let load_closure = [IMov (Reg RAX, RegOffset (-word_size, RBP)); ISub (Reg RAX, closure_tag)] in
  (* mapi starts at 0, so we add 3 to get to [RAX + 24] to load first *)
  let unpack_closure =
    List.flatten
      (List.mapi
         (fun i name ->
           [ IMov (Reg scratch_reg, RegOffset (word_size * (i + 3), RAX));
             IMov (find lam_env name, Reg scratch_reg) ] )
         fvs )
  in
  (* Function body: the body of a function is in tail position
     note that we pass down the new tag, for a new scope *)
  let compiled_body = compile_aexpr body lam_tag env num_args true in
  (* function epilogue *)
  let body_epilogue = [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] in
  [IJmp (Label done_label); ILabel (body_label, Some 2)]
  @ reserve_insts @ load_closure @ unpack_closure @ compiled_body @ body_epilogue
  @ [ILabel (done_label, None)]

(* load up the closure starting at the addreess from the given reg
   where label is the code pointer to the body itself, and we pass the
   current env (not env for body) to look up where to load.
   this will mutate the contents of scratch_reg and the given reg; none else.
   note that this will fill the padded word with 0s, and doesn't change heap_reg *)
and compile_closure_load free_vars arity label reg sub_env =
  let num_free_vars = List.length free_vars in
  let load_free_vars =
    List.flatten
      (List.mapi
         (fun i name ->
           [ IMov (Reg scratch_reg, find sub_env name);
             (* add 2 since we start at 0 and want [REG + 24] to come first *)
             IMov (RegOffset (word_size * (3 + i), reg), Reg scratch_reg) ] )
         free_vars )
  in
  let maybe_padding =
    if (num_free_vars + 3) mod 2 = 0 then
      []
    else
      (* in this case, we used the first num_free_vars + 3 words,
         so we add in a padding word at word num_free_vars + 4 *)
      [IMov (Sized (QWORD_PTR, RegOffset (word_size * (num_free_vars + 4), reg)), Const 0L)]
  in
  [ IMov (Sized (QWORD_PTR, RegOffset (0, reg)), Const (Int64.of_int (arity * 2)));
    ILea (Reg scratch_reg, RelLabel label);
    IMov (Sized (QWORD_PTR, RegOffset (word_size, reg)), Reg scratch_reg);
    IMov
      (Sized (QWORD_PTR, RegOffset (word_size * 2, reg)), Const (Int64.of_int (num_free_vars * 2)))
  ]
  @ load_free_vars @ maybe_padding

and compile_imm e (env_tag : tag) env =
  match e with
  (* All SNAKEVALs are shifted left at compilation and shifted back in C for intepretation
     We must tag here, and untag later *)
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find (find env env_tag) x
  | ImmNil _ -> const_nil
;;

(* compiles a function, abstracting compilation of decls and our_code_starts_here *)
(* let compile_fun (name : string) (args : string list) (env : arg envt tag_envt) (body : tag aexpr) :
       instruction list =
     let num_args = List.length args in
     let offset = deepest_stack body env num_args in
     let offset_aligned = offset + (offset mod 2) in
     (* This is mostly to help us use print stack: instead of increment RSP,
        which is faster, we just push zeroes onto the stack as padding/placeholder *)
     let prelude = ILabel name :: replicate (IPush (Const 0L)) offset_aligned in
     (* let prelude = [ILabel name; ISub (Reg RSP, Const (Int64.of_int (8 * offset_aligned)))] in *)
     let compile_body = compile_aexpr body env num_args true in
     let postlude = [IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet] in
     prelude @ compile_body @ postlude
   ;; *)

(* This function can be used to take the native functions and produce DFuns whose bodies
   simply contain an EApp (with a Native call_type) to that native function.  Then,
   your existing compilation can turn these DFuns into ELambdas, which can then be called
   as in the rest of Fer-De-Lance, but the Native EApps will do the work of actually
   native_calling the runtime-provided functions. *)

(* let add_native_lambdas (p : sourcespan program) =
   let wrap_native name arity =
     let argnames = List.init arity (fun i -> sprintf "%s_arg_%d" name i) in
     [DFun(name, List.map (fun name -> BName(name, false, dummy_span)) argnames, EApp(EId(name, dummy_span), List.map(fun name -> EId(name, dummy_span)) argnames, Native, dummy_span), dummy_span)]
   in
   match p with
   | Program(declss, body, tag) ->
     Program((List.fold_left (fun declss (name, (_, arity)) -> (wrap_native name arity)::declss) declss native_fun_bindings), body, tag)
*)

let compile_prog ((anfed : (tag * StringSet.t) aprogram), (env : arg envt tag_envt)) : string =
  let prelude_no_defaults =
    "section .text\n\
     global our_code_starts_here\n\
     extern error\n\
     extern print_stack\n\
     extern STACK_BOTTOM\n\
     extern ?try_gc\n\
     extern ?HEAP_END\n"
  in
  let get_asm_label (_, ct, _) =
    match ct with
    | Native s -> sprintf "extern %s" s
    | _ -> ""
  in
  let prelude = prelude_no_defaults ^ String.concat "\n" (List.map get_asm_label initial_fun_env) in
  let error_handle_label (code, name) =
    ILabel (name, None)
    :: [IMov (Reg RSI, Reg RAX); IMov (Reg RDI, Const code); ICall (Label "error")]
  in
  let error_asm = List.concat (List.map error_handle_label all_errors) in
  match anfed with
  | AProgram (body, (tag, _)) ->
      (* OCSH was called by C, meaning we have to do some extra instructions for this case specifically,
          we adapt it to our new, internal calling convention here *)
      let ocsh_prelude =
        ILabel ("our_code_starts_here", None)
        :: (* before pushing RBP and stuff, stash all callee saved registers
              (below STACK_BOTTOM, so we don't try to visit them during GC) *)
           List.map (fun reg -> IPush (Reg reg)) callee_saved_registers
        @ [ IPush (Reg RBP);
            IMov (Reg RBP, Reg RSP);
            IMov (RelLabel "STACK_BOTTOM", Reg RBP);
            (* load heap_red with our argument, the heap pointer, which is already aligned *)
            IMov (Reg heap_reg, Reg (List.nth first_six_args_registers 0)) ]
          (* zero out all allocatable registers, so GC doesn't break.
             allowed, since we have stashed everything we are in charge of saving *)
        @ List.map (fun reg -> IXor (Reg reg, Reg reg)) allocatable_registers
      in
      (* still compile ocsh as if it had zero arguments *)
      let offset = try deepest_stack (find env tag) with InternalCompilerError _ -> 0 in
      let offset_aligned = offset + (offset mod 2) in
      let reserve = replicate (IPush (Const 0L)) offset_aligned in
      (* we specify that OCSH itself isn't in tail position, to ensure that the default
         function environment (now converted to lambdas) are kept on the stack.
         also, we now use callee-saved registers, so those have to be saved too,
         meaning we need to pop all back into place before returning to C *)
      let ocsh_body = compile_aexpr body tag env 1 false in
      let postlude =
        [IMov (Reg RSP, Reg RBP); IPop (Reg RBP)]
        (* now, we put all of the callee saved values back in their rightful registers,
           popping off in reverse order from how they were pushed on *)
        @ List.map (fun reg -> IPop (Reg reg)) (List.rev callee_saved_registers)
        @ [IRet]
      in
      let as_assembly_string = to_asm (ocsh_prelude @ reserve @ ocsh_body @ postlude @ error_asm) in
      sprintf "%s%s\n" prelude as_assembly_string
;;

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)

let run_if should_run f =
  if should_run then
    f
  else
    no_op_phase
;;

let pick_alloc_strategy (strat : alloc_strategy) =
  match strat with
  | Naive -> naive_stack_allocation
  | Register -> register_allocation
;;

(* To handle function naming errors as variable naming errors, we desugar
   *before* we check well-formedness. To ensure proper error reporting,
   we carefully maintain the original sourcespan information *)

(* Add at least one desugaring phase somewhere in here *)
let compile_to_string
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
  |> add_phase result compile_prog
;;

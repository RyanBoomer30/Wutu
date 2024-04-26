open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors
open Compile
open Constants

(* More helper functions *)

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

let rec replicate x i =
  if i <= 0 then
    []
  else
    x :: replicate x (i - 1)
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
  let prelude = prelude_no_defaults ^ String.concat "\n" (List.map get_asm_label (initial_fun_env @ js_fun_env)) in
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

let compile_to_asm_string
    ?(no_builtins = false)
    (alloc_strat : alloc_strategy)
    (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_phase desugared (desugar ~no_builtins ~no_js_builtins:true)
  |> add_err_phase well_formed is_well_formed
  |> add_phase tagged tag |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase cached free_vars_cache
  |> add_phase locate_bindings (pick_alloc_strategy alloc_strat)
  |> add_phase result compile_prog
;;

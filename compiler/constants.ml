open Exprs
open Assembly

(* SNAKE CONSTANTS *)

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

let max_snake_val = Int64.div Int64.max_int 2L

let min_snake_val = Int64.div Int64.min_int 2L

(* ASM CONSTANTS *)

let first_six_args_registers = [RDI; RSI; RDX; RCX; R8; R9]

let unused_registers = [R12; R13; R14; RBX]

let allocatable_registers = unused_registers @ first_six_args_registers

let callee_saved_registers = [RBX; R12; R13; R14; R15]

let caller_saved_registers = R10 :: R11 :: first_six_args_registers

(* You may find some of these helpers useful *)

(* register constants *)

let heap_reg = R15

let scratch_reg = R11

let scratch_reg2 = R10

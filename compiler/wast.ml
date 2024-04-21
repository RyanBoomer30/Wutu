open Printf

type val_type =
  | I64
  | I32
  | F64
  | F32

type global_type =
  | Immut of val_type
  | Mut of val_type

(* list of parameter types, plus output type *)
type func_type = val_type list * val_type

type winstr =
  (* Constants *)
  | WI32Const of int  (* technically these are i31 in OCaml, but that's okay for our purposes *)
  | WI64Const of int64
  (* Globals (all of which are named here) *)
  | WGlobalGet of string
  | WGlobalSet of string
  (* Control flow *)
  | WCall
  | WCallIndirect of int  (* arity *)
  | WTailCall of int  (* arity *)
  | WIfThen of winstr list  (* convenience *)
  | WIfThenElse of func_type * winstr list * winstr list
  (* Locals *)
  | WLocalGetName of string
  | WLocalSetName of string
  | WLocalIdxGet of int
  | WLocalIdxSet of int
  (* Memory *)
  | WStore of int  (* offset, also always storing i64 *)
  | WLoad of int  (* offset, also always loading i64 *)  
  (* OPERATIONS *)
  (* Numeric: *)
  | WAdd of val_type
  | WSub of val_type
  (* | WMul *)
  (* for now, these only operate on i64, since nothing more is necessary *)
  | WAnd
  | WGt
  | WGe
  | WLt
  | WLe
  | WEq
  (* Conversions: *)
  | WI64ExtendI32
  | WI32WrapI64

type importable = 
  (* initial size of function table *)
  | FuncTableImport of int
  (* initial size (in pages) of memory *)
  | MemoryImport of int
  (* Global name and its type *)
  | GlobalImport of string * global_type
  (* Function name and arity *)
  | FunctionImport of string * int

type import = string * string * importable

(* all globals have names, according to this signature *)
type global = string * global_type * winstr list

(* offset plus indices of functions to populate function table *)
type elem_segment = int * int list

(* name of function, name to export at, arity, and number of (unnamed) locals *)
type wfunc = string option * string option * int * int * winstr list

(* convenience constructor for wasm functions, since most will be unnamed and un-exported *)
let wfunc_def arity locals body = (None, None, arity, locals, body)

type wmodule = {
  imports : import list;
  globals : global list;
  funtypes : func_type list;
  elems : elem_segment;
  funcs : wfunc list
}
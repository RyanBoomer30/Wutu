open Printf
open Errors
open Assembly

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
  (* Comments *)
  | WComment of string
  | WCommentInstr of winstr * string
  (* Constants *)
  (* technically these are i31 in OCaml, but that's okay for our purposes.
     we never get close to using the missing bit *)
  | WI32Const of int
  | WI64Const of int64
  | WI64HexConst of arg (* arg is specifically a HexConst, for compatability purposes *)
  (* Globals (all of which are named here) *)
  | WGlobalGet of string
  | WGlobalSet of string
  (* Control flow *)
  | WCall of string
  | WCallIndirect of int * bool (* arity and whether or not it is in tail position *)
  | WIfThen of winstr list (* convenience *)
  | WIfThenElse of func_type * winstr list * winstr list
  (* Locals *)
  | WLocalGet of int
  | WLocalSet of int
  (* Memory *)
  | WStore of int (* offset, also always storing i64 *)
  | WLoad of int (* offset, also always loading i64 *)
  (* OPERATIONS *)
  (* Numeric: *)
  | WAdd of val_type
  | WSub of val_type
  | WMul of val_type
  | WShr of val_type
  (* for now, these only operate on i64, since nothing more is necessary *)
  | WAnd
  | WXor
  | WGt
  | WGe
  | WLt
  | WLe
  | WEq
  | WNe
  | WEqz
  (* Conversions: *)
  | WI64ExtendI32
  | WI32WrapI64
  (* Drop *)
  | WDrop

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

(* all globals have names, according to this signature.
   for now, our globals are all i32s, and as such we store an int
   as the initial value (still specifying global_type for mutability).
   an extension would allow for any value here *)
type global = string * global_type * int

(* offset plus indices of functions to populate function table *)
type elem_segment = int * int list

(* name of function, name to export at, arity, and number of (unnamed) locals *)
type wfunc = string option * string option * int * int * winstr list

(* convenience constructor for wasm functions, since most will be unnamed and un-exported *)
let wfunc_def arity locals body = (None, None, arity, locals, body)

type wmodule =
  { imports: import list;
    globals: global list;
    funtypes: func_type list;
    elems: elem_segment;
    funcs: wfunc list }

(* printing to .wat file *)

let string_of_val_type vt =
  match vt with
  | I64 -> "i64"
  | I32 -> "i32"
  | F64 -> "f64"
  | F32 -> "f32"
;;

let string_of_global_type gt =
  match gt with
  | Immut vt -> string_of_val_type vt
  | Mut vt -> sprintf "(mut %s)" (string_of_val_type vt)
;;

let string_of_func_type (params, result) =
  let param_str =
    if params = [] then
      ""
    else
      sprintf "(param %s) " (String.concat " " (List.map string_of_val_type params))
  in
  sprintf "%s(result %s)" param_str (string_of_val_type result)
;;

let string_of_func_type_top ft = sprintf "  (type (func %s))" (string_of_func_type ft)

let rec replicate x i =
  if i <= 0 then
    []
  else
    x :: replicate x (i - 1)
;;

let rec string_of_winstr winst =
  match winst with
  | WComment s -> "    ;; " ^ s
  | WCommentInstr (inst, s) -> string_of_winstr inst ^ "  ;; " ^ s
  | WI32Const i -> sprintf "    i32.const %+d" i
  | WI64Const i -> sprintf "    i64.const %+Ld" i
  | WI64HexConst (HexConst n) -> sprintf "    i64.const 0x%Lx" n
  | WI64HexConst _ -> raise (InternalCompilerError "WI64HexConst only supports HexConst args")
  | WGlobalGet s -> sprintf "    global.get $%s" s
  | WGlobalSet s -> sprintf "    global.set $%s" s
  | WCall s -> sprintf "    call $%s" s
  (* we have to spell these all out inline, to get the wat to consistently compile
     if the user has errors in _their_ arities *)
  | WCallIndirect (d, false) ->
      sprintf "    call_indirect %s" (string_of_func_type (replicate I64 d, I64))
  | WCallIndirect (d, true) ->
      sprintf "    return_call_indirect %s" (string_of_func_type (replicate I64 d, I64))
  | WIfThen ws ->
      sprintf "    (if\n      (then\n    %s\n      )\n    )"
        (String.concat "\n    " (List.map string_of_winstr ws))
  | WIfThenElse (ft, ws1, ws2) ->
      sprintf
        "    (if %s\n      (then\n        %s\n      )\n      (else\n        %s\n      )\n    )"
        (string_of_func_type ft)
        (String.concat "\n    " (List.map string_of_winstr ws1))
        (String.concat "\n    " (List.map string_of_winstr ws2))
  | WLocalGet i -> sprintf "    local.get %d" i
  | WLocalSet i -> sprintf "    local.set %d" i
  | WStore off -> sprintf "    i64.store offset=%d" off
  | WLoad off -> sprintf "    i64.load offset=%d" off
  | WAdd vt -> sprintf "    %s.add" (string_of_val_type vt)
  | WSub vt -> sprintf "    %s.sub" (string_of_val_type vt)
  | WMul vt -> sprintf "    %s.mul" (string_of_val_type vt)
  | WShr vt -> sprintf "    %s.shr_s" (string_of_val_type vt)
  | WAnd -> "    i64.and"
  | WXor -> "    i64.xor"
  | WGt -> "    i64.gt_s"
  | WGe -> "    i64.ge_s"
  | WLt -> "    i64.lt_s"
  | WLe -> "    i64.le_s"
  | WEq -> "    i64.eq"
  | WNe -> "    i64.ne"
  | WEqz -> "    i64.eqz"
  | WI64ExtendI32 -> "    i64.extend_i32_s"
  | WI32WrapI64 -> "    i32.wrap_i64"
  | WDrop -> "    drop"
;;

let string_of_importable impbl =
  match impbl with
  | FuncTableImport size -> sprintf "(table %d funcref)" size
  | MemoryImport size -> sprintf "(memory %d)" size
  | GlobalImport (name, gt) -> sprintf "(global $%s %s)" name (string_of_global_type gt)
  | FunctionImport (name, arity) -> sprintf "(func $%s (type %d))" name arity
;;

let string_of_import (s1, s2, impbl) =
  sprintf "  (import \"%s\" \"%s\" %s)" s1 s2 (string_of_importable impbl)
;;

let string_of_global (name, gt, init) =
  (* for now, initial value is always an i32 *)
  sprintf "  (global $%s %s (i32.const %+d))" name (string_of_global_type gt) init
;;

let string_of_elem_segment (offset, indices) =
  sprintf "  (elem (i32.const %d) %s)" offset (String.concat " " (List.map string_of_int indices))
;;

let string_of_wfunc (name, export, arity, num_locals, winstrs) =
  let namestr =
    match name with
    | Some s -> " $" ^ s
    | None -> ""
  in
  let expstring =
    match export with
    | Some s -> sprintf "(export \"%s\") " s
    | None -> ""
  in
  sprintf "  (func%s %s(type %d)\n    (local%s)\n%s\n  )" namestr expstring arity
    (String.concat "" (replicate " i64" num_locals))
    (String.concat "\n" (List.map string_of_winstr winstrs))
;;

let watstring_of_wmodule {imports= imps; globals= globs; funtypes= fts; elems= elms; funcs= wfuns} =
  sprintf "(module\n%s\n%s\n%s\n%s\n\n%s\n)"
    (String.concat "\n" (List.map string_of_import imps))
    (String.concat "\n" (List.map string_of_global globs))
    (String.concat "\n" (List.map string_of_func_type_top fts))
    (string_of_elem_segment elms)
    (String.concat "\n" (List.map string_of_wfunc wfuns))
;;

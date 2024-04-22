(module
  (func $i (import "imports" "imported_func") (param i64))
  
  (func $add (import "imports" "safeAdd") (param i64) (param i64) (result i64))
  
  (func (export "exported_func") (result i64)
    i64.const +9223372036854775807
    i64.const +3
    call $add
  )
)
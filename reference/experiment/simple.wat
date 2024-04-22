(module
  (func $i (import "imports" "imported_func") (param i64))
  (func (export "exported_func")
    i64.const +84
    call $i
  )
)

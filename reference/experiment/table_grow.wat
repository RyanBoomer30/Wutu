(module
  (import "js" "tbl" (table 0 funcref))
  (func (result i32) i32.const 42)
  (func (result i32) i32.const 83)
  (elem declare funcref (ref.func 0) (ref.func 1))
  
  (export "ocsh" (func $ocsh))
  (func $ocsh (result i32)
  	(ref.null func)
    i32.const 2
    table.grow
    
    (table.set 0 (i32.const 0) (ref.func 0))
    (table.set 0 (i32.const 1) (ref.func 1))
  )
)
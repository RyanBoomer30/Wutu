(module
  (import "js" "tbl" (table 0 funcref))
  (func $a (result i32) i32.const 42)
  (func $b (result i32) i32.const 83)
  (elem declare funcref (ref.func $a) (ref.func $b))
  
  (func $ocsh (export "ocsh") (result i32)
  	(ref.null func)
    i32.const 2
    table.grow
    
    (table.set 0 (i32.const 0) (ref.func $a))
    (table.set 0 (i32.const 1) (ref.func $b))
  )
)
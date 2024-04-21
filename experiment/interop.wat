(module
  (import "js" "tbl" (table 1 funcref))
  (func $do_stuff (import "imports" "doStuff") (param i64 i64) (result i64))

  (memory (import "js" "mem") 10)
  (global $r15 (mut i32) (i32.const 0))

  (elem (i32.const 0) $id)

  ;; if max arity function in our program is n,
  ;; there should be exactly n + 1 of these
  (type (func (result i64)))
  (type (func (param i64) (result i64)))
  (type (func (param i64 i64) (result i64)))

  (func $id (param $clos i64) (param $x i64) (result i64)
    local.get $x
  )

  (func $ocsh (export "ocsh") (result i64)
    (local $id_clos i64)

    ;; build and load the add closure
    global.get $r15
    i64.const +2
    i64.store offset=0

    global.get $r15
    i64.const +0
    i64.store offset=8
    
    global.get $r15
    i64.const +0
    i64.store offset=16

    global.get $r15
    i64.const +0
    i64.store offset=24

    global.get $r15
    i32.const +5
    i32.add
    i64.extend_i32_s
    local.set $id_clos

    global.get $r15
    i32.const +32
    i32.add
    global.set $r15

    ;; at this point, we have the closure identity function
    local.get $id_clos
    i64.const +69
    call $do_stuff
  )
)
(module
  (import "js" "tbl" (table 2 funcref))
  (memory (import "js" "mem") 10)
  (global $r15 (mut i32) (i32.const 0))

  (elem (i32.const 0) $outer $inner)

  ;; if max arity function in our program is n,
  ;; there should be exactly n + 1 of these
  (type (func (result i64)))
  (type (func (param i64) (result i64)))
  (type (func (param i64) (param i64) (result i64)))

  (func $outer (param $clos i64) (param $x i64) (result i64)
    (local $ret i64)
    global.get $r15
    i32.const +5
    i32.add
    i64.extend_i32_s
    local.set $ret

    global.get $r15
    i64.const +2
    i64.store offset=0

    global.get $r15
    i64.const +1
    i64.store offset=8
    
    global.get $r15
    i64.const +1
    i64.store offset=16

    global.get $r15
    local.get $x
    i64.store offset=24

    global.get $r15
    i32.const +32
    i32.add
    global.set $r15

    local.get $ret
  )

  (func $inner (param $clos i64) (param $y i64) (result i64)
    (local $x i64)

    local.get $clos
    i32.wrap_i64
    i32.const +5
    i32.sub
    i64.load offset=24
    local.set $x

    local.get $x
    local.get $y
    i64.add
  )

  (func $ocsh (export "ocsh") (result i64)
    (local $add i64)
    (local $tmp i64)

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
    local.set $add

    global.get $r15
    i32.const +32
    i32.add
    global.set $r15

    ;; <tag check> goes here with error call
    
    
    local.get $add
    i64.const +1

    local.get $add
    i32.wrap_i64
    i32.const +5
    i32.sub
    i64.load offset=8
    i32.wrap_i64

    call_indirect (type 2)
    local.set $tmp

    ;; we just called add to get tmp,
    ;; time to call tmp to get our result!
    local.get $tmp
    ;; i64.const +2

    local.get $tmp
    i32.wrap_i64
    i32.const +5
    i32.sub
    i64.load offset=8
    i32.wrap_i64

    ;; what the typechecker tells us is whether the stuff
    ;; that we pushed on for the calls agrees with the arity
    ;; that we specify. then, at runtime, it checks if the
    ;; arity we specify is the actual arity
    call_indirect (type 1)
  )
)
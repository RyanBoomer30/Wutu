(module
  (import "runtime" "table" (table 3 funcref))
  (import "runtime" "memory" (memory 1))
  (import "runtime" "print" (func $print (type 1)))
  (import "runtime" "input" (func $input (type 0)))
  (import "runtime" "equal" (func $equal (type 2)))
  (import "runtime" "safe_add" (func $safe_add (type 2)))
  (import "runtime" "safe_sub" (func $safe_sub (type 2)))
  (import "runtime" "safe_mul" (func $safe_mul (type 2)))
  (import "runtime" "error" (func $error (type 2)))
  (global $r15 (mut i32) (i32.const +16))
  (type (func (result i64)))
  (type (func (param i64) (result i64)))
  (type (func (param i64 i64) (result i64)))
  (type (func (param i64 i64 i64) (result i64)))
  (elem (i32.const 0) 7 8 9 10 11 12 13)

  (func (type 2)
    (local)
    local.get 1
    call $print
  )
  (func (type 1)
    (local)
    call $input
  )
  (func (type 3)
    (local)
    local.get 1
    local.get 2
    call $equal
  )
  (func (export "our_code_starts_here") (type 0)
    (local i64 i64 i64)
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 0
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 1
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 2
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    local.get 0
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +2
    i64.store offset=0
    local.get 0
    i32.wrap_i64
    i64.const +0
    i64.store offset=3
    local.get 0
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 0
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    local.get 1
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +0
    i64.store offset=0
    local.get 1
    i32.wrap_i64
    i64.const +2
    i64.store offset=3
    local.get 1
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 1
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    local.get 2
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +4
    i64.store offset=0
    local.get 2
    i32.wrap_i64
    i64.const +4
    i64.store offset=3
    local.get 2
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 2
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    i64.const +246
  )
  (func $load_num (type 2)
    (local)
    i64.const 0x1
    local.get 0
    i64.and
    i64.const 0x0
    i64.sub
    i32.wrap_i64
    (if
      (then
        local.get 0
        local.get 1
        call $error
        drop
      )
    )
    local.get 0
  )
  (func $load_bool (type 2)
    (local)
    i64.const 0xf
    local.get 0
    i64.and
    i64.const 0xf
    i64.sub
    i32.wrap_i64
    (if
      (then
        local.get 0
        local.get 1
        call $error
        drop
      )
    )
    local.get 0
  )
  (func $load_tuple (type 2)
    (local)
    i64.const 0x7
    local.get 0
    i64.and
    i64.const 0x1
    i64.sub
    i32.wrap_i64
    (if
      (then
        local.get 0
        local.get 1
        call $error
        drop
      )
    )
    local.get 0
    i64.const 0x1
    i64.sub
  )
  (func $load_closure (type 2)
    (local)
    i64.const 0x7
    local.get 0
    i64.and
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    (if
      (then
        local.get 0
        local.get 1
        call $error
        drop
      )
    )
    local.get 0
    i64.const 0x5
    i64.sub
  )
)
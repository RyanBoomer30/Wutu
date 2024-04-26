(module
  (import "runtime" "table" (table 10 funcref))
  (import "runtime" "memory" (memory 1))
  (import "runtime" "print" (func $print (type 1)))
  (import "runtime" "input" (func $input (type 0)))
  (import "runtime" "add_trigger" (func $add_trigger (type 1)))
  (import "runtime" "equal" (func $equal (type 2)))
  (import "runtime" "alert_val" (func $alert_val (type 1)))
  (import "runtime" "display_val" (func $display_val (type 1)))
  (import "runtime" "big_bang" (func $big_bang (type 5)))
  (import "runtime" "safe_add" (func $safe_add (type 2)))
  (import "runtime" "safe_sub" (func $safe_sub (type 2)))
  (import "runtime" "safe_mul" (func $safe_mul (type 2)))
  (import "runtime" "error" (func $error (type 2)))
  (global $r15 (mut i32) (i32.const +16))
  (type (func (result i64)))
  (type (func (param i64) (result i64)))
  (type (func (param i64 i64) (result i64)))
  (type (func (param i64 i64 i64) (result i64)))
  (type (func (param i64 i64 i64 i64) (result i64)))
  (type (func (param i64 i64 i64 i64 i64) (result i64)))
  (type (func (param i64 i64 i64 i64 i64 i64) (result i64)))
  (elem (i32.const 0) 11 12 13 14 15 16 17 18 19 20 21)

  (func (type 2)
    (local)
    local.get 1
    call $print
  )
  (func (type 1)
    (local)
    call $input
  )
  (func (type 2)
    (local)
    local.get 1
    call $add_trigger
  )
  (func (type 3)
    (local)
    local.get 1
    local.get 2
    call $equal
  )
  (func (type 2)
    (local)
    local.get 1
    call $alert_val
  )
  (func (type 2)
    (local)
    local.get 1
    call $display_val
  )
  (func (type 6)
    (local)
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    local.get 5
    call $big_bang
  )
  (func (type 2)
    (local)
    local.get 1
    i64.const +2
    call $safe_add
  )
  (func (type 2)
    (local)
    local.get 1
  )
  (func (type 2)
    (local)
    local.get 1
    i64.const +1
    call $load_num
    i64.const +20
    i64.const +1
    call $load_num
    i64.gt_s
    (if (result i64)
      (then
            i64.const 0xffffffffffffffff
      )
      (else
            i64.const 0x7fffffffffffffff
      )
    )
  )
  (func (export "our_code_starts_here") (type 0)
    (local i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
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
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 3
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 4
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 5
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    local.set 6
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
    i64.const +2
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
    local.get 3
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +4
    i64.store offset=0
    local.get 3
    i32.wrap_i64
    i64.const +6
    i64.store offset=3
    local.get 3
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 3
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    local.get 4
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +2
    i64.store offset=0
    local.get 4
    i32.wrap_i64
    i64.const +8
    i64.store offset=3
    local.get 4
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 4
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    local.get 5
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +2
    i64.store offset=0
    local.get 5
    i32.wrap_i64
    i64.const +10
    i64.store offset=3
    local.get 5
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 5
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    local.get 6
    i64.const 0x5
    i64.sub
    i32.wrap_i64
    i64.const +10
    i64.store offset=0
    local.get 6
    i32.wrap_i64
    i64.const +12
    i64.store offset=3
    local.get 6
    i32.wrap_i64
    i64.const +0
    i64.store offset=11
    local.get 6
    i32.wrap_i64
    i64.const +0
    i64.store offset=24
    global.get $r15
    i64.const +2
    i64.store offset=0
    global.get $r15
    i64.const +14
    i64.store offset=8
    global.get $r15
    i64.const +0
    i64.store offset=16
    global.get $r15
    i64.const +0
    i64.store offset=24
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    local.set 7
    global.get $r15
    i64.const +2
    i64.store offset=0
    global.get $r15
    i64.const +16
    i64.store offset=8
    global.get $r15
    i64.const +0
    i64.store offset=16
    global.get $r15
    i64.const +0
    i64.store offset=24
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    local.set 8
    global.get $r15
    i64.const +2
    i64.store offset=0
    global.get $r15
    i64.const +18
    i64.store offset=8
    global.get $r15
    i64.const +0
    i64.store offset=16
    global.get $r15
    i64.const +0
    i64.store offset=24
    global.get $r15
    i64.extend_i32_s
    i64.const 0x5
    i64.add
    global.get $r15
    i32.const +32
    i32.add
    global.set $r15
    local.set 9
    local.get 6
    i64.const +16
    call $load_closure
    i32.wrap_i64
    i64.load offset=0
    i64.const +10
    i64.ne
    (if
      (then
        local.get 6
        i64.const +17
        call $error
        drop
      )
    )
    local.get 6
    local.get 7
    local.get 8
    i64.const +0
    i64.const +1000
    local.get 9
    local.get 6
    i32.wrap_i64
    i64.load offset=3
    i64.const +1
    i64.shr_s
    i32.wrap_i64
    call_indirect (param i64 i64 i64 i64 i64 i64) (result i64)
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
(module
  (memory (import "runtime" "memory") 1)
  (global $r15 (mut i32) (i32.const +0))

  (func (export "our_code_starts_here") (result i64)
    global.get $r15
    i64.const +8
    i64.store offset=0

    global.get $r15
    i64.const +2
    i64.store offset=8

    global.get $r15
    i64.const +4
    i64.store offset=16

    global.get $r15
    i64.const +6
    i64.store offset=24

    global.get $r15
    i64.const +8
    i64.store offset=32

    global.get $r15
    i64.const +0
    i64.store offset=40

    global.get $r15
    i64.extend_i32_s
    i64.const 0x1
    i64.add

    global.get $r15
    i32.const +48
    i32.add
    global.set $r15
  )
)
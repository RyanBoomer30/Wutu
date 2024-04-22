(module
  (memory (import "js" "mem") 10)
  (global $r15 (mut i32) (i32.const 0))

  (func (export "exported_func") (result i64)
    (local $ret i64)
    global.get $r15
    i32.const +3
    i32.add
    i64.extend_i32_s
    local.set $ret

    global.get $r15
    i64.const +2
    i64.store offset=0

    global.get $r15
    i64.const +2
    i64.store offset=8
    
    global.get $r15
    i64.const +4
    i64.store offset=16

    global.get $r15
    i64.const +0
    i64.store offset=24

    global.get $r15
    i32.const +32
    i32.add
    global.set $r15

    local.get $ret
  )
)
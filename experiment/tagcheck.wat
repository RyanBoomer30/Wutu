(module  
  (func $error (import "imports" "error") (param i64) (param i64))
  
  (func $tag_check_three (param $x i64) (result i64)
    i64.const +0x0000000000000007
    local.get $x
    i64.and
    i64.const +3
    i64.sub
    i32.wrap_i64
    ;; NOTE: the way to compile actual if is with specifying param and result like functions
    (if
      (then
        local.get $x
        i64.const +4 ;; error code
        call $error
      )
    )
    ;; untag and return it
    local.get $x
    i64.const +3
    i64.sub
  )

  (func (export "exported_func") (result i64)
    i64.const +12
    call $tag_check_three
  )
)
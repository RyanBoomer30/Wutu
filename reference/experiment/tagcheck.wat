(module  
  (func $error (import "imports" "error") (param i64) (param i64))
  
  ;; NOTE: Could be inline but basically just an implementation detail
  (func $tag_check_three (param $x i64) (param $ec i64) (result i64)
    i64.const +0x0000000000000007  ;; mask
    local.get $x
    i64.and
    i64.const +3  ;; tag
    i64.sub
    i32.wrap_i64
    ;; NOTE: the way to compile actual if is with specifying param and result like functions
    (if
      (then
        local.get $ec ;; error code
        local.get $x
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
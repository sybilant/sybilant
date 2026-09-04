bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object_a:
    dq SYBILANT_BOOLEAN_TYPE
object_b:
    dq SYBILANT_BOOLEAN_TYPE

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8
    lea rdi, [rel object_a]
    lea rsi, [rel object_b]
    call sybilant_S_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "distinct values of a heap type without defined equality should compare unequal by identity"
    ret

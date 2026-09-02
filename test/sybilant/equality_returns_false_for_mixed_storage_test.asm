bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_D_e

testcase:
    sub rsp, 8
    mov edi, SYBILANT_FALSE
    lea rsi, [rel object]
    call sybilant_D_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant-= should return false for immediate and heap values"
    ret

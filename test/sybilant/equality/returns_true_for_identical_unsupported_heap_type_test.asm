bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_BOOLEAN_TYPE

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8
    lea rdi, [rel object]
    mov rsi, rdi
    call sybilant_S_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant/= should return true for identical values of a heap type without equality support"
    ret

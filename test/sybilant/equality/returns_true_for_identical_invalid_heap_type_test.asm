bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
invalid_object:
    dq SYBILANT_FALSE

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8
    lea rdi, [rel invalid_object]
    mov rsi, rdi
    call sybilant_S_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant/= should return true for identical values without validating an invalid heap type"
    ret

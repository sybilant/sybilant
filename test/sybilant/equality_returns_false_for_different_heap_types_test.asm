bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
left:
    dq SYBILANT_BOOLEAN_TYPE
right:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_D_e

testcase:
    sub rsp, 8
    lea rdi, [rel left]
    lea rsi, [rel right]
    call sybilant_D_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant-= should return false for different heap types"
    ret

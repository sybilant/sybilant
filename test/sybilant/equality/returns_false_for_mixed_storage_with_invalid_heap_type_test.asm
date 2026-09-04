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
    mov edi, SYBILANT_TRUE
    lea rsi, [rel invalid_object]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/= should return false for an immediate and an invalid heap type without validating it"

    lea rdi, [rel invalid_object]
    mov esi, SYBILANT_TRUE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/= should return false for an invalid heap type and an immediate without validating it"

    add rsp, 8
    ret

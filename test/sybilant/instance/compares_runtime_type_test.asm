bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_Dinstance_q
extern sybilant_Dinstance_q_Dunchecked

testcase:
    sub rsp, 8

    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-instance? should return true for a matching type"

    lea rdi, [rel object]
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant-instance? should return false for a different type"

    mov edi, SYBILANT_TRUE
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q_Dunchecked
    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked sybilant-instance? should accept proven arguments"

    add rsp, 8
    ret

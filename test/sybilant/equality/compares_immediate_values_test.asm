bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_FALSE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant/= should return true for equal booleans"

    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_TRUE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/= should return false for different booleans"

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant/= should return true for equal extended immediates"

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TYPE_TYPE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/= should return false for different extended immediates"

    mov edi, SYBILANT_NIL
    mov esi, SYBILANT_NIL
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant/= should return true for nil and nil"

    mov edi, SYBILANT_NIL
    mov esi, SYBILANT_FALSE
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/= should return false for different immediate kinds"

    add rsp, 8
    ret

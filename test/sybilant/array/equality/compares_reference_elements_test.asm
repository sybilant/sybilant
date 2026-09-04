bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
string_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_STRING_TYPE
    dd 8
    dd 0

align 8
alpha:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 5
    db "alpha"
align 8
alpha_copy:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 5
    db "alpha"
align 8
beta:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 4
    db "beta"

align 8
reference_array:
    dq string_array_type
    dq SYBILANT_NIL
    dq 2
    dq alpha, beta
align 8
reference_array_equal:
    dq string_array_type
    dq SYBILANT_NIL
    dq 2
    dq alpha_copy, beta
align 8
reference_array_different:
    dq string_array_type
    dq SYBILANT_NIL
    dq 2
    dq alpha, alpha

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    lea rdi, [rel reference_array]
    lea rsi, [rel reference_array_equal]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "arrays of strings with equal but distinct elements should be equal by recursion"

    lea rdi, [rel reference_array]
    lea rsi, [rel reference_array_different]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "arrays of strings with a differing element should not be equal"

    add rsp, 8
    ret

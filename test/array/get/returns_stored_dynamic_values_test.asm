bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
boolean_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
    dd 8
    dd 0
boolean_array:
    dq boolean_array_type
    dq SYBILANT_NIL
    dq 2
    dq SYBILANT_FALSE, SYBILANT_TRUE
atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
atom:
    dq atom_type
    dq SYBILANT_TRUE
atom_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq atom_type
    dd 8
    dd 0
atom_array:
    dq atom_array_type
    dq SYBILANT_NIL
    dq 1
    dq atom

section .text
extern sybilant_Sarray_Dget

testcase:
    sub rsp, 8

    lea rdi, [rel boolean_array]
    mov esi, 1
    call sybilant_Sarray_Dget
    ASSERT_EQ rax, SYBILANT_TRUE, "array-get should return an already boxed immediate value"

    lea rdi, [rel atom_array]
    xor esi, esi
    call sybilant_Sarray_Dget
    lea rdx, [rel atom]
    ASSERT_EQ rax, rdx, "array-get should return an already boxed heap value"

    add rsp, 8
    ret

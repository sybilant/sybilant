bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
boolean_atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
align 8
atom_a:
    dq boolean_atom_type
    dq SYBILANT_TRUE
align 8
atom_b:
    dq boolean_atom_type
    dq SYBILANT_TRUE

section .text
extern sybilant_datom_S_e

testcase:
    sub rsp, 8

    lea rdi, [rel atom_a]
    mov rsi, rdi
    call sybilant_datom_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant.atom/= should report an atom equal to itself"

    lea rdi, [rel atom_a]
    lea rsi, [rel atom_b]
    call sybilant_datom_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant.atom/= should report distinct atoms as unequal"

    add rsp, 8

    ASSERT_EXIT .invalid_left, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.atom/= should reject a non-atom left argument"
    ASSERT_EXIT .invalid_right, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.atom/= should reject a non-atom right argument"
    ret

.invalid_left:
    mov edi, SYBILANT_TRUE
    lea rsi, [rel atom_a]
    jmp sybilant_datom_S_e

.invalid_right:
    lea rdi, [rel atom_a]
    mov esi, SYBILANT_TRUE
    jmp sybilant_datom_S_e

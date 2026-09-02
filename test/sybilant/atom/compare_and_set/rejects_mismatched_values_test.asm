bits 64
default rel

%include "test/support.asm"

section .data
align 8
atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
atom:
    dq atom_type
    dq SYBILANT_FALSE

section .text
extern sybilant_Satom_Dcompare_Dand_Dset
extern sybilant_Satom_Dcompare_Dand_Dset_Dunchecked

testcase:
    ASSERT_EXIT .mismatched_old_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom-compare-and-set should reject an old value outside the element type"
    ASSERT_EXIT .mismatched_new_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom-compare-and-set should reject a new value outside the element type"

    sub rsp, 8
    lea rdi, [rel atom]
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_EXTENDED_TAG_NAT32
    call sybilant_Satom_Dcompare_Dand_Dset_Dunchecked
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked atom-compare-and-set should trust statically proven value types"
    ASSERT_EQ qword [rel atom + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_EXTENDED_TAG_NAT32, "unchecked atom-compare-and-set should store a statically proven value"
    ret

.mismatched_old_value:
    lea rdi, [rel atom]
    mov esi, SYBILANT_EXTENDED_TAG_NAT32
    mov edx, SYBILANT_TRUE
    jmp sybilant_Satom_Dcompare_Dand_Dset

.mismatched_new_value:
    lea rdi, [rel atom]
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_EXTENDED_TAG_NAT32
    jmp sybilant_Satom_Dcompare_Dand_Dset

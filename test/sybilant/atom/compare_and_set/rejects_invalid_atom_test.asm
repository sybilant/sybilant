bits 64
default rel

%include "test/support.asm"

section .data
align 8
not_atom:
    dq SYBILANT_UINT64_TYPE
    dq SYBILANT_FALSE
not_atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_BOOLEAN_TYPE
    dq SYBILANT_BOOLEAN_TYPE
not_atom_value:
    dq not_atom_type
    dq SYBILANT_FALSE

section .text
extern sybilant_datom_Scompare_Dand_Dset
extern sybilant_datom_Scompare_Dand_Dset_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/compare-and-set should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/compare-and-set should reject an immediate value"
    ASSERT_EXIT .other_heap_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/compare-and-set should reject another heap type"
    ASSERT_EXIT .other_parameterized_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/compare-and-set should reject another parameterized type"

    sub rsp, 8
    lea rdi, [rel not_atom]
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    call sybilant_datom_Scompare_Dand_Dset_Dunchecked
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked atom/compare-and-set should skip the atom type guard"
    ASSERT_EQ qword [rel not_atom + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_TRUE, "unchecked atom/compare-and-set should update a proven atom"
    ret

.nil:
    mov edi, SYBILANT_NIL
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    jmp sybilant_datom_Scompare_Dand_Dset

.immediate:
    mov edi, SYBILANT_TRUE
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    jmp sybilant_datom_Scompare_Dand_Dset

.other_heap_value:
    lea rdi, [rel not_atom]
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    jmp sybilant_datom_Scompare_Dand_Dset

.other_parameterized_value:
    lea rdi, [rel not_atom_value]
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    jmp sybilant_datom_Scompare_Dand_Dset

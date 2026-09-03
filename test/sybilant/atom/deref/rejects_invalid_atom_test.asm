bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
not_atom:
    dq SYBILANT_UINT64_TYPE
    dq SYBILANT_TRUE
not_atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_BOOLEAN_TYPE
    dq SYBILANT_BOOLEAN_TYPE
not_atom_value:
    dq not_atom_type
    dq SYBILANT_TRUE

section .text
extern sybilant_datom_Sderef
extern sybilant_datom_Sderef_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/deref should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/deref should reject an immediate value"
    ASSERT_EXIT .other_heap_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/deref should reject another heap type"
    ASSERT_EXIT .other_parameterized_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/deref should reject another parameterized type"

    sub rsp, 8
    lea rdi, [rel not_atom]
    call sybilant_datom_Sderef_Dunchecked
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked atom/deref should skip the atom type guard"
    ret

.nil:
    mov edi, SYBILANT_NIL
    jmp sybilant_datom_Sderef

.immediate:
    mov edi, SYBILANT_TRUE
    jmp sybilant_datom_Sderef

.other_heap_value:
    lea rdi, [rel not_atom]
    jmp sybilant_datom_Sderef

.other_parameterized_value:
    lea rdi, [rel not_atom_value]
    jmp sybilant_datom_Sderef

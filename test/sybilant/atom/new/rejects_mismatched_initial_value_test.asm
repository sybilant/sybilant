bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_datom_Snew
extern sybilant_datom_Snew_Dunchecked

testcase:
    ASSERT_EXIT .mismatched_initial_value, SYBILANT_ERROR_INVALID_ARGUMENT, "atom/new should reject an initial value outside its element type"

    sub rsp, 8
    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_EXTENDED_TAG_NAT32
    call sybilant_datom_Snew_Dunchecked
    add rsp, 8

    mov rdx, [rax + SYBILANT_ATOM_TYPE_OFFSET]
    ASSERT_EQ qword [rdx + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_BOOLEAN_TYPE, "unchecked atom/new should trust the proven element type"
    ASSERT_EQ qword [rax + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_EXTENDED_TAG_NAT32, "unchecked atom/new should trust the proven initial value"
    ret

.mismatched_initial_value:
    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_EXTENDED_TAG_NAT32
    jmp sybilant_datom_Snew

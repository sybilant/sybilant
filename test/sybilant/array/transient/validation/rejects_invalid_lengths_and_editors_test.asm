bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_darray_Stransient_Dnew_B

testcase:
    ASSERT_EXIT .invalid_length, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-new! should reject a non-integer length"
    ASSERT_EXIT .invalid_editor, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-new! should reject a non-atom editor"
    ret

.invalid_length:
    mov edi, SYBILANT_UINT8_TYPE
    mov esi, SYBILANT_TRUE
    mov edx, SYBILANT_NIL
    jmp sybilant_darray_Stransient_Dnew_B

.invalid_editor:
    mov edi, SYBILANT_UINT8_TYPE
    mov esi, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32
    mov edx, SYBILANT_TRUE
    jmp sybilant_darray_Stransient_Dnew_B

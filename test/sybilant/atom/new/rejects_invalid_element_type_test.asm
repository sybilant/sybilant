bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Satom_Dnew

testcase:
    ASSERT_EXIT .invalid_element_type, SYBILANT_ERROR_INVALID_ARGUMENT, "atom-new should reject a non-type element type"
    ret

.invalid_element_type:
    mov edi, SYBILANT_TRUE
    mov esi, SYBILANT_TRUE
    jmp sybilant_Satom_Dnew

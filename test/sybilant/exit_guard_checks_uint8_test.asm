bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dexit

testcase:
    ASSERT_EXIT .exit_255, 255, "sybilant-exit should accept the maximum uint8"
    ASSERT_EXIT .exit_256, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant-exit should reject a status larger than a uint8"
    ret

.exit_255:
    mov edi, 255
    jmp sybilant_Dexit

.exit_256:
    mov edi, 256
    jmp sybilant_Dexit

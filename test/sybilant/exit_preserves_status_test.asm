bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dexit

testcase:
    ASSERT_EXIT .exit_42, 42, "sybilant-exit should preserve the requested status"
    ret

.exit_42:
    mov edi, 42
    jmp sybilant_Dexit

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dmalloc

testcase:
    ASSERT_EXIT .malloc_zero, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant-malloc should reject a zero size"
    ret

.malloc_zero:
    xor edi, edi
    jmp sybilant_Dmalloc

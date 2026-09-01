bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_FALSE

section .text
extern sybilant_Dtype

testcase:
    ASSERT_EXIT .type, SYBILANT_ERROR_INVALID_STATE, "sybilant-type should reject a non-type pointer header"
    ret

.type:
    lea rdi, [rel object]
    jmp sybilant_Dtype

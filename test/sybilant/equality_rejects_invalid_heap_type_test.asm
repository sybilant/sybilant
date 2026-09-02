bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
invalid_object:
    dq SYBILANT_FALSE
valid_object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_D_e

testcase:
    ASSERT_EXIT .equal, SYBILANT_ERROR_INVALID_STATE, "sybilant-= should reject an invalid heap type"
    ret

.equal:
    lea rdi, [rel invalid_object]
    lea rsi, [rel valid_object]
    jmp sybilant_D_e

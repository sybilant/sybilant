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
extern sybilant_S_e

testcase:
    ASSERT_EXIT .invalid_left, SYBILANT_ERROR_INVALID_STATE, "sybilant/= should reject an invalid left heap type"
    ASSERT_EXIT .invalid_right, SYBILANT_ERROR_INVALID_STATE, "sybilant/= should reject an invalid right heap type"
    ret

.invalid_left:
    lea rdi, [rel invalid_object]
    lea rsi, [rel valid_object]
    jmp sybilant_S_e

.invalid_right:
    lea rdi, [rel valid_object]
    lea rsi, [rel invalid_object]
    jmp sybilant_S_e

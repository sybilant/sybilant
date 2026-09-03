bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
valid_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 1
    db 0x41
align 8
invalid_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 2
    db 0xc0, 0x80

section .text
extern sybilant_S_e

testcase:
    ASSERT_EXIT .invalid_left, SYBILANT_ERROR_INVALID_STATE, "string equality should reject invalid UTF-8 in the left value"
    ASSERT_EXIT .invalid_right, SYBILANT_ERROR_INVALID_STATE, "string equality should reject invalid UTF-8 in the right value"
    ret

.invalid_left:
    lea rdi, [rel invalid_string]
    lea rsi, [rel valid_string]
    jmp sybilant_S_e

.invalid_right:
    lea rdi, [rel valid_string]
    lea rsi, [rel invalid_string]
    jmp sybilant_S_e

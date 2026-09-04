bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
hello:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 5
    db "hello"
align 8
hello_copy:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 5
    db "hello"
align 8
world:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 5
    db "world"

section .text
extern sybilant_dstring_S_e

testcase:
    sub rsp, 8

    lea rdi, [rel hello]
    lea rsi, [rel hello_copy]
    call sybilant_dstring_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant.string/= should validate and compare equal strings"

    lea rdi, [rel hello]
    lea rsi, [rel world]
    call sybilant_dstring_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant.string/= should distinguish different strings"

    add rsp, 8

    ASSERT_EXIT .invalid_left, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.string/= should reject a non-string left argument"
    ASSERT_EXIT .invalid_right, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.string/= should reject a non-string right argument"
    ret

.invalid_left:
    mov edi, SYBILANT_TRUE
    lea rsi, [rel hello]
    jmp sybilant_dstring_S_e

.invalid_right:
    lea rdi, [rel hello]
    mov esi, SYBILANT_TRUE
    jmp sybilant_dstring_S_e

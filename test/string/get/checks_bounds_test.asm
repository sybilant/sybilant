bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 1
    db 0x41
align 8
empty_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_Sstring_Dget
extern sybilant_Sstring_Dget_Dunchecked

testcase:
    ASSERT_EXIT .dynamic_at_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get should report an index equal to the codepoint count as out of bounds"
    ASSERT_EXIT .dynamic_above_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get should report an index above the codepoint count as out of bounds"
    ASSERT_EXIT .dynamic_empty, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get should report every index into an empty string as out of bounds"
    ASSERT_EXIT .unchecked_at_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get-unchecked should report an index equal to the codepoint count as out of bounds"
    ASSERT_EXIT .unchecked_above_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get-unchecked should report an index above the codepoint count as out of bounds"
    ASSERT_EXIT .unchecked_empty, SYBILANT_ERROR_OUT_OF_BOUNDS, "string-get-unchecked should report every index into an empty string as out of bounds"
    ret

.dynamic_at_length:
    lea rdi, [rel string]
    mov esi, 1
    jmp sybilant_Sstring_Dget

.dynamic_above_length:
    lea rdi, [rel string]
    mov rsi, -1
    jmp sybilant_Sstring_Dget

.dynamic_empty:
    lea rdi, [rel empty_string]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.unchecked_at_length:
    lea rdi, [rel string]
    mov esi, 1
    jmp sybilant_Sstring_Dget_Dunchecked

.unchecked_above_length:
    lea rdi, [rel string]
    mov rsi, -1
    jmp sybilant_Sstring_Dget_Dunchecked

.unchecked_empty:
    lea rdi, [rel empty_string]
    xor esi, esi
    jmp sybilant_Sstring_Dget_Dunchecked

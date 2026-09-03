bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
unexpected_continuation:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 1
    db 0x80
align 8
truncated_sequence:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 2
    db 0xe2, 0x82, 0xac
align 8
invalid_continuation:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 3
    db 0xe2, 0x28, 0xa1
align 8
overlong_encoding:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 2
    db 0xc0, 0x80
align 8
surrogate_encoding:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 3
    db 0xed, 0xa0, 0x80
align 8
codepoint_above_maximum:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 4
    db 0xf4, 0x90, 0x80, 0x80

section .text
extern sybilant_Sstring_Dget
extern sybilant_Sstring_Dget_Dunchecked

testcase:
    ASSERT_EXIT .dynamic_unexpected_continuation, SYBILANT_ERROR_INVALID_STATE, "string-get should reject an unexpected UTF-8 continuation byte"
    ASSERT_EXIT .dynamic_truncated_sequence, SYBILANT_ERROR_INVALID_STATE, "string-get should reject a UTF-8 sequence truncated at the byte length"
    ASSERT_EXIT .dynamic_invalid_continuation, SYBILANT_ERROR_INVALID_STATE, "string-get should reject an invalid UTF-8 continuation byte"
    ASSERT_EXIT .dynamic_overlong_encoding, SYBILANT_ERROR_INVALID_STATE, "string-get should reject an overlong UTF-8 encoding"
    ASSERT_EXIT .dynamic_surrogate_encoding, SYBILANT_ERROR_INVALID_STATE, "string-get should reject a UTF-8 encoding of a surrogate"
    ASSERT_EXIT .dynamic_codepoint_above_maximum, SYBILANT_ERROR_INVALID_STATE, "string-get should reject a codepoint above U+10FFFF"
    ASSERT_EXIT .unchecked_invalid_encoding, SYBILANT_ERROR_INVALID_STATE, "string-get-unchecked should retain UTF-8 validation"
    ret

.dynamic_unexpected_continuation:
    lea rdi, [rel unexpected_continuation]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.dynamic_truncated_sequence:
    lea rdi, [rel truncated_sequence]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.dynamic_invalid_continuation:
    lea rdi, [rel invalid_continuation]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.dynamic_overlong_encoding:
    lea rdi, [rel overlong_encoding]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.dynamic_surrogate_encoding:
    lea rdi, [rel surrogate_encoding]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.dynamic_codepoint_above_maximum:
    lea rdi, [rel codepoint_above_maximum]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.unchecked_invalid_encoding:
    lea rdi, [rel invalid_continuation]
    xor esi, esi
    jmp sybilant_Sstring_Dget_Dunchecked

bits 64
default rel

%include "test/support.asm"

%macro ASSERT_STRING_EQUAL 4
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call sybilant_S_e
    ASSERT_EQ rax, %3, %4
%endmacro

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 11
    db 0x41, 0x00
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x8d, 0x88
align 8
equal_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 11
    db 0x41, 0x00
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x8d, 0x88
align 8
different_codepoint_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 11
    db 0x41, 0x00
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x8d, 0x89
align 8
same_count_shorter_bytes:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 8
    db 0x41, 0x00
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0x78
align 8
shorter_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 7
    db 0x41, 0x00
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
align 8
empty_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 0
equal_empty_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    ASSERT_STRING_EQUAL string, equal_string, SYBILANT_TRUE, "distinct strings with equal codepoints should be equal"
    ASSERT_STRING_EQUAL string, different_codepoint_string, SYBILANT_FALSE, "strings with different codepoints should not be equal"
    ASSERT_STRING_EQUAL string, same_count_shorter_bytes, SYBILANT_FALSE, "strings with equal codepoint counts and different contents should not be equal"
    ASSERT_STRING_EQUAL string, shorter_string, SYBILANT_FALSE, "strings with different codepoint counts should not be equal"
    ASSERT_STRING_EQUAL empty_string, equal_empty_string, SYBILANT_TRUE, "distinct empty strings should be equal"

    add rsp, 8
    ret

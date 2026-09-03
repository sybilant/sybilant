bits 64
default rel

%include "test/support.asm"

%macro ASSERT_STRING_GET 3
    lea rdi, [rel string]
    mov esi, %1
    call sybilant_Sstring_Dget

    mov edx, %2
    shl rdx, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdx, SYBILANT_EXTENDED_TAG_CODEPOINT
    ASSERT_EQ rax, rdx, %3
%endmacro

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 20
    db 0x00
    db 0x7f
    db 0xc2, 0x80
    db 0xdf, 0xbf
    db 0xe0, 0xa0, 0x80
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x80, 0x80
    db 0xf4, 0x8f, 0xbf, 0xbf

section .text
extern sybilant_Sstring_Dget

testcase:
    sub rsp, 8

    ASSERT_STRING_GET 0, 0x000000, "string-get should box U+0000"
    ASSERT_STRING_GET 1, 0x00007f, "string-get should box the largest one-byte codepoint"
    ASSERT_STRING_GET 2, 0x000080, "string-get should box the smallest two-byte codepoint"
    ASSERT_STRING_GET 3, 0x0007ff, "string-get should box the largest two-byte codepoint"
    ASSERT_STRING_GET 4, 0x000800, "string-get should box the smallest three-byte codepoint"
    ASSERT_STRING_GET 5, 0x0020ac, "string-get should box a three-byte codepoint"
    ASSERT_STRING_GET 6, 0x010000, "string-get should box the smallest four-byte codepoint"
    ASSERT_STRING_GET 7, 0x10ffff, "string-get should box the largest Unicode codepoint"

    add rsp, 8
    ret

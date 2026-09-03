bits 64
default rel

%include "test/support.asm"

%macro ASSERT_STRING_GET_UNCHECKED 3
    mov rax, -1
    lea rdi, [rel string]
    mov esi, %1
    call sybilant_dstring_Sget_Dunchecked
    ASSERT_EQ eax, %2, %3
    ASSERT_EQ rax, %2, "string/get-unchecked should return the codepoint in eax"
%endmacro

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 10
    db 0x41
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x8d, 0x88

section .text
extern sybilant_dstring_Sget_Dunchecked

testcase:
    sub rsp, 8

    ASSERT_STRING_GET_UNCHECKED 0, 0x000041, "string/get-unchecked should decode a one-byte codepoint"
    ASSERT_STRING_GET_UNCHECKED 1, 0x0000a2, "string/get-unchecked should decode a two-byte codepoint"
    ASSERT_STRING_GET_UNCHECKED 2, 0x0020ac, "string/get-unchecked should decode a three-byte codepoint"
    ASSERT_STRING_GET_UNCHECKED 3, 0x010348, "string/get-unchecked should decode a four-byte codepoint"

    add rsp, 8
    ret

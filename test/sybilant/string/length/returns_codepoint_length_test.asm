bits 64
default rel

%include "test/support.asm"

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
align 8
empty_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_dstring_Slength
extern sybilant_dstring_Slength_Dunchecked

testcase:
    sub rsp, 8

    lea rdi, [rel string]
    call sybilant_dstring_Slength
    ASSERT_EQ rax, 4, "string/length should return the codepoint count instead of the byte count"

    lea rdi, [rel string]
    call sybilant_dstring_Slength_Dunchecked
    ASSERT_EQ rax, 4, "string/length-unchecked should return the codepoint count"

    lea rdi, [rel empty_string]
    call sybilant_dstring_Slength
    ASSERT_EQ rax, 0, "string/length should return zero for an empty string"

    add rsp, 8
    ret

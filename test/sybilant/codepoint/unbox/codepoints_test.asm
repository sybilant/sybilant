bits 64
default rel

%include "test/support.asm"

%macro ASSERT_UNBOX_CODEPOINT 2
    mov edi, %1
    shl rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdi, SYBILANT_EXTENDED_TAG_CODEPOINT
    call sybilant_Sunbox_Dcodepoint

    mov edx, %1
    ASSERT_EQ rax, rdx, %2
%endmacro

section .text
extern sybilant_Sunbox_Dcodepoint

testcase:
    sub rsp, 8

    ASSERT_UNBOX_CODEPOINT 0, "unbox-codepoint should return U+0000"
    ASSERT_UNBOX_CODEPOINT 0x1f642, "unbox-codepoint should zero-extend a supplementary-plane codepoint"
    ASSERT_UNBOX_CODEPOINT SYBILANT_CODEPOINT_MAX, "unbox-codepoint should return the maximum Unicode codepoint"

    add rsp, 8
    ret

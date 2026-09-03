bits 64
default rel

%include "test/support.asm"

%macro ASSERT_BOX_CODEPOINT 2
    mov edi, %1
    call sybilant_Sbox_Dcodepoint

    mov edx, %1
    and edx, SYBILANT_CODEPOINT_PAYLOAD_MASK
    shl rdx, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdx, SYBILANT_EXTENDED_TAG_CODEPOINT
    ASSERT_EQ rax, rdx, %2
%endmacro

section .text
extern sybilant_Sbox_Dcodepoint

testcase:
    sub rsp, 8

    ASSERT_BOX_CODEPOINT 0, "box-codepoint should box U+0000 with the codepoint tag"
    ASSERT_BOX_CODEPOINT 0x1f642, "box-codepoint should preserve a supplementary-plane codepoint"
    ASSERT_BOX_CODEPOINT SYBILANT_CODEPOINT_MAX, "box-codepoint should preserve the maximum Unicode codepoint"

    add rsp, 8
    ret

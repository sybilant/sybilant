bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Sunbox_Dcodepoint_Dunchecked

testcase:
    sub rsp, 8

    mov edi, 0x1f642
    shl rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdi, SYBILANT_EXTENDED_TAG_UINT32
    call sybilant_Sunbox_Dcodepoint_Dunchecked
    ASSERT_EQ rax, 0x1f642, "unbox-codepoint-unchecked should ignore the extended tag"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Stype
extern sybilant_Stype_Dunchecked

testcase:
    sub rsp, 8

    mov edi, 0x1f642
    shl rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdi, SYBILANT_EXTENDED_TAG_CODEPOINT
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_CODEPOINT_TYPE, "sybilant/type should recognize a boxed codepoint"

    mov edi, 0x1f642
    shl rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdi, SYBILANT_EXTENDED_TAG_CODEPOINT
    call sybilant_Stype_Dunchecked
    ASSERT_EQ rax, SYBILANT_CODEPOINT_TYPE, "sybilant/type-unchecked should recognize a boxed codepoint"

    add rsp, 8
    ret

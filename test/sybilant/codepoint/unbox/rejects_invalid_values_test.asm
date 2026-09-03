bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Sunbox_Dcodepoint

testcase:
    ASSERT_EXIT .uint32, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-codepoint should reject another extended immediate"
    ASSERT_EXIT .boolean, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-codepoint should reject a boolean"
    ret

.uint32:
    mov edi, 0x41
    shl rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rdi, SYBILANT_EXTENDED_TAG_UINT32
    jmp sybilant_Sunbox_Dcodepoint

.boolean:
    mov edi, SYBILANT_TRUE
    jmp sybilant_Sunbox_Dcodepoint

bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
invalid_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 4
    db 0x41, 0x42, 0xe2, 0x82, 0xac

section .text
extern sybilant_dstring_Slength
extern sybilant_dstring_Slength_Dunchecked

testcase:
    ASSERT_EXIT .dynamic, SYBILANT_ERROR_INVALID_STATE, "string/length should reject UTF-8 truncated at the byte length"
    ASSERT_EXIT .unchecked, SYBILANT_ERROR_INVALID_STATE, "string/length-unchecked should retain UTF-8 validation"
    ret

.dynamic:
    lea rdi, [rel invalid_string]
    jmp sybilant_dstring_Slength

.unchecked:
    lea rdi, [rel invalid_string]
    jmp sybilant_dstring_Slength_Dunchecked

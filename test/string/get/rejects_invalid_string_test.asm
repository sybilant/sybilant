bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
other_heap_value:
    dq SYBILANT_UINT64_TYPE
    dq 37
invalid_string:
    dq SYBILANT_FALSE
    dq SYBILANT_NIL
    dq 1
    db 0x41

section .text
extern sybilant_Sstring_Dget
extern sybilant_Sstring_Dget_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "string-get should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "string-get should reject an immediate value"
    ASSERT_EXIT .other_heap, SYBILANT_ERROR_INVALID_ARGUMENT, "string-get should reject another heap type"
    ASSERT_EXIT .invalid_type, SYBILANT_ERROR_INVALID_ARGUMENT, "string-get should reject an invalid string type"

    sub rsp, 8
    lea rdi, [rel invalid_string]
    xor esi, esi
    call sybilant_Sstring_Dget_Dunchecked
    add rsp, 8
    ASSERT_EQ eax, 0x41, "string-get-unchecked should skip the string type guard"
    ret

.nil:
    mov edi, SYBILANT_NIL
    xor esi, esi
    jmp sybilant_Sstring_Dget

.immediate:
    mov edi, SYBILANT_TRUE
    xor esi, esi
    jmp sybilant_Sstring_Dget

.other_heap:
    lea rdi, [rel other_heap_value]
    xor esi, esi
    jmp sybilant_Sstring_Dget

.invalid_type:
    lea rdi, [rel invalid_string]
    xor esi, esi
    jmp sybilant_Sstring_Dget

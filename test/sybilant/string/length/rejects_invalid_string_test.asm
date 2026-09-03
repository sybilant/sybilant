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
extern sybilant_dstring_Slength
extern sybilant_dstring_Slength_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "string/length should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "string/length should reject an immediate value"
    ASSERT_EXIT .other_heap, SYBILANT_ERROR_INVALID_ARGUMENT, "string/length should reject another heap type"
    ASSERT_EXIT .invalid_type, SYBILANT_ERROR_INVALID_ARGUMENT, "string/length should reject an invalid string type"

    sub rsp, 8
    lea rdi, [rel invalid_string]
    call sybilant_dstring_Slength_Dunchecked
    add rsp, 8
    ASSERT_EQ rax, 1, "string/length-unchecked should skip the string type guard"
    ret

.nil:
    mov edi, SYBILANT_NIL
    jmp sybilant_dstring_Slength

.immediate:
    mov edi, SYBILANT_TRUE
    jmp sybilant_dstring_Slength

.other_heap:
    lea rdi, [rel other_heap_value]
    jmp sybilant_dstring_Slength

.invalid_type:
    lea rdi, [rel invalid_string]
    jmp sybilant_dstring_Slength

bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
atom_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
atom:
    dq atom_type
    dq SYBILANT_TRUE
other_heap_value:
    dq SYBILANT_UINT64_TYPE
    dq 37
    dq 41
invalid_array_type:
    dq SYBILANT_FALSE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
invalid_array:
    dq invalid_array_type
    dq SYBILANT_NIL
    dq 43
reserved_flags_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 1
reserved_flags_array:
    dq reserved_flags_array_type
    dq SYBILANT_NIL
    dq 47

section .text
extern sybilant_Sarray_Dlength
extern sybilant_Sarray_Dlength_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject an immediate value"
    ASSERT_EXIT .other_heap, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject another heap type"
    ASSERT_EXIT .other_parameterized, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject another parameterized type"
    ASSERT_EXIT .invalid_type, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject an invalid array type"
    ASSERT_EXIT .reserved_layout_flags, SYBILANT_ERROR_INVALID_ARGUMENT, "array-length should reject reserved layout flags"

    sub rsp, 8
    lea rdi, [rel invalid_array]
    call sybilant_Sarray_Dlength_Dunchecked
    add rsp, 8
    ASSERT_EQ rax, 43, "array-length-unchecked should skip the array type guard"
    ret

.nil:
    mov edi, SYBILANT_NIL
    jmp sybilant_Sarray_Dlength

.immediate:
    mov edi, SYBILANT_TRUE
    jmp sybilant_Sarray_Dlength

.other_heap:
    lea rdi, [rel other_heap_value]
    jmp sybilant_Sarray_Dlength

.other_parameterized:
    lea rdi, [rel atom]
    jmp sybilant_Sarray_Dlength

.invalid_type:
    lea rdi, [rel invalid_array]
    jmp sybilant_Sarray_Dlength

.reserved_layout_flags:
    lea rdi, [rel reserved_flags_array]
    jmp sybilant_Sarray_Dlength

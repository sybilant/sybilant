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
invalid_array_type:
    dq SYBILANT_FALSE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
invalid_array:
    dq invalid_array_type
    dq SYBILANT_NIL
    dq 1
    db 0x7a
align 8
invalid_element_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_TRUE
    dd 8
    dd 0
invalid_element_array:
    dq invalid_element_array_type
    dq SYBILANT_NIL
    dq 1
    dq SYBILANT_TRUE
reserved_flags_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 1
reserved_flags_array:
    dq reserved_flags_array_type
    dq SYBILANT_NIL
    dq 1
    db 0x7a

section .text
extern sybilant_Sarray_Dget
extern sybilant_Sarray_Dget_Dunchecked

testcase:
    ASSERT_EXIT .nil, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject nil"
    ASSERT_EXIT .immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject an immediate value"
    ASSERT_EXIT .other_heap, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject another heap type"
    ASSERT_EXIT .other_parameterized, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject another parameterized type"
    ASSERT_EXIT .invalid_type, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject an invalid array type"
    ASSERT_EXIT .invalid_element_type, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject a non-type element type"
    ASSERT_EXIT .reserved_layout_flags, SYBILANT_ERROR_INVALID_ARGUMENT, "array-get should reject reserved layout flags"

    sub rsp, 8
    lea rdi, [rel invalid_array]
    xor esi, esi
    call sybilant_Sarray_Dget_Dunchecked
    add rsp, 8
    ASSERT_EQ al, 0x7a, "array-get-unchecked should skip the array type guard"
    ret

.nil:
    mov edi, SYBILANT_NIL
    xor esi, esi
    jmp sybilant_Sarray_Dget

.immediate:
    mov edi, SYBILANT_TRUE
    xor esi, esi
    jmp sybilant_Sarray_Dget

.other_heap:
    lea rdi, [rel other_heap_value]
    xor esi, esi
    jmp sybilant_Sarray_Dget

.other_parameterized:
    lea rdi, [rel atom]
    xor esi, esi
    jmp sybilant_Sarray_Dget

.invalid_type:
    lea rdi, [rel invalid_array]
    xor esi, esi
    jmp sybilant_Sarray_Dget

.invalid_element_type:
    lea rdi, [rel invalid_element_array]
    xor esi, esi
    jmp sybilant_Sarray_Dget

.reserved_layout_flags:
    lea rdi, [rel reserved_flags_array]
    xor esi, esi
    jmp sybilant_Sarray_Dget

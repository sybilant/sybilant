bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
array:
    dq array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56

section .text

testcase:
    lea rax, [rel array_type]
    ASSERT_EQ qword [rel array + SYBILANT_ARRAY_TYPE_OFFSET], rax, "an array should reference its parameterized type"
    ASSERT_EQ qword [rel array + SYBILANT_ARRAY_EDITOR_OFFSET], SYBILANT_NIL, "an immutable array should have a null editor"
    ASSERT_EQ qword [rel array + SYBILANT_ARRAY_LENGTH_OFFSET], 3, "an array should store its length"
    ASSERT_EQ byte [rel array + SYBILANT_ARRAY_DATA_OFFSET], 0x12, "array data should follow the header"
    ASSERT_EQ byte [rel array + SYBILANT_ARRAY_DATA_OFFSET + 1], 0x34, "uint8 array elements should use one byte each"
    ASSERT_EQ byte [rel array + SYBILANT_ARRAY_DATA_OFFSET + 2], 0x56, "packed array data should preserve adjacent elements"

    ASSERT_EQ qword [rel array_type + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE, "an array type should itself be a type"
    ASSERT_EQ qword [rel array_type + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ARRAY_TYPE_CONSTRUCTOR, "an array type should identify the array constructor"
    ASSERT_EQ qword [rel array_type + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_UINT8_TYPE, "an array type should contain its element type"
    ASSERT_EQ dword [rel array_type + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET], 1, "an array type should contain a 32-bit element stride"
    ASSERT_EQ dword [rel array_type + SYBILANT_ARRAY_TYPE_LAYOUT_FLAGS_OFFSET], 0, "an array type should reserve layout flags"
    ret

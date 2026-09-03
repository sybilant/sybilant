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
    dq 2
    db 0x12, 0x34
align 8
empty_array:
    dq array_type
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_Sarray_Dget
extern sybilant_Sarray_Dget_Dunchecked

testcase:
    ASSERT_EXIT .dynamic_at_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get should report an out-of-bounds index equal to the length"
    ASSERT_EXIT .dynamic_above_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get should report an out-of-bounds index above the length"
    ASSERT_EXIT .dynamic_empty, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get should report every index into an empty array as out of bounds"
    ASSERT_EXIT .unchecked_at_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get-unchecked should report an out-of-bounds index equal to the length"
    ASSERT_EXIT .unchecked_above_length, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get-unchecked should report an out-of-bounds index above the length"
    ASSERT_EXIT .unchecked_empty, SYBILANT_ERROR_OUT_OF_BOUNDS, "array-get-unchecked should report every index into an empty array as out of bounds"
    ret

.dynamic_at_length:
    lea rdi, [rel array]
    mov esi, 2
    jmp sybilant_Sarray_Dget

.dynamic_above_length:
    lea rdi, [rel array]
    mov rsi, -1
    jmp sybilant_Sarray_Dget

.dynamic_empty:
    lea rdi, [rel empty_array]
    xor esi, esi
    jmp sybilant_Sarray_Dget

.unchecked_at_length:
    lea rdi, [rel array]
    mov esi, 2
    jmp sybilant_Sarray_Dget_Dunchecked

.unchecked_above_length:
    lea rdi, [rel array]
    mov rsi, -1
    jmp sybilant_Sarray_Dget_Dunchecked

.unchecked_empty:
    lea rdi, [rel empty_array]
    xor esi, esi
    jmp sybilant_Sarray_Dget_Dunchecked

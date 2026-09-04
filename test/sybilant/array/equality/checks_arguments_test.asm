bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
uint8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
int8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_INT8_TYPE
    dd 1
    dd 0
align 8
array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56
align 8
array_equal:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56
align 8
different_type_array:
    dq int8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56

section .text
extern sybilant_darray_S_e

testcase:
    sub rsp, 8

    lea rdi, [rel array]
    lea rsi, [rel array_equal]
    call sybilant_darray_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant.array/= should validate and compare equal arrays"

    lea rdi, [rel array]
    lea rsi, [rel different_type_array]
    call sybilant_darray_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant.array/= should treat arrays of different element types as unequal"

    add rsp, 8

    ASSERT_EXIT .invalid_left, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.array/= should reject a non-array left argument"
    ASSERT_EXIT .invalid_right, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant.array/= should reject a non-array right argument"
    ret

.invalid_left:
    mov edi, SYBILANT_TRUE
    lea rsi, [rel array]
    jmp sybilant_darray_S_e

.invalid_right:
    lea rdi, [rel array]
    mov esi, SYBILANT_TRUE
    jmp sybilant_darray_S_e

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
equal_uint8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
uint16_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT16_TYPE
    dd 2
    dd 0
different_stride_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 2
    dd 0
array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_S_e
extern sybilant_Sinstance_q
extern sybilant_Stype

testcase:
    push r12

    lea rdi, [rel array]
    call sybilant_Stype
    mov r12, rax
    lea rdx, [rel uint8_array_type]
    ASSERT_EQ r12, rdx, "sybilant/type should return an array's parameterized type"

    mov rdi, r12
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_TYPE_TYPE, "an array type should be an instance of type"

    lea rdi, [rel uint8_array_type]
    lea rsi, [rel equal_uint8_array_type]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "array types with equal element types should be equal"

    lea rdi, [rel uint8_array_type]
    lea rsi, [rel uint16_array_type]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "array types with different element types should not be equal"

    lea rdi, [rel uint8_array_type]
    lea rsi, [rel different_stride_array_type]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "array types with different element strides should not be equal"

    lea rdi, [rel array]
    lea rsi, [rel equal_uint8_array_type]
    call sybilant_Sinstance_q
    ASSERT_EQ rax, SYBILANT_TRUE, "an array should match an equal parameterized array type"

    lea rdi, [rel array]
    lea rsi, [rel uint16_array_type]
    call sybilant_Sinstance_q
    ASSERT_EQ rax, SYBILANT_FALSE, "an array should not match an array type with a different element"

    lea rdi, [rel array]
    lea rsi, [rel different_stride_array_type]
    call sybilant_Sinstance_q
    ASSERT_EQ rax, SYBILANT_FALSE, "an array should not match an array type with a different element stride"

    pop r12
    ret

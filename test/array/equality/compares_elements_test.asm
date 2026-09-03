bits 64
default rel

%include "test/support.asm"

%macro ASSERT_ARRAY_EQUAL 3
    lea rdi, [rel array]
    lea rsi, [rel %1]
    call sybilant_S_e
    ASSERT_EQ rax, %2, %3
%endmacro

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
array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56
align 8
equal_array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56
align 8
different_element_array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x35, 0x56
align 8
shorter_array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 2
    db 0x12, 0x34
align 8
different_type_array:
    dq int8_array_type
    dq SYBILANT_NIL
    dq 3
    db 0x12, 0x34, 0x56

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    ASSERT_ARRAY_EQUAL equal_array, SYBILANT_TRUE, "distinct immutable arrays with equal elements should be equal"
    ASSERT_ARRAY_EQUAL different_element_array, SYBILANT_FALSE, "immutable arrays with different elements should not be equal"
    ASSERT_ARRAY_EQUAL shorter_array, SYBILANT_FALSE, "immutable arrays with different lengths should not be equal"
    ASSERT_ARRAY_EQUAL different_type_array, SYBILANT_FALSE, "immutable arrays with different element types should not be equal"

    add rsp, 8
    ret

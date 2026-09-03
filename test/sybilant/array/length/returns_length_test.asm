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
extern sybilant_darray_Slength
extern sybilant_darray_Slength_Dunchecked

testcase:
    sub rsp, 8

    lea rdi, [rel array]
    call sybilant_darray_Slength
    ASSERT_EQ rax, 3, "array/length should return the unboxed length"

    lea rdi, [rel array]
    call sybilant_darray_Slength_Dunchecked
    ASSERT_EQ rax, 3, "array/length-unchecked should return the unboxed length"

    add rsp, 8
    ret

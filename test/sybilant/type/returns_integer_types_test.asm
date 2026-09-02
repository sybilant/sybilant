bits 64
default rel

%include "test/support.asm"

%macro ASSERT_IMMEDIATE_TYPE 4
    mov rdi, %1
    shl rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rdi, %2
    call sybilant_Dtype
    ASSERT_EQ rax, %3, %4
%endmacro

%macro ASSERT_POINTER_TYPE 3
    lea rdi, [rel %1]
    call sybilant_Dtype
    ASSERT_EQ rax, %2, %3
%endmacro

section .rodata
align 8
uint64_object:
    dq SYBILANT_UINT64_TYPE
    dq 0
int64_object:
    dq SYBILANT_INT64_TYPE
    dq 0
nat64_object:
    dq SYBILANT_NAT64_TYPE
    dq 0

section .text
extern sybilant_Dtype

testcase:
    sub rsp, 8

    ASSERT_IMMEDIATE_TYPE 0xff, SYBILANT_EXTENDED_TAG_UINT8, SYBILANT_UINT8_TYPE, "sybilant-type should recognize a boxed uint8"
    ASSERT_IMMEDIATE_TYPE 0xffff, SYBILANT_EXTENDED_TAG_UINT16, SYBILANT_UINT16_TYPE, "sybilant-type should recognize a boxed uint16"
    ASSERT_IMMEDIATE_TYPE 0xffffffff, SYBILANT_EXTENDED_TAG_UINT32, SYBILANT_UINT32_TYPE, "sybilant-type should recognize a boxed uint32"
    ASSERT_POINTER_TYPE uint64_object, SYBILANT_UINT64_TYPE, "sybilant-type should recognize a boxed uint64"

    ASSERT_IMMEDIATE_TYPE 0x80, SYBILANT_EXTENDED_TAG_INT8, SYBILANT_INT8_TYPE, "sybilant-type should recognize a boxed int8"
    ASSERT_IMMEDIATE_TYPE 0x8000, SYBILANT_EXTENDED_TAG_INT16, SYBILANT_INT16_TYPE, "sybilant-type should recognize a boxed int16"
    ASSERT_IMMEDIATE_TYPE 0x80000000, SYBILANT_EXTENDED_TAG_INT32, SYBILANT_INT32_TYPE, "sybilant-type should recognize a boxed int32"
    ASSERT_POINTER_TYPE int64_object, SYBILANT_INT64_TYPE, "sybilant-type should recognize a boxed int64"

    ASSERT_IMMEDIATE_TYPE 0x7f, SYBILANT_EXTENDED_TAG_NAT8, SYBILANT_NAT8_TYPE, "sybilant-type should recognize a boxed nat8"
    ASSERT_IMMEDIATE_TYPE 0x7fff, SYBILANT_EXTENDED_TAG_NAT16, SYBILANT_NAT16_TYPE, "sybilant-type should recognize a boxed nat16"
    ASSERT_IMMEDIATE_TYPE 0x7fffffff, SYBILANT_EXTENDED_TAG_NAT32, SYBILANT_NAT32_TYPE, "sybilant-type should recognize a boxed nat32"
    ASSERT_POINTER_TYPE nat64_object, SYBILANT_NAT64_TYPE, "sybilant-type should recognize a boxed nat64"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

%macro ASSERT_ARRAY_GET 5
    lea rdi, [rel %1]
    mov esi, 1
    call sybilant_Sarray_Dget
    mov rdx, %2
    shl rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rdx, %3
    ASSERT_EQ rax, rdx, %4
    ASSERT_EQ qword [rel %1 + SYBILANT_ARRAY_LENGTH_OFFSET], 2, %5
%endmacro

section .rodata
align 8
uint8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
uint8_array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dq 2
    db 0x12, 0xfe
align 8
int8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_INT8_TYPE
    dd 1
    dd 0
int8_array:
    dq int8_array_type
    dq SYBILANT_NIL
    dq 2
    db 0x34, 0x80
align 8
nat8_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_NAT8_TYPE
    dd 1
    dd 0
nat8_array:
    dq nat8_array_type
    dq SYBILANT_NIL
    dd 2
    dd 0
    db 0x56, 0x7f
align 8
uint16_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT16_TYPE
    dd 2
    dd 0
uint16_array:
    dq uint16_array_type
    dq SYBILANT_NIL
    dd 2
    dd 0
    dw 0x1234, 0xfedc
align 8
int16_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_INT16_TYPE
    dd 2
    dd 0
int16_array:
    dq int16_array_type
    dq SYBILANT_NIL
    dq 2
    dw 0x3456, 0x8001
align 8
nat16_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_NAT16_TYPE
    dd 2
    dd 0
nat16_array:
    dq nat16_array_type
    dq SYBILANT_NIL
    dq 2
    dw 0x5678, 0x7fff
align 8
uint32_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT32_TYPE
    dd 4
    dd 0
uint32_array:
    dq uint32_array_type
    dq SYBILANT_NIL
    dq 2
    dd 0x12345678, 0xfedcba98
align 8
int32_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_INT32_TYPE
    dd 4
    dd 0
int32_array:
    dq int32_array_type
    dq SYBILANT_NIL
    dq 2
    dd 0x3456789a, 0x80000001
align 8
nat32_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_NAT32_TYPE
    dd 4
    dd 0
nat32_array:
    dq nat32_array_type
    dq SYBILANT_NIL
    dq 2
    dd 0x12345678, 0x7fffffff

section .text
extern sybilant_Sarray_Dget

testcase:
    sub rsp, 8

    ASSERT_ARRAY_GET uint8_array, 0xfe, SYBILANT_EXTENDED_TAG_UINT8, "array-get should box a packed uint8", "array-get should not overwrite a uint8 array"
    ASSERT_ARRAY_GET int8_array, 0x80, SYBILANT_EXTENDED_TAG_INT8, "array-get should box a packed int8", "array-get should not overwrite an int8 array"
    ASSERT_ARRAY_GET nat8_array, 0x7f, SYBILANT_EXTENDED_TAG_NAT8, "array-get should box a packed nat8", "array-get should not overwrite a nat8 array"
    ASSERT_ARRAY_GET uint16_array, 0xfedc, SYBILANT_EXTENDED_TAG_UINT16, "array-get should box a packed uint16", "array-get should not overwrite a uint16 array"
    ASSERT_ARRAY_GET int16_array, 0x8001, SYBILANT_EXTENDED_TAG_INT16, "array-get should box a packed int16", "array-get should not overwrite an int16 array"
    ASSERT_ARRAY_GET nat16_array, 0x7fff, SYBILANT_EXTENDED_TAG_NAT16, "array-get should box a packed nat16", "array-get should not overwrite a nat16 array"
    ASSERT_ARRAY_GET uint32_array, 0xfedcba98, SYBILANT_EXTENDED_TAG_UINT32, "array-get should box a packed uint32", "array-get should not overwrite a uint32 array"
    ASSERT_ARRAY_GET int32_array, 0x80000001, SYBILANT_EXTENDED_TAG_INT32, "array-get should box a packed int32", "array-get should not overwrite an int32 array"
    ASSERT_ARRAY_GET nat32_array, 0x7fffffff, SYBILANT_EXTENDED_TAG_NAT32, "array-get should box a packed nat32", "array-get should not overwrite a nat32 array"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

%macro ASSERT_ARRAY_GET64 4
    lea rdi, [rel %1]
    mov esi, 1
    call sybilant_darray_Sget
    mov %2, rax
    mov rdx, SYBILANT_MALLOC_START
    ASSERT_ABE %2, rdx, %4
    ASSERT_EQ qword [%2 + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], %3, "array/get should box a 64-bit integer with its element type"
    mov rdx, [rel %1 + SYBILANT_ARRAY_DATA_OFFSET + 8]
    ASSERT_EQ qword [%2 + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET], rdx, "array/get should preserve a 64-bit integer payload"
%endmacro

section .rodata
align 8
uint64_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT64_TYPE
    dd 8
    dd 0
uint64_array:
    dq uint64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x0123456789abcdef, 0xfedcba9876543210
int64_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_INT64_TYPE
    dd 8
    dd 0
int64_array:
    dq int64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x123456789abcdef0, 0x8000000000000001
nat64_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_NAT64_TYPE
    dd 8
    dd 0
nat64_array:
    dq nat64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x123456789abcdef0, 0x7fffffffffffffff

section .text
extern sybilant_darray_Sget

testcase:
    push r12
    push r13
    push r14

    ASSERT_ARRAY_GET64 uint64_array, r12, SYBILANT_UINT64_TYPE, "array/get should allocate a boxed uint64"
    ASSERT_ARRAY_GET64 int64_array, r13, SYBILANT_INT64_TYPE, "array/get should allocate a boxed int64"
    ASSERT_ARRAY_GET64 nat64_array, r14, SYBILANT_NAT64_TYPE, "array/get should allocate a boxed nat64"

    pop r14
    pop r13
    pop r12
    ret

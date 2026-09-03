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
uint8_array:
    dq uint8_array_type
    dq SYBILANT_NIL
    dd 2
    dd 0
    db 0x12, 0xfe
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
    dq 2
    dw 0x1234, 0xfedc
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

section .text
extern sybilant_Sarray_Dget_Dunchecked

testcase:
    sub rsp, 8

    mov rax, 0x0123456789abcdef
    lea rdi, [rel uint8_array]
    mov esi, 1
    call sybilant_Sarray_Dget_Dunchecked
    ASSERT_EQ al, 0xfe, "array-get-unchecked should return a one-byte element in al"
    mov rdx, 0x0123456789abcdfe
    ASSERT_EQ rax, rdx, "a one-byte array get should not zero-extend rax"

    mov rax, 0x0123456789abcdef
    lea rdi, [rel uint16_array]
    mov esi, 1
    call sybilant_Sarray_Dget_Dunchecked
    ASSERT_EQ ax, 0xfedc, "array-get-unchecked should return a two-byte element in ax"
    mov rdx, 0x0123456789abfedc
    ASSERT_EQ rax, rdx, "a two-byte array get should not zero-extend rax"

    lea rdi, [rel uint32_array]
    mov esi, 1
    call sybilant_Sarray_Dget_Dunchecked
    ASSERT_EQ eax, 0xfedcba98, "array-get-unchecked should return a four-byte element in eax"

    lea rdi, [rel uint64_array]
    mov esi, 1
    call sybilant_Sarray_Dget_Dunchecked
    mov rdx, 0xfedcba9876543210
    ASSERT_EQ rax, rdx, "array-get-unchecked should return an eight-byte element in rax"

    add rsp, 8
    ret

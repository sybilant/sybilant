bits 64
default rel

%include "test/support.asm"

    BYTE_VALUE_TYPE_CONSTRUCTOR  equ 0b00100000_00001111
    WORD_VALUE_TYPE_CONSTRUCTOR  equ 0b00100001_00001111
    DWORD_VALUE_TYPE_CONSTRUCTOR equ 0b00100010_00001111

section .rodata
align 8
byte_value_type:
    dq SYBILANT_TYPE_TYPE
    dq BYTE_VALUE_TYPE_CONSTRUCTOR
byte_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq byte_value_type
    dd 1
    dd 0xfeedface
byte_array:
    dq byte_array_type
    dq SYBILANT_NIL
    dd 2
    dd 0
    db 0x12, 0xfe
    times 6 db 0
    dq 0x1111111111111111
word_value_type:
    dq SYBILANT_TYPE_TYPE
    dq WORD_VALUE_TYPE_CONSTRUCTOR
word_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq word_value_type
    dd 2
    dd 0
word_array:
    dq word_array_type
    dq SYBILANT_NIL
    dq 2
    dw 0x1234, 0xfedc
    dd 0
    dq 0x2222222222222222
dword_value_type:
    dq SYBILANT_TYPE_TYPE
    dq DWORD_VALUE_TYPE_CONSTRUCTOR
dword_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq dword_value_type
    dd 4
    dd 0
dword_array:
    dq dword_array_type
    dq SYBILANT_NIL
    dq 2
    dd 0x12345678, 0xfedcba98
    dq 0x3333333333333333

section .text
extern sybilant_darray_Sget_Dunchecked

testcase:
    sub rsp, 8

    mov rax, 0x0123456789abcdef
    lea rdi, [rel byte_array]
    mov esi, 1
    call sybilant_darray_Sget_Dunchecked
    mov rdx, 0x0123456789abcdfe
    ASSERT_EQ rax, rdx, "array/get-unchecked should use a one-byte stride from an array type"

    mov rax, 0x0123456789abcdef
    lea rdi, [rel word_array]
    mov esi, 1
    call sybilant_darray_Sget_Dunchecked
    mov rdx, 0x0123456789abfedc
    ASSERT_EQ rax, rdx, "array/get-unchecked should use a two-byte stride from an array type"

    lea rdi, [rel dword_array]
    mov esi, 1
    call sybilant_darray_Sget_Dunchecked
    ASSERT_EQ eax, 0xfedcba98, "array/get-unchecked should use a four-byte stride from an array type"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

%macro ASSERT_ARRAY_EQ 4
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call sybilant_S_e
    ASSERT_EQ rax, %3, %4
%endmacro

section .rodata
align 8
uint16_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT16_TYPE
    dd 2
    dd 0
uint32_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT32_TYPE
    dd 4
    dd 0
uint64_array_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT64_TYPE
    dd 8
    dd 0

align 8
word_array:
    dq uint16_array_type
    dq SYBILANT_NIL
    dq 3
    dw 0x1234, 0x5678, 0x9abc
align 8
word_array_equal:
    dq uint16_array_type
    dq SYBILANT_NIL
    dq 3
    dw 0x1234, 0x5678, 0x9abc
align 8
word_array_different:
    dq uint16_array_type
    dq SYBILANT_NIL
    dq 3
    dw 0x1234, 0x5679, 0x9abc

align 8
doubleword_array:
    dq uint32_array_type
    dq SYBILANT_NIL
    dq 3
    dd 0x11111111, 0x22222222, 0x33333333
align 8
doubleword_array_equal:
    dq uint32_array_type
    dq SYBILANT_NIL
    dq 3
    dd 0x11111111, 0x22222222, 0x33333333
align 8
doubleword_array_different:
    dq uint32_array_type
    dq SYBILANT_NIL
    dq 3
    dd 0x11111111, 0x22222223, 0x33333333

align 8
quadword_array:
    dq uint64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x1111111111111111, 0x2222222222222222
align 8
quadword_array_equal:
    dq uint64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x1111111111111111, 0x2222222222222222
align 8
quadword_array_different:
    dq uint64_array_type
    dq SYBILANT_NIL
    dq 2
    dq 0x1111111111111111, 0x2222222222222223

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    ASSERT_ARRAY_EQ word_array, word_array_equal, SYBILANT_TRUE, "distinct uint16 arrays with equal elements should be equal"
    ASSERT_ARRAY_EQ word_array, word_array_different, SYBILANT_FALSE, "uint16 arrays with a differing element should not be equal"

    ASSERT_ARRAY_EQ doubleword_array, doubleword_array_equal, SYBILANT_TRUE, "distinct uint32 arrays with equal elements should be equal"
    ASSERT_ARRAY_EQ doubleword_array, doubleword_array_different, SYBILANT_FALSE, "uint32 arrays with a differing element should not be equal"

    ASSERT_ARRAY_EQ quadword_array, quadword_array_equal, SYBILANT_TRUE, "distinct uint64 arrays with equal elements should be equal"
    ASSERT_ARRAY_EQ quadword_array, quadword_array_different, SYBILANT_FALSE, "uint64 arrays with a differing element should not be equal"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Stype

testcase:
    sub rsp, 8

    mov edi, SYBILANT_FALSE
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_BOOLEAN_TYPE, "sybilant/type should return the boolean type for false"

    mov edi, SYBILANT_TRUE
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_BOOLEAN_TYPE, "sybilant/type should return the boolean type for true"

    add rsp, 8
    ret

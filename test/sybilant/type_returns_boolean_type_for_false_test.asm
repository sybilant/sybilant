bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dtype

testcase:
    sub rsp, 8
    mov edi, SYBILANT_FALSE
    call sybilant_Dtype
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_BOOLEAN_TYPE, "sybilant-type should return the boolean type for false"
    ret

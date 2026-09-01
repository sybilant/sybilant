bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dboolean_q

testcase:
    sub rsp, 8
    mov edi, SYBILANT_FALSE
    call sybilant_Dboolean_q
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-boolean? should recognize false"
    ret

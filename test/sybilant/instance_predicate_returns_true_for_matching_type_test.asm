bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dinstance_q

testcase:
    sub rsp, 8
    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-instance? should return true for a matching type"
    ret

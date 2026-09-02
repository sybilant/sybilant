bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dboolean_q
extern sybilant_Dboolean_q_Dunchecked

testcase:
    sub rsp, 8

    mov edi, SYBILANT_FALSE
    call sybilant_Dboolean_q
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-boolean? should recognize false"

    mov edi, SYBILANT_TRUE
    call sybilant_Dboolean_q
    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-boolean? should recognize true"

    mov edi, SYBILANT_TRUE
    call sybilant_Dboolean_q_Dunchecked
    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked sybilant-boolean? should accept a proven boolean"

    add rsp, 8
    ret

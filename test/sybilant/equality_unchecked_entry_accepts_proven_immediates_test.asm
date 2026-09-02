bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_D_e_Dunchecked

testcase:
    sub rsp, 8
    mov edi, SYBILANT_TRUE
    mov esi, SYBILANT_TRUE
    call sybilant_D_e_Dunchecked
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked sybilant-= should accept proven immediate values"
    ret

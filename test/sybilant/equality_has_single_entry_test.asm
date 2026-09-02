bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_D_e
global sybilant_D_e_Dunchecked

testcase:
    sub rsp, 8
    mov edi, SYBILANT_TRUE
    mov esi, SYBILANT_TRUE
    call sybilant_D_e
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_TRUE, "sybilant-= should accept proven immediate values through its sole entry"
    ret

;; Reserve the removed entry's symbol so the test fails if the runtime exports it.
sybilant_D_e_Dunchecked:
    ud2

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Satom_Dnew
extern sybilant_Satom_Dderef
extern sybilant_Satom_Dderef_Dunchecked

testcase:
    push r12

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_Satom_Dnew
    mov r12, rax

    mov rdi, r12
    call sybilant_Satom_Dderef
    ASSERT_EQ rax, SYBILANT_TRUE, "atom-deref should return the initial value"

    mov rdi, r12
    call sybilant_Satom_Dderef_Dunchecked
    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked atom-deref should return the initial value"

    pop r12
    ret

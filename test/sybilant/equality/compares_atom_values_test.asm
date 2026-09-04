bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_S_e
extern sybilant_datom_Snew

testcase:
    push r12
    push r13

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_datom_Snew
    mov r12, rax

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_datom_Snew
    mov r13, rax

    mov rdi, r12
    mov rsi, r13
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "distinct atoms with equal contents should not be equal"

    mov rdi, r12
    mov rsi, r12
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "an atom should be equal to itself"

    pop r13
    pop r12
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Satom_Dcompare_Dand_Dset
extern sybilant_Satom_Dderef
extern sybilant_Satom_Dnew
extern sybilant_Stype

testcase:
    push r12
    push r13
    push r14

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_Satom_Dnew
    mov r12, rax

    mov rdi, r12
    call sybilant_Stype
    mov r13, rax

    mov rdi, r13
    mov rsi, r12
    call sybilant_Satom_Dnew
    mov r14, rax

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_FALSE
    call sybilant_Satom_Dnew
    mov r13, rax

    mov rdi, r14
    mov rsi, r12
    mov rdx, r13
    call sybilant_Satom_Dcompare_Dand_Dset
    ASSERT_EQ rax, SYBILANT_TRUE, "atom-compare-and-set should accept an equal parameterized element type"

    mov rdi, r14
    call sybilant_Satom_Dderef
    ASSERT_EQ rax, r13, "an atom should store a value with a parameterized element type"

    pop r14
    pop r13
    pop r12
    ret

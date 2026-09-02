bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_S_e
extern sybilant_Satom_Dnew
extern sybilant_Stype

testcase:
    push r12
    push r13
    push r14

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_Satom_Dnew
    mov rdi, rax
    call sybilant_Stype
    mov r12, rax

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_FALSE
    call sybilant_Satom_Dnew
    mov rdi, rax
    call sybilant_Stype
    mov r13, rax

    mov edi, SYBILANT_NAT32_TYPE
    mov esi, SYBILANT_EXTENDED_TAG_NAT32
    call sybilant_Satom_Dnew
    mov rdi, rax
    call sybilant_Stype
    mov r14, rax

    mov rdi, r12
    mov rsi, r13
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, "atom types with equal element types should be equal"

    mov rdi, r12
    mov rsi, r14
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, "atom types with different element types should not be equal"

    pop r14
    pop r13
    pop r12
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Satom_Dcompare_Dand_Dset
extern sybilant_Satom_Dderef
extern sybilant_Satom_Dnew
extern sybilant_Sbox_Duint64

testcase:
    push r12
    push r13
    push r14

    mov rdi, -1
    call sybilant_Sbox_Duint64
    mov r12, rax

    mov rdi, -1
    call sybilant_Sbox_Duint64
    mov r13, rax

    mov edi, SYBILANT_UINT64_TYPE
    mov rsi, r12
    call sybilant_Satom_Dnew
    mov r14, rax

    mov rdi, r14
    mov rsi, r13
    mov rdx, r12
    call sybilant_Satom_Dcompare_Dand_Dset
    ASSERT_EQ rax, SYBILANT_FALSE, "atom-compare-and-set should reject an equal but nonidentical value"

    mov rdi, r14
    call sybilant_Satom_Dderef
    ASSERT_EQ rax, r12, "a rejected nonidentical value should leave the atom unchanged"

    mov rdi, r14
    mov rsi, r12
    mov rdx, r13
    call sybilant_Satom_Dcompare_Dand_Dset
    ASSERT_EQ rax, SYBILANT_TRUE, "atom-compare-and-set should accept the identical heap value"

    pop r14
    pop r13
    pop r12
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_datom_Scompare_Dand_Dset
extern sybilant_datom_Sderef
extern sybilant_datom_Snew

testcase:
    push r12

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_datom_Snew
    mov r12, rax

    mov rdi, r12
    mov esi, SYBILANT_TRUE
    mov edx, SYBILANT_FALSE
    call sybilant_datom_Scompare_Dand_Dset
    ASSERT_EQ rax, SYBILANT_TRUE, "atom/compare-and-set should update an identical value"

    mov rdi, r12
    call sybilant_datom_Sderef
    ASSERT_EQ rax, SYBILANT_FALSE, "a successful atom/compare-and-set should store the new value"

    mov rdi, r12
    mov esi, SYBILANT_TRUE
    mov edx, SYBILANT_TRUE
    call sybilant_datom_Scompare_Dand_Dset
    ASSERT_EQ rax, SYBILANT_FALSE, "atom/compare-and-set should reject a different current value"

    mov rdi, r12
    call sybilant_datom_Sderef
    ASSERT_EQ rax, SYBILANT_FALSE, "a failed atom/compare-and-set should preserve the current value"

    pop r12
    ret

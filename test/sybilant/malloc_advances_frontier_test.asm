bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dmalloc

testcase:
    push r12

    mov edi, 4097
    call sybilant_Dmalloc
    mov r12, rax

    mov edi, 17
    call sybilant_Dmalloc

    lea rdx, [r12 + 4097]
    ASSERT_ABE rax, rdx, "a later allocation should be above the earlier allocation"

    pop r12
    ret

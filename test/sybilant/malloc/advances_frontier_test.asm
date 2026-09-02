bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dmalloc
extern sybilant_Dmalloc_Dunchecked

testcase:
    push r12

    mov edi, 4097
    call sybilant_Dmalloc
    mov r12, rax

    mov edi, 17
    call sybilant_Dmalloc_Dunchecked

    lea rdx, [r12 + 4097]
    ASSERT_EQ rax, rdx, "a later allocation should begin at the byte-granular frontier"

    pop r12
    ret

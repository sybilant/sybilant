bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Smalloc

testcase:
    push r12

    mov edi, PAGE_SIZE
    call sybilant_Smalloc
    mov r12, rax

    mov byte [r12], 0x5a
    mov byte [r12 + PAGE_SIZE - 1], 0xa5

    mov edi, 1
    call sybilant_Smalloc

    ASSERT_EQ byte [r12], 0x5a, "extending the allocation mapping should preserve earlier data"
    ASSERT_EQ byte [r12 + PAGE_SIZE - 1], 0xa5, "extending the allocation mapping should preserve the complete earlier allocation"

    pop r12
    ret

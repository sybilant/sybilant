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

    mov byte [r12], 0x5a
    mov byte [r12 + 4096], 0xa5

    mov edi, 17
    call sybilant_Dmalloc

    ASSERT_EQ byte [r12], 0x5a, "a later allocation should preserve earlier data"
    ASSERT_EQ byte [r12 + 4096], 0xa5, "a later allocation should preserve the complete earlier allocation"

    pop r12
    ret

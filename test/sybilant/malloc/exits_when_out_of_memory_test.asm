bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Smalloc

testcase:
    ASSERT_EXIT .malloc_maximum, SYBILANT_ERROR_OUT_OF_MEMORY, "sybilant/malloc should report out of memory"
    ret

.malloc_maximum:
    mov rdi, -1
    jmp sybilant_Smalloc

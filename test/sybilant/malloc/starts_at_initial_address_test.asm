bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dmalloc

testcase:
    sub rsp, 8
    mov edi, 1
    call sybilant_Dmalloc
    add rsp, 8

    mov rdx, SYBILANT_MALLOC_START
    ASSERT_EQ rax, rdx, "the first allocation should start at the initial malloc address"
    ret

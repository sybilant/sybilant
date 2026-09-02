bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Smalloc

testcase:
    sub rsp, 8
    mov edi, 4097
    call sybilant_Smalloc
    add rsp, 8

    mov byte [rax], 0x5a
    mov byte [rax + 4096], 0xa5

    ASSERT_EQ byte [rax], 0x5a, "the start of an allocation should be writable"
    ASSERT_EQ byte [rax + 4096], 0xa5, "the end of an allocation should be writable"

    ret

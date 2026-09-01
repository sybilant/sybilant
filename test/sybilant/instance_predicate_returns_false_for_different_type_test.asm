bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_Dinstance_q

testcase:
    sub rsp, 8
    lea rdi, [rel object]
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant-instance? should return false for a different type"
    ret

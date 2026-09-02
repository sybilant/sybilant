bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_Sboolean_q

testcase:
    sub rsp, 8
    lea rdi, [rel object]
    call sybilant_Sboolean_q
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "sybilant/boolean? should reject a non-boolean value"
    ret

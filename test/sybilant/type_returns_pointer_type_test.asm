bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_BOOLEAN_TYPE

section .text
extern sybilant_Dtype

testcase:
    sub rsp, 8
    lea rdi, [rel object]
    call sybilant_Dtype
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_BOOLEAN_TYPE, "sybilant-type should return the first word of a pointer value"
    ret

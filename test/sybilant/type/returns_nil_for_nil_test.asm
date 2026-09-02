bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Stype

testcase:
    sub rsp, 8
    mov edi, SYBILANT_NIL
    call sybilant_Stype
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_NIL, "sybilant/type should return nil for nil"
    ret

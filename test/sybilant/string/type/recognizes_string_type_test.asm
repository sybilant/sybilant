bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 0

section .text
extern sybilant_Sinstance_q
extern sybilant_Stype

testcase:
    sub rsp, 8

    lea rdi, [rel string]
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_STRING_TYPE, "sybilant/type should return the string type"

    lea rdi, [rel string]
    mov esi, SYBILANT_STRING_TYPE
    call sybilant_Sinstance_q
    ASSERT_EQ rax, SYBILANT_TRUE, "a string should be an instance of the string type"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Stype
extern sybilant_Stype_Dunchecked

testcase:
    sub rsp, 8

    mov edi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_TYPE_TYPE, "sybilant/type should return the type type for a type value"

    mov edi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Stype_Dunchecked
    ASSERT_EQ rax, SYBILANT_TYPE_TYPE, "unchecked sybilant/type should return the type type for a proven type value"

    add rsp, 8
    ret

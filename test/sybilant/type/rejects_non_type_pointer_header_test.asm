bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_FALSE

section .text
extern sybilant_Stype
extern sybilant_Stype_Dunchecked

testcase:
    ASSERT_EXIT .type, SYBILANT_ERROR_INVALID_STATE, "sybilant/type should reject a non-type pointer header"

    sub rsp, 8
    lea rdi, [rel object]
    call sybilant_Stype_Dunchecked
    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "unchecked sybilant/type should return an unvalidated pointer header"
    ret

.type:
    lea rdi, [rel object]
    jmp sybilant_Stype

bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object:
    dq SYBILANT_TYPE_TYPE

section .text
extern sybilant_D_e

testcase:
    ASSERT_EXIT .equal, SYBILANT_ERROR_INVALID_STATE, "sybilant-= should reject a heap type without equality support"
    ret

.equal:
    lea rdi, [rel object]
    mov rsi, rdi
    jmp sybilant_D_e

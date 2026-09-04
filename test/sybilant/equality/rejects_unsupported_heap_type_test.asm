bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
object_a:
    dq SYBILANT_BOOLEAN_TYPE
object_b:
    dq SYBILANT_BOOLEAN_TYPE

section .text
extern sybilant_S_e

testcase:
    ASSERT_EXIT .distinct, SYBILANT_ERROR_INVALID_STATE, "sybilant/= should reject distinct values of a heap type without equality support"
    ret

.distinct:
    lea rdi, [rel object_a]
    lea rsi, [rel object_b]
    jmp sybilant_S_e

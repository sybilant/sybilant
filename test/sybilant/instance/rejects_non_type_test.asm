bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dinstance_q

testcase:
    ASSERT_EXIT .instance_predicate, SYBILANT_ERROR_INVALID_ARGUMENT, "sybilant-instance? should reject a non-type second argument"
    ret

.instance_predicate:
    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_FALSE
    jmp sybilant_Dinstance_q

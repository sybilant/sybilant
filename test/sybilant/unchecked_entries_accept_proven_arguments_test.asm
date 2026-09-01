bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dboolean_q_Dunchecked
extern sybilant_Dexit_Dunchecked
extern sybilant_Dinstance_q_Dunchecked
extern sybilant_Dmalloc_Dunchecked
extern sybilant_Dtype_Dunchecked

testcase:
    sub rsp, 8

    mov edi, SYBILANT_FALSE
    call sybilant_Dtype_Dunchecked
    ASSERT_EQ rax, SYBILANT_BOOLEAN_TYPE, "unchecked sybilant-type should return a proven value's type"

    mov edi, SYBILANT_FALSE
    mov esi, SYBILANT_BOOLEAN_TYPE
    call sybilant_Dinstance_q_Dunchecked
    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked sybilant-instance? should accept proven arguments"

    mov edi, SYBILANT_TRUE
    call sybilant_Dboolean_q_Dunchecked
    ASSERT_EQ rax, SYBILANT_TRUE, "unchecked sybilant-boolean? should accept a proven value"

    mov edi, 1
    call sybilant_Dmalloc_Dunchecked
    mov byte [rax], 0xa5
    ASSERT_EQ byte [rax], 0xa5, "unchecked sybilant-malloc should return writable storage"

    add rsp, 8

    ASSERT_EXIT .exit_42, 42, "unchecked sybilant-exit should preserve a proven status"
    ret

.exit_42:
    mov edi, 42
    jmp sybilant_Dexit_Dunchecked

bits 64
default rel

%include "test/support.asm"

section .data
align 8
editor_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_THREAD_TYPE
editor:
    dq editor_type
    dq 0
transient_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_BOOLEAN_TYPE
    dd 8
    dd 0
transient:
    dq transient_type
    dq editor
    dq 1
    dq SYBILANT_TRUE

section .text
extern sybilant_dthread_Sself
extern sybilant_darray_Stransient_Dget_B

testcase:
    call sybilant_dthread_Sself
    mov [rel editor + SYBILANT_ATOM_VALUE_OFFSET], rax

    lea rdi, [rel transient]
    xor esi, esi
    call sybilant_darray_Stransient_Dget_B
    ASSERT_EQ rax, SYBILANT_TRUE, "array/transient-get! should pass a boolean element through without boxing"
    ret

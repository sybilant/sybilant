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
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
transient:
    dq transient_type
    dq editor
    dq 2
    db 0x12, 0x34

section .text
extern sybilant_dthread_Sself
extern sybilant_darray_Stransient_Dpersistent_B

testcase:
    call sybilant_dthread_Sself
    mov [rel editor + SYBILANT_ATOM_VALUE_OFFSET], rax
    lea rdi, [rel transient]
    call sybilant_darray_Stransient_Dpersistent_B
    lea rdx, [rel transient]
    ASSERT_EQ rax, rdx, "array/transient-persistent! should return the converted transient"
    ASSERT_EQ qword [rel transient + SYBILANT_ARRAY_EDITOR_OFFSET], SYBILANT_NIL, "persistent conversion should clear the editor"
    mov rax, [rel transient + SYBILANT_ARRAY_TYPE_OFFSET]
    ASSERT_EQ qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ARRAY_TYPE_CONSTRUCTOR, "persistent conversion should switch to the immutable array type"
    ASSERT_EQ qword [rel transient + SYBILANT_ARRAY_LENGTH_OFFSET], 2, "persistent conversion should preserve the length"
    ASSERT_EQ byte [rel transient + SYBILANT_ARRAY_DATA_OFFSET], 0x12, "persistent conversion should preserve the data"
    ret

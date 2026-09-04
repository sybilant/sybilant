bits 64
default rel

%include "test/support.asm"

section .data
align 8
transient_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
editor_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_THREAD_TYPE
editor:
    dq editor_type
    dq 0
transient:
    dq transient_type
    dq editor
    dq 2
    db 0x12, 0x34

section .text
extern sybilant_dthread_Sself
extern sybilant_darray_Stransient_Dinsert_B
extern sybilant_darray_Stransient_Ddelete_B
extern sybilant_darray_Stransient_Dresize_B

testcase:
    call sybilant_dthread_Sself
    mov [rel editor + SYBILANT_ATOM_VALUE_OFFSET], rax
    lea rdi, [rel transient]
    mov esi, 1
    mov edx, (0x56 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    call sybilant_darray_Stransient_Dinsert_B
    mov rdi, rax
    test rdi, rdi
    setnz al
    ASSERT_EQ al, 1, "array/transient-insert! should return a new transient"
    ASSERT_EQ qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], 3, "array/transient-insert! should increase the length"
    ASSERT_EQ byte [rdi + SYBILANT_ARRAY_DATA_OFFSET + 1], 0x56, "array/transient-insert! should place the inserted value"
    ASSERT_EQ qword [rel transient + SYBILANT_ARRAY_EDITOR_OFFSET], SYBILANT_NIL, "array/transient-insert! should invalidate the old transient"

    mov esi, 1
    call sybilant_darray_Stransient_Ddelete_B
    mov rdi, rax
    test rdi, rdi
    setnz al
    ASSERT_EQ al, 1, "array/transient-delete! should return a new transient"
    ASSERT_EQ qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], 2, "array/transient-delete! should reduce the length"

    mov esi, (4 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32
    call sybilant_darray_Stransient_Dresize_B
    mov rdi, rax
    test rdi, rdi
    setnz al
    ASSERT_EQ al, 1, "array/transient-resize! should return a new transient"
    ASSERT_EQ qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], 4, "array/transient-resize! should change the length"
    ret

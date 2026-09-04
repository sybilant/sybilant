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
extern sybilant_darray_Stransient_Dlength_B
extern sybilant_darray_Stransient_Dget_B
extern sybilant_darray_Stransient_Dset_B
extern sybilant_darray_Stransient_Dget_B_Dunchecked

testcase:
    sub rsp, 8
    call sybilant_dthread_Sself
    mov [rel editor + SYBILANT_ATOM_VALUE_OFFSET], rax

    lea rdi, [rel transient]
    call sybilant_darray_Stransient_Dlength_B
    ASSERT_EQ rax, 2, "array/transient-length! should return the length for the editor"

    lea rdi, [rel transient]
    mov esi, 1
    call sybilant_darray_Stransient_Dget_B
    mov rdx, (0x34 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    ASSERT_EQ rax, rdx, "array/transient-get! should return the selected element"

    lea rdi, [rel transient]
    mov esi, 0
    mov edx, (0x56 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    call sybilant_darray_Stransient_Dset_B
    lea rdx, [rel transient]
    ASSERT_EQ rax, rdx, "array/transient-set! should return the transient reference"
    ASSERT_EQ byte [rel transient + SYBILANT_ARRAY_DATA_OFFSET], 0x56, "array/transient-set! should update the selected element"

    lea rdi, [rel transient]
    xor esi, esi
    call sybilant_darray_Stransient_Dget_B_Dunchecked
    ASSERT_EQ al, 0x56, "array/transient-get!-unchecked should skip the editor check"

    add rsp, 8
    ret

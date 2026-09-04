bits 64
default rel

%include "test/support.asm"

section .data
align 8
other_thread:
    dq SYBILANT_THREAD_TYPE
    dq 999
    dq SYBILANT_NIL
editor_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_ATOM_TYPE_CONSTRUCTOR
    dq SYBILANT_THREAD_TYPE
mismatched_editor:
    dq editor_type
    dq other_thread
transient_type:
    dq SYBILANT_TYPE_TYPE
    dq SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR
    dq SYBILANT_UINT8_TYPE
    dd 1
    dd 0
mismatched_transient:
    dq transient_type
    dq mismatched_editor
    dq 2
    db 0x12, 0x34
align 8
invalidated_transient:
    dq transient_type
    dq 0
    dq 2
    db 0x12, 0x34

section .text
extern sybilant_darray_Stransient_Dlength_B
extern sybilant_darray_Stransient_Dget_B
extern sybilant_darray_Stransient_Dset_B
extern sybilant_darray_Stransient_Dinsert_B
extern sybilant_darray_Stransient_Ddelete_B
extern sybilant_darray_Stransient_Dresize_B
extern sybilant_darray_Stransient_Dpersistent_B

testcase:
    ASSERT_EXIT .length_mismatched, SYBILANT_ERROR_INVALID_STATE, "array/transient-length! should reject an editor held by another thread"
    ASSERT_EXIT .get_invalidated, SYBILANT_ERROR_INVALID_STATE, "array/transient-get! should reject an invalidated transient"
    ASSERT_EXIT .set_mismatched, SYBILANT_ERROR_INVALID_STATE, "array/transient-set! should reject an editor held by another thread"
    ASSERT_EXIT .insert_invalidated, SYBILANT_ERROR_INVALID_STATE, "array/transient-insert! should reject an invalidated transient"
    ASSERT_EXIT .delete_mismatched, SYBILANT_ERROR_INVALID_STATE, "array/transient-delete! should reject an editor held by another thread"
    ASSERT_EXIT .resize_invalidated, SYBILANT_ERROR_INVALID_STATE, "array/transient-resize! should reject an invalidated transient"
    ASSERT_EXIT .persistent_mismatched, SYBILANT_ERROR_INVALID_STATE, "array/transient-persistent! should reject an editor held by another thread"
    ret

.length_mismatched:
    lea rdi, [rel mismatched_transient]
    jmp sybilant_darray_Stransient_Dlength_B

.get_invalidated:
    lea rdi, [rel invalidated_transient]
    xor esi, esi
    jmp sybilant_darray_Stransient_Dget_B

.set_mismatched:
    lea rdi, [rel mismatched_transient]
    xor esi, esi
    mov edx, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_darray_Stransient_Dset_B

.insert_invalidated:
    lea rdi, [rel invalidated_transient]
    xor esi, esi
    mov edx, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_darray_Stransient_Dinsert_B

.delete_mismatched:
    lea rdi, [rel mismatched_transient]
    xor esi, esi
    jmp sybilant_darray_Stransient_Ddelete_B

.resize_invalidated:
    lea rdi, [rel invalidated_transient]
    mov esi, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32
    jmp sybilant_darray_Stransient_Dresize_B

.persistent_mismatched:
    lea rdi, [rel mismatched_transient]
    jmp sybilant_darray_Stransient_Dpersistent_B

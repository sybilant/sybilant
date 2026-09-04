bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_darray_Stransient_Dlength_B
extern sybilant_darray_Stransient_Dget_B
extern sybilant_darray_Stransient_Dset_B
extern sybilant_darray_Stransient_Dinsert_B
extern sybilant_darray_Stransient_Ddelete_B
extern sybilant_darray_Stransient_Dresize_B
extern sybilant_darray_Stransient_Dpersistent_B

testcase:
    ASSERT_EXIT .length, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-length! should reject a nil reference"
    ASSERT_EXIT .get, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-get! should reject a nil reference"
    ASSERT_EXIT .set, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-set! should reject a nil reference"
    ASSERT_EXIT .insert, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-insert! should reject a nil reference"
    ASSERT_EXIT .delete, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-delete! should reject a nil reference"
    ASSERT_EXIT .resize, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-resize! should reject a nil reference"
    ASSERT_EXIT .persistent, SYBILANT_ERROR_INVALID_ARGUMENT, "array/transient-persistent! should reject a nil reference"
    ret

.length:
    xor edi, edi
    jmp sybilant_darray_Stransient_Dlength_B

.get:
    xor edi, edi
    xor esi, esi
    jmp sybilant_darray_Stransient_Dget_B

.set:
    xor edi, edi
    xor esi, esi
    mov edx, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_darray_Stransient_Dset_B

.insert:
    xor edi, edi
    xor esi, esi
    mov edx, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_darray_Stransient_Dinsert_B

.delete:
    xor edi, edi
    xor esi, esi
    jmp sybilant_darray_Stransient_Ddelete_B

.resize:
    xor edi, edi
    mov esi, (1 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32
    jmp sybilant_darray_Stransient_Dresize_B

.persistent:
    xor edi, edi
    jmp sybilant_darray_Stransient_Dpersistent_B

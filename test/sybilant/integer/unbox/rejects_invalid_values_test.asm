bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
uint64_object:
    dq SYBILANT_UINT64_TYPE
    dq 0
int64_object:
    dq SYBILANT_INT64_TYPE
    dq 0

section .text
extern sybilant_Sunbox_Duint8
extern sybilant_Sunbox_Duint64
extern sybilant_Sunbox_Dnat8
extern sybilant_Sunbox_Dnat64

testcase:
    ASSERT_EXIT .unbox_uint8_from_uint16, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-uint8 should reject a different width"
    ASSERT_EXIT .unbox_nat8_from_uint8, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-nat8 should reject an unsigned integer"
    ASSERT_EXIT .unbox_nat8_from_int8, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-nat8 should reject a signed integer"
    ASSERT_EXIT .unbox_nat64_from_uint64, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-nat64 should reject an unsigned integer"
    ASSERT_EXIT .unbox_nat64_from_int64, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-nat64 should reject a signed integer"
    ASSERT_EXIT .unbox_uint64_from_nil, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-uint64 should reject nil"
    ASSERT_EXIT .unbox_uint64_from_immediate, SYBILANT_ERROR_INVALID_ARGUMENT, "unbox-uint64 should reject an immediate value"
    ret

.unbox_uint8_from_uint16:
    mov edi, SYBILANT_EXTENDED_TAG_UINT16
    jmp sybilant_Sunbox_Duint8

.unbox_nat8_from_uint8:
    mov edi, SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_Sunbox_Dnat8

.unbox_nat8_from_int8:
    mov edi, SYBILANT_EXTENDED_TAG_INT8
    jmp sybilant_Sunbox_Dnat8

.unbox_nat64_from_uint64:
    lea rdi, [rel uint64_object]
    jmp sybilant_Sunbox_Dnat64

.unbox_nat64_from_int64:
    lea rdi, [rel int64_object]
    jmp sybilant_Sunbox_Dnat64

.unbox_uint64_from_nil:
    mov edi, SYBILANT_NIL
    jmp sybilant_Sunbox_Duint64

.unbox_uint64_from_immediate:
    mov edi, SYBILANT_EXTENDED_TAG_UINT8
    jmp sybilant_Sunbox_Duint64

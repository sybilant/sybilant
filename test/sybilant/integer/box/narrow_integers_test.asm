bits 64
default rel

%include "test/support.asm"

%macro ASSERT_BOX 5
    mov rdi, %2
    call %1

    mov rdx, %2
    and edx, %3
    shl rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rdx, %4
    ASSERT_EQ rax, rdx, %5
%endmacro

section .text
extern sybilant_Sbox_Duint8
extern sybilant_Sbox_Duint16
extern sybilant_Sbox_Duint32
extern sybilant_Sbox_Dint8
extern sybilant_Sbox_Dint16
extern sybilant_Sbox_Dint32
extern sybilant_Sbox_Dnat8
extern sybilant_Sbox_Dnat16
extern sybilant_Sbox_Dnat32

testcase:
    sub rsp, 8

    ASSERT_BOX sybilant_Sbox_Duint8, 0xff, 0xff, SYBILANT_EXTENDED_TAG_UINT8, "box-uint8 should preserve the complete payload"
    ASSERT_BOX sybilant_Sbox_Duint16, 0xffff, 0xffff, SYBILANT_EXTENDED_TAG_UINT16, "box-uint16 should preserve the complete payload"
    ASSERT_BOX sybilant_Sbox_Duint32, 0xffffffff, 0xffffffff, SYBILANT_EXTENDED_TAG_UINT32, "box-uint32 should preserve the complete payload"

    ASSERT_BOX sybilant_Sbox_Dint8, -128, 0xff, SYBILANT_EXTENDED_TAG_INT8, "box-int8 should preserve a negative payload"
    ASSERT_BOX sybilant_Sbox_Dint8, 127, 0xff, SYBILANT_EXTENDED_TAG_INT8, "box-int8 should preserve a positive payload"
    ASSERT_BOX sybilant_Sbox_Dint16, -32768, 0xffff, SYBILANT_EXTENDED_TAG_INT16, "box-int16 should preserve a negative payload"
    ASSERT_BOX sybilant_Sbox_Dint16, 32767, 0xffff, SYBILANT_EXTENDED_TAG_INT16, "box-int16 should preserve a positive payload"
    ASSERT_BOX sybilant_Sbox_Dint32, -2147483648, 0xffffffff, SYBILANT_EXTENDED_TAG_INT32, "box-int32 should preserve a negative payload"
    ASSERT_BOX sybilant_Sbox_Dint32, 2147483647, 0xffffffff, SYBILANT_EXTENDED_TAG_INT32, "box-int32 should preserve a positive payload"

    ASSERT_BOX sybilant_Sbox_Dnat8, 127, 0xff, SYBILANT_EXTENDED_TAG_NAT8, "box-nat8 should preserve its maximum payload"
    ASSERT_BOX sybilant_Sbox_Dnat16, 32767, 0xffff, SYBILANT_EXTENDED_TAG_NAT16, "box-nat16 should preserve its maximum payload"
    ASSERT_BOX sybilant_Sbox_Dnat32, 2147483647, 0xffffffff, SYBILANT_EXTENDED_TAG_NAT32, "box-nat32 should preserve its maximum payload"

    add rsp, 8
    ret

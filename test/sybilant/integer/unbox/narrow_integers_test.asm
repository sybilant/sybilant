bits 64
default rel

%include "test/support.asm"

%macro ASSERT_UNBOX 6
    mov rdi, %2
    and edi, %3
    shl rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rdi, %4
    call %1

    mov rdx, %5
    ASSERT_EQ rax, rdx, %6
%endmacro

section .text
extern sybilant_Dunbox_Duint8
extern sybilant_Dunbox_Duint16
extern sybilant_Dunbox_Duint32
extern sybilant_Dunbox_Dint8
extern sybilant_Dunbox_Dint16
extern sybilant_Dunbox_Dint32
extern sybilant_Dunbox_Dnat8
extern sybilant_Dunbox_Dnat16
extern sybilant_Dunbox_Dnat32

testcase:
    sub rsp, 8

    ASSERT_UNBOX sybilant_Dunbox_Duint8, 0xff, 0xff, SYBILANT_EXTENDED_TAG_UINT8, 0xff, "unbox-uint8 should zero-extend its payload"
    ASSERT_UNBOX sybilant_Dunbox_Duint16, 0xffff, 0xffff, SYBILANT_EXTENDED_TAG_UINT16, 0xffff, "unbox-uint16 should zero-extend its payload"
    ASSERT_UNBOX sybilant_Dunbox_Duint32, 0xffffffff, 0xffffffff, SYBILANT_EXTENDED_TAG_UINT32, 0xffffffff, "unbox-uint32 should zero-extend its payload"

    ASSERT_UNBOX sybilant_Dunbox_Dint8, -128, 0xff, SYBILANT_EXTENDED_TAG_INT8, -128, "unbox-int8 should sign-extend a negative payload"
    ASSERT_UNBOX sybilant_Dunbox_Dint8, 127, 0xff, SYBILANT_EXTENDED_TAG_INT8, 127, "unbox-int8 should preserve a positive payload"
    ASSERT_UNBOX sybilant_Dunbox_Dint16, -32768, 0xffff, SYBILANT_EXTENDED_TAG_INT16, -32768, "unbox-int16 should sign-extend a negative payload"
    ASSERT_UNBOX sybilant_Dunbox_Dint16, 32767, 0xffff, SYBILANT_EXTENDED_TAG_INT16, 32767, "unbox-int16 should preserve a positive payload"
    ASSERT_UNBOX sybilant_Dunbox_Dint32, -2147483648, 0xffffffff, SYBILANT_EXTENDED_TAG_INT32, -2147483648, "unbox-int32 should sign-extend a negative payload"
    ASSERT_UNBOX sybilant_Dunbox_Dint32, 2147483647, 0xffffffff, SYBILANT_EXTENDED_TAG_INT32, 2147483647, "unbox-int32 should preserve a positive payload"

    ASSERT_UNBOX sybilant_Dunbox_Dnat8, 127, 0xff, SYBILANT_EXTENDED_TAG_NAT8, 127, "unbox-nat8 should zero-extend its payload"
    ASSERT_UNBOX sybilant_Dunbox_Dnat16, 32767, 0xffff, SYBILANT_EXTENDED_TAG_NAT16, 32767, "unbox-nat16 should zero-extend its payload"
    ASSERT_UNBOX sybilant_Dunbox_Dnat32, 2147483647, 0xffffffff, SYBILANT_EXTENDED_TAG_NAT32, 2147483647, "unbox-nat32 should zero-extend its payload"

    add rsp, 8
    ret

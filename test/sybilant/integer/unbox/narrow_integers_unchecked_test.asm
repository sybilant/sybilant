bits 64
default rel

%include "test/support.asm"

%macro ASSERT_UNBOX_UNCHECKED 6
    mov rdi, %2
    and edi, %3
    shl rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rdi, %4
    call %1

    mov rdx, %5
    ASSERT_EQ rax, rdx, %6
%endmacro

section .text
extern sybilant_Sunbox_Duint8_Dunchecked
extern sybilant_Sunbox_Duint16_Dunchecked
extern sybilant_Sunbox_Duint32_Dunchecked
extern sybilant_Sunbox_Dint8_Dunchecked
extern sybilant_Sunbox_Dint16_Dunchecked
extern sybilant_Sunbox_Dint32_Dunchecked
extern sybilant_Sunbox_Dnat8_Dunchecked
extern sybilant_Sunbox_Dnat16_Dunchecked
extern sybilant_Sunbox_Dnat32_Dunchecked

testcase:
    sub rsp, 8

    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Duint8_Dunchecked, 0xff, 0xff, SYBILANT_EXTENDED_TAG_INT8, 0xff, "unbox-uint8-unchecked should ignore the tag and zero-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Duint16_Dunchecked, 0xffff, 0xffff, SYBILANT_EXTENDED_TAG_INT16, 0xffff, "unbox-uint16-unchecked should ignore the tag and zero-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Duint32_Dunchecked, 0xffffffff, 0xffffffff, SYBILANT_EXTENDED_TAG_INT32, 0xffffffff, "unbox-uint32-unchecked should ignore the tag and zero-extend"

    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dint8_Dunchecked, -128, 0xff, SYBILANT_EXTENDED_TAG_UINT8, -128, "unbox-int8-unchecked should ignore the tag and sign-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dint16_Dunchecked, -32768, 0xffff, SYBILANT_EXTENDED_TAG_UINT16, -32768, "unbox-int16-unchecked should ignore the tag and sign-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dint32_Dunchecked, -2147483648, 0xffffffff, SYBILANT_EXTENDED_TAG_UINT32, -2147483648, "unbox-int32-unchecked should ignore the tag and sign-extend"

    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dnat8_Dunchecked, 127, 0xff, SYBILANT_EXTENDED_TAG_UINT8, 127, "unbox-nat8-unchecked should ignore the tag and zero-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dnat16_Dunchecked, 32767, 0xffff, SYBILANT_EXTENDED_TAG_UINT16, 32767, "unbox-nat16-unchecked should ignore the tag and zero-extend"
    ASSERT_UNBOX_UNCHECKED sybilant_Sunbox_Dnat32_Dunchecked, 2147483647, 0xffffffff, SYBILANT_EXTENDED_TAG_UINT32, 2147483647, "unbox-nat32-unchecked should ignore the tag and zero-extend"

    add rsp, 8
    ret

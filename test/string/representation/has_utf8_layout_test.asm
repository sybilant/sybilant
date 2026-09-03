bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 10
    db 0x41
    db 0xc2, 0xa2
    db 0xe2, 0x82, 0xac
    db 0xf0, 0x90, 0x8d, 0x88

section .text

testcase:
    ASSERT_EQ qword [rel string + SYBILANT_STRING_TYPE_OFFSET], SYBILANT_STRING_TYPE, "a string should identify its built-in type"
    ASSERT_EQ qword [rel string + SYBILANT_STRING_EDITOR_OFFSET], SYBILANT_NIL, "an immutable string should have a null editor"
    ASSERT_EQ qword [rel string + SYBILANT_STRING_BYTE_LENGTH_OFFSET], 10, "a string should store its byte length"
    ASSERT_EQ byte [rel string + SYBILANT_STRING_DATA_OFFSET], 0x41, "UTF-8 string data should follow the header"
    ASSERT_EQ byte [rel string + SYBILANT_STRING_DATA_OFFSET + 1], 0xc2, "string data should preserve a multibyte leading byte"
    ASSERT_EQ byte [rel string + SYBILANT_STRING_DATA_OFFSET + 9], 0x88, "string data should preserve the last continuation byte"
    ret

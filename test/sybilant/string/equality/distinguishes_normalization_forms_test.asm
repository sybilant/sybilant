bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
composed_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 2
    db 0xc3, 0xa9
align 8
decomposed_string:
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 3
    db 0x65, 0xcc, 0x81

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    lea rdi, [rel composed_string]
    lea rsi, [rel decomposed_string]
    call sybilant_S_e

    add rsp, 8

    ASSERT_EQ rax, SYBILANT_FALSE, "composed and decomposed codepoint sequences should not be equal"
    ret

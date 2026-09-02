bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
uint64_object:
    dq SYBILANT_FALSE
    dq 0xffffffffffffffff
int64_object:
    dq SYBILANT_UINT64_TYPE
    dq 0x8000000000000000
nat64_object:
    dq SYBILANT_INT64_TYPE
    dq 0x7fffffffffffffff

section .text
extern sybilant_Dunbox_Duint64_Dunchecked
extern sybilant_Dunbox_Dint64_Dunchecked
extern sybilant_Dunbox_Dnat64_Dunchecked

testcase:
    sub rsp, 8

    lea rdi, [rel uint64_object]
    call sybilant_Dunbox_Duint64_Dunchecked
    mov rdx, -1
    ASSERT_EQ rax, rdx, "unbox-uint64-unchecked should ignore an invalid type header"

    lea rdi, [rel int64_object]
    call sybilant_Dunbox_Dint64_Dunchecked
    mov rdx, 0x8000000000000000
    ASSERT_EQ rax, rdx, "unbox-int64-unchecked should ignore a mismatched type header"

    lea rdi, [rel nat64_object]
    call sybilant_Dunbox_Dnat64_Dunchecked
    mov rdx, 0x7fffffffffffffff
    ASSERT_EQ rax, rdx, "unbox-nat64-unchecked should ignore a mismatched type header"

    add rsp, 8
    ret

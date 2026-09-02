bits 64
default rel

%include "test/support.asm"

section .rodata
align 8
uint64_object:
    dq SYBILANT_UINT64_TYPE
    dq 0xffffffffffffffff
int64_object:
    dq SYBILANT_INT64_TYPE
    dq 0x8000000000000000
nat64_object:
    dq SYBILANT_NAT64_TYPE
    dq 0x7fffffffffffffff

section .text
extern sybilant_Sunbox_Duint64
extern sybilant_Sunbox_Dint64
extern sybilant_Sunbox_Dnat64

testcase:
    sub rsp, 8

    lea rdi, [rel uint64_object]
    call sybilant_Sunbox_Duint64
    mov rdx, -1
    ASSERT_EQ rax, rdx, "unbox-uint64 should return the complete payload"

    lea rdi, [rel int64_object]
    call sybilant_Sunbox_Dint64
    mov rdx, 0x8000000000000000
    ASSERT_EQ rax, rdx, "unbox-int64 should return the complete payload"

    lea rdi, [rel nat64_object]
    call sybilant_Sunbox_Dnat64
    mov rdx, 0x7fffffffffffffff
    ASSERT_EQ rax, rdx, "unbox-nat64 should return the complete payload"

    add rsp, 8
    ret

bits 64
default rel

%include "test/support.asm"

%macro ASSERT_INTEGER_EQUAL 3
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_TRUE, %3
%endmacro

%macro ASSERT_INTEGER_UNEQUAL 3
    lea rdi, [rel %1]
    lea rsi, [rel %2]
    call sybilant_S_e
    ASSERT_EQ rax, SYBILANT_FALSE, %3
%endmacro

section .rodata
align 8
uint64_a:
    dq SYBILANT_UINT64_TYPE
    dq 0xffffffffffffffff
uint64_b:
    dq SYBILANT_UINT64_TYPE
    dq 0xffffffffffffffff
uint64_other:
    dq SYBILANT_UINT64_TYPE
    dq 0xfffffffffffffffe

int64_a:
    dq SYBILANT_INT64_TYPE
    dq 0x8000000000000000
int64_b:
    dq SYBILANT_INT64_TYPE
    dq 0x8000000000000000
int64_other:
    dq SYBILANT_INT64_TYPE
    dq 0x7fffffffffffffff

nat64_a:
    dq SYBILANT_NAT64_TYPE
    dq 0x7fffffffffffffff
nat64_b:
    dq SYBILANT_NAT64_TYPE
    dq 0x7fffffffffffffff
nat64_other:
    dq SYBILANT_NAT64_TYPE
    dq 0x7ffffffffffffffe

section .text
extern sybilant_S_e

testcase:
    sub rsp, 8

    ASSERT_INTEGER_EQUAL uint64_a, uint64_b, "sybilant/= should compare boxed uint64 values"
    ASSERT_INTEGER_UNEQUAL uint64_a, uint64_other, "sybilant/= should distinguish boxed uint64 values"

    ASSERT_INTEGER_EQUAL int64_a, int64_b, "sybilant/= should compare boxed int64 values"
    ASSERT_INTEGER_UNEQUAL int64_a, int64_other, "sybilant/= should distinguish boxed int64 values"

    ASSERT_INTEGER_EQUAL nat64_a, nat64_b, "sybilant/= should compare boxed nat64 values"
    ASSERT_INTEGER_UNEQUAL nat64_a, nat64_other, "sybilant/= should distinguish boxed nat64 values"

    add rsp, 8
    ret

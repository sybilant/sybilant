bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_Dmalloc
extern sybilant_Dbox_Duint64
extern sybilant_Dbox_Dint64
extern sybilant_Dbox_Dnat64

testcase:
    push r12
    push r13
    push r14

    mov edi, 1
    call sybilant_Dmalloc

    mov rdi, -1
    call sybilant_Dbox_Duint64
    mov r12, rax

    mov rdx, SYBILANT_MALLOC_START + 1
    ASSERT_ABE r12, rdx, "box-uint64 should return an allocated object"
    mov rdx, r12
    and edx, SYBILANT_TAG_MASK
    ASSERT_EQ edx, 0, "box-uint64 should align its object"
    ASSERT_EQ qword [r12 + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], SYBILANT_UINT64_TYPE, "box-uint64 should store the uint64 type"
    ASSERT_EQ qword [r12 + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET], -1, "box-uint64 should store the complete payload"

    mov rdi, 0x8000000000000000
    call sybilant_Dbox_Dint64
    mov r13, rax

    lea rdx, [r12 + SYBILANT_BOXED_INTEGER_SIZE]
    ASSERT_ABE r13, rdx, "box-int64 should not overlap the earlier object"
    mov rdx, r13
    and edx, SYBILANT_TAG_MASK
    ASSERT_EQ edx, 0, "box-int64 should align its object"
    ASSERT_EQ qword [r13 + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], SYBILANT_INT64_TYPE, "box-int64 should store the int64 type"
    mov rdx, 0x8000000000000000
    ASSERT_EQ qword [r13 + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET], rdx, "box-int64 should store the complete payload"

    mov rdi, 0x7fffffffffffffff
    call sybilant_Dbox_Dnat64
    mov r14, rax

    lea rdx, [r13 + SYBILANT_BOXED_INTEGER_SIZE]
    ASSERT_ABE r14, rdx, "box-nat64 should not overlap the earlier object"
    mov rdx, r14
    and edx, SYBILANT_TAG_MASK
    ASSERT_EQ edx, 0, "box-nat64 should align its object"
    ASSERT_EQ qword [r14 + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], SYBILANT_NAT64_TYPE, "box-nat64 should store the nat64 type"
    mov rdx, 0x7fffffffffffffff
    ASSERT_EQ qword [r14 + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET], rdx, "box-nat64 should store the complete payload"

    pop r14
    pop r13
    pop r12
    ret

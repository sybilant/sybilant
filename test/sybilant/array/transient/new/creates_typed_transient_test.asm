bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_dthread_Sself
extern sybilant_datom_Snew
extern sybilant_darray_Stransient_Dnew_B

testcase:
    push r12
    push r13

    call sybilant_dthread_Sself
    mov r12, rax
    mov edi, SYBILANT_THREAD_TYPE
    mov rsi, r12
    call sybilant_datom_Snew
    mov r13, rax

    mov edi, SYBILANT_UINT8_TYPE
    mov esi, (3 << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32
    mov rdx, r13
    call sybilant_darray_Stransient_Dnew_B

    mov r12, rax
    test r12, r12
    setnz al
    ASSERT_EQ al, 1, "array/transient-new! should return a transient array"
    test r12, SYBILANT_TAG_MASK
    setz al
    ASSERT_EQ al, 1, "array/transient-new! should return an aligned value"

    ASSERT_EQ qword [r12 + SYBILANT_ARRAY_EDITOR_OFFSET], r13, "a transient should retain its editor atom"
    mov r13, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    ASSERT_EQ qword [r13 + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR, "a transient should have a distinct array type"
    ASSERT_EQ qword [r13 + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_UINT8_TYPE, "a transient type should contain its element type"
    ASSERT_EQ dword [r13 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET], 1, "a transient type should contain the element stride"
    ASSERT_EQ qword [r12 + SYBILANT_ARRAY_LENGTH_OFFSET], 3, "array/transient-new! should decode the boxed length"
    ASSERT_EQ byte [r12 + SYBILANT_ARRAY_DATA_OFFSET], 0, "a new transient should initialize its data"

    pop r13
    pop r12
    ret

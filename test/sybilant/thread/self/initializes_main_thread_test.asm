bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_dthread_Sself
extern sybilant_Stype

testcase:
    push r12
    push r13
    sub rsp, 8

    call sybilant_dthread_Sself
    mov r12, rax
    test r12, r12
    setnz al
    ASSERT_EQ al, 1, "sybilant.thread/self should return the main thread"

    mov rax, r12
    and eax, SYBILANT_TAG_MASK
    ASSERT_EQ eax, 0, "sybilant.thread/self should return an aligned value"

    mov rdi, r12
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_THREAD_TYPE, "the main thread should have the thread type"

    mov eax, SYS_GETTID
    syscall
    ASSERT_EQ qword [r12 + SYBILANT_THREAD_ID_OFFSET], rax, "the main thread should contain its Linux thread ID"

    mov r13, [r12 + SYBILANT_THREAD_NAME_OFFSET]
    test r13, r13
    setnz al
    ASSERT_EQ al, 1, "the main thread should have a name"
    ASSERT_EQ qword [r13 + SYBILANT_STRING_TYPE_OFFSET], SYBILANT_STRING_TYPE, "the main thread name should be a string"
    ASSERT_EQ qword [r13 + SYBILANT_STRING_EDITOR_OFFSET], SYBILANT_NIL, "the main thread name should be immutable"
    ASSERT_EQ qword [r13 + SYBILANT_STRING_BYTE_LENGTH_OFFSET], 4, "the main thread name should contain four bytes"
    ASSERT_EQ dword [r13 + SYBILANT_STRING_DATA_OFFSET], 0x6e69616d, "the main thread name should be main"

    add rsp, 8
    pop r13
    pop r12
    ret

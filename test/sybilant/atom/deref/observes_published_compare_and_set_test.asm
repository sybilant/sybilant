bits 64
default rel

%include "test/support.asm"

    MAP_SHARED equ 0x1
    PUBLISHED_VALUE_OFFSET equ SYBILANT_ATOM_SIZE
    PUBLISHED_VALUE equ 0x123456789abcdef0
    SPIN_LIMIT equ 10000000

section .bss
align 4
wait_status:
    resd 1

section .text
extern sybilant_datom_Scompare_Dand_Dset
extern sybilant_datom_Sderef

testcase:
    push r12
    push r13
    push r14

    xor edi, edi
    mov esi, PAGE_SIZE
    mov edx, PROT_READ | PROT_WRITE
    mov r10d, MAP_SHARED | MAP_ANONYMOUS
    mov r8, -1
    xor r9d, r9d
    mov eax, SYS_MMAP
    syscall
    mov r12, rax

    mov qword [r12 + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    mov qword [r12 + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR
    mov qword [r12 + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_BOOLEAN_TYPE
    mov rdx, r12
    add r12, SYBILANT_ATOM_TYPE_SIZE
    mov [r12 + SYBILANT_ATOM_TYPE_OFFSET], rdx
    mov qword [r12 + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_FALSE
    mov qword [r12 + PUBLISHED_VALUE_OFFSET], 0

    mov eax, SYS_FORK
    syscall
    test eax, eax
    jz .publisher
    mov r14d, eax

    mov r13d, SPIN_LIMIT
.await_publication:
    mov rdi, r12
    call sybilant_datom_Sderef
    cmp rax, SYBILANT_TRUE
    je .published
    pause
    dec r13d
    jnz .await_publication

    ASSERT_EQ rax, SYBILANT_TRUE, "atom/deref should observe a completed compare-and-set"

.published:
    mov rdx, PUBLISHED_VALUE
    ASSERT_EQ qword [r12 + PUBLISHED_VALUE_OFFSET], rdx, "atom/deref should acquire writes published by compare-and-set"

    mov dword [rel wait_status], -1
    mov edi, r14d
    lea rsi, [rel wait_status]
    xor edx, edx
    xor r10d, r10d
    mov eax, SYS_WAIT4
    syscall
    ASSERT_EQ dword [rel wait_status], 0, "the compare-and-set publisher should succeed"

    pop r14
    pop r13
    pop r12
    ret

.publisher:
    mov rax, PUBLISHED_VALUE
    mov [r12 + PUBLISHED_VALUE_OFFSET], rax

    mov rdi, r12
    mov esi, SYBILANT_FALSE
    mov edx, SYBILANT_TRUE
    call sybilant_datom_Scompare_Dand_Dset
    cmp rax, SYBILANT_TRUE
    jne .publisher_failed

    xor edi, edi
    mov eax, SYS_EXIT
    syscall
    ud2

.publisher_failed:
    mov edi, 1
    mov eax, SYS_EXIT
    syscall
    ud2

bits 64
default rel

%include "test/support.asm"

    MAP_SHARED equ 0x1
    READY_OFFSET equ SYBILANT_ATOM_SIZE
    START_OFFSET equ READY_OFFSET + 8
    ITERATION_COUNT equ 250000
    VALUE_INCREMENT equ 1 << SYBILANT_INTEGER_PAYLOAD_SHIFT
    EXPECTED_VALUE equ ((ITERATION_COUNT * 2) << SYBILANT_INTEGER_PAYLOAD_SHIFT) | SYBILANT_EXTENDED_TAG_NAT32

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

    mov qword [r12 + SYBILANT_ATOM_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    mov qword [r12 + SYBILANT_ATOM_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR
    mov qword [r12 + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_NAT32_TYPE
    mov rdx, r12
    add r12, SYBILANT_ATOM_TYPE_SIZE
    mov [r12 + SYBILANT_ATOM_TYPE_OFFSET], rdx
    mov qword [r12 + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_EXTENDED_TAG_NAT32
    mov qword [r12 + READY_OFFSET], 0
    mov qword [r12 + START_OFFSET], 0

    mov eax, SYS_FORK
    syscall
    test eax, eax
    jz .child
    mov r14d, eax

.await_child:
    cmp qword [r12 + READY_OFFSET], 1
    je .start
    pause
    jmp .await_child

.start:
    mov qword [r12 + START_OFFSET], 1
    call .increment

    mov dword [rel wait_status], -1
    mov edi, r14d
    lea rsi, [rel wait_status]
    xor edx, edx
    xor r10d, r10d
    mov eax, SYS_WAIT4
    syscall
    ASSERT_EQ dword [rel wait_status], 0, "the concurrent atom updater should succeed"

    mov rdi, r12
    call sybilant_datom_Sderef
    mov rdx, EXPECTED_VALUE
    ASSERT_EQ rax, rdx, "atom/compare-and-set should serialize concurrent updates"

    pop r14
    pop r13
    pop r12
    ret

.child:
    mov qword [r12 + READY_OFFSET], 1
.await_start:
    cmp qword [r12 + START_OFFSET], 1
    je .run
    pause
    jmp .await_start

.run:
    call .increment
    xor edi, edi
    mov eax, SYS_EXIT
    syscall
    ud2

.increment:
    push r13
    mov r13d, ITERATION_COUNT

.retry:
    mov rdi, r12
    call sybilant_datom_Sderef
    mov rsi, rax
    lea rdx, [rax + VALUE_INCREMENT]
    mov rdi, r12
    call sybilant_datom_Scompare_Dand_Dset
    cmp rax, SYBILANT_TRUE
    jne .retry

    dec r13d
    jnz .retry

    pop r13
    ret

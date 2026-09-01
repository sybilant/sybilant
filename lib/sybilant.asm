bits 64
default rel

%include "lib/constants.asm"

section .text
global _start
global sybilant_Dexit
global sybilant_Dmalloc
extern sybilant_Dmain

_start:
    mov rax, SYBILANT_MALLOC_START
    mov [rel sybilant_Dmalloc_Dstart], rax
    mov [rel sybilant_Dmalloc_Dmaximum], rax

    call sybilant_Dmain

    mov edi, eax
    call sybilant_Dexit
    ud2

sybilant_Dexit:
    mov eax, SYS_EXIT
    syscall
    ud2

sybilant_Dmalloc:
    test rdi, rdi
    jz .invalid_argument

    push r12
    push r13

    mov r12, [rel sybilant_Dmalloc_Dstart]
    mov r13, r12
    add r13, rdi
    jc .out_of_memory

    cmp r13, [rel sybilant_Dmalloc_Dmaximum]
    jbe .allocated

    mov rsi, r13
    add rsi, PAGE_SIZE - 1
    jc .out_of_memory
    and rsi, -PAGE_SIZE

    mov rdi, [rel sybilant_Dmalloc_Dmaximum]
    sub rsi, rdi
    mov edx, PROT_READ | PROT_WRITE
    mov r10d, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE
    mov r8, -1
    xor r9d, r9d
    mov eax, SYS_MMAP
    syscall

    cmp rax, rdi
    jne .out_of_memory

    add rdi, rsi
    mov [rel sybilant_Dmalloc_Dmaximum], rdi

.allocated:
    mov [rel sybilant_Dmalloc_Dstart], r13
    mov rax, r12

    pop r13
    pop r12
    ret

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Dexit

.out_of_memory:
    mov edi, SYBILANT_ERROR_OUT_OF_MEMORY
    jmp sybilant_Dexit

section .bss
align 8
sybilant_Dmalloc_Dstart:
    resq 1
sybilant_Dmalloc_Dmaximum:
    resq 1

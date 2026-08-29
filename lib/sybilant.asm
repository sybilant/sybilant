bits 64
default rel

; Process exit codes used by the runtime.
SYBILANT_EXIT_OUT_OF_MEMORY equ 1
SYBILANT_EXIT_INVALID_ARGUMENT equ 2
SYBILANT_EXIT_BOUNDS equ 3
SYBILANT_EXIT_CORRUPT_DATA equ 4

SYS_BRK equ 12
SYS_EXIT equ 60
ALLOC_ALIGNMENT equ 16

section .bss
align 8
global sybilant_frontier
sybilant_frontier: resq 1

section .text
global sybilant_alloc
global sybilant_exit

; Allocate rdi bytes and return a 16-byte-aligned address in rax.
; Allocations are never moved or reused. This initial allocator is intentionally
; process-global and single-threaded.
sybilant_alloc:
    test rdi, rdi
    jz .invalid_argument

    add rdi, ALLOC_ALIGNMENT - 1
    jc .out_of_memory
    and rdi, -ALLOC_ALIGNMENT
    mov rdx, rdi

    mov r10, qword sybilant_frontier
    mov r8, [r10]
    test r8, r8
    jnz .have_frontier

    xor edi, edi
    mov eax, SYS_BRK
    syscall
    test rax, rax
    jz .out_of_memory
    add rax, ALLOC_ALIGNMENT - 1
    jc .out_of_memory
    and rax, -ALLOC_ALIGNMENT
    mov r8, rax

.have_frontier:
    mov r9, r8
    add r9, rdx
    jc .out_of_memory

    mov rdi, r9
    mov eax, SYS_BRK
    syscall
    cmp rax, r9
    jne .out_of_memory

    mov [r10], r9
    mov rax, r8
    ret

.invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit

.out_of_memory:
    mov edi, SYBILANT_EXIT_OUT_OF_MEMORY
    jmp sybilant_exit

; Terminate the process with exit code rdi.
sybilant_exit:
    mov eax, SYS_EXIT
    syscall
    ud2

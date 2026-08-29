bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_frontier

section .text
global _start

_start:
    mov edi, 1
    call sybilant_alloc
    mov r12, rax
    test rax, 15
    jnz .alignment_failed

    mov r13, [sybilant_frontier]
    sub r13, r12
    cmp r13, 16
    jne .first_rounding_failed

    mov edi, 17
    call sybilant_alloc
    mov r14, rax
    cmp rax, r12
    jbe .monotonic_failed
    sub rax, r12
    cmp rax, 16
    jne .contiguous_failed
    mov r13, [sybilant_frontier]
    sub r13, r14
    cmp r13, 32
    jne .second_rounding_failed

    xor edi, edi
    jmp sybilant_exit

.alignment_failed:
    mov edi, 64
    jmp sybilant_exit
.first_rounding_failed:
    mov edi, 65
    jmp sybilant_exit
.monotonic_failed:
    mov edi, 66
    jmp sybilant_exit
.contiguous_failed:
    mov edi, 67
    jmp sybilant_exit
.second_rounding_failed:
    mov edi, 68
    jmp sybilant_exit

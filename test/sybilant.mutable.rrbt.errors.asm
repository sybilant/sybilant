bits 64
default rel

extern sybilant_exit
extern sybilant_atom_compare_and_set
extern sybilant_mutable_rrbt_get
extern sybilant_mutable_rrbt_length
extern sybilant_mutable_rrbt_new
extern sybilant_mutable_rrbt_persistent
extern sybilant_thread_current
extern sybilant_vector_empty

%include "lib/sybilant.constants.asm"

    RRBT_EDITOR_OFFSET equ 8
    INVALID_STATUS equ SYBILANT_EXIT_INVALID_ARGUMENT << 8

section .bss
wait_status: resd 1

section .text
global main
main:
    lea rdi, [sybilant_vector_empty]
    call sybilant_mutable_rrbt_new
    mov r12, rax

    ;; An atom that no longer names this thread invalidates every operation.
    mov rdi, r12
    lea rsi, [invalidate_atom]
    call expect_invalid

    ;; persistent also invalidates the former mutable handle.
    mov rdi, r12
    call sybilant_mutable_rrbt_persistent
    mov rdi, r12
    lea rsi, [sybilant_mutable_rrbt_length]
    call expect_invalid

    xor edi, edi
    jmp sybilant_exit

invalidate_atom:
    push rbx
    push r12
    push r13
    mov r12, rdi
    mov r13, [r12 + RRBT_EDITOR_OFFSET]
    call sybilant_thread_current
    mov rdi, r13
    mov rsi, rax
    mov edx, SYBILANT_NIL
    call sybilant_atom_compare_and_set
    mov rdi, r12
    pop r13
    pop r12
    pop rbx
    jmp sybilant_mutable_rrbt_length

    ;; Run rsi(tree) in a child and require invalid-argument exit status 2.
expect_invalid:
    mov r8, rdi
    mov r9, rsi
    mov eax, SYS_FORK
    syscall
    test rax, rax
    js .system_failed
    jz .child
    mov rdi, rax
    lea rsi, [wait_status]
    xor edx, edx
    xor r10d, r10d
    mov eax, SYS_WAIT4
    syscall
    test rax, rax
    js .system_failed
    cmp dword [wait_status], INVALID_STATUS
    jne .wrong_status
    ret
.child:
    mov rdi, r8
    call r9
    mov edi, 126
    jmp sybilant_exit
.system_failed:
    mov edi, 64
    jmp sybilant_exit
.wrong_status:
    mov edi, 65
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

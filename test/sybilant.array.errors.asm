bits 64
default rel

extern sybilant_array_delete
extern sybilant_array_empty
extern sybilant_array_get
extern sybilant_array_insert
extern sybilant_array_set
extern sybilant_array_slice
extern sybilant_exit

%include "lib/sybilant.constants.asm"

    BOUNDS_STATUS equ SYBILANT_EXIT_BOUNDS << 8

section .bss
wait_status: resd 1

section .text
global main
main:
    lea rsi, [sybilant_array_empty]
    lea rdi, [sybilant_array_get]
    xor edx, edx
    call expect_bounds

    lea rsi, [sybilant_array_empty]
    lea rdi, [sybilant_array_slice]
    mov edx, 1
    call expect_bounds

    lea rsi, [sybilant_array_empty]
    lea rdi, [sybilant_array_delete]
    xor edx, edx
    call expect_bounds

    lea rsi, [sybilant_array_empty]
    lea rdi, [sybilant_array_set]
    xor edx, edx
    call expect_bounds

    lea rsi, [sybilant_array_empty]
    lea rdi, [sybilant_array_insert]
    mov edx, 1
    call expect_bounds

    xor edi, edi
    jmp sybilant_exit

    ;; Run an array operation in a child and require bounds-error exit status 3.
    ;; rdi = operation address, rsi = array, rdx = index
expect_bounds:
    mov r8, rdi
    mov r9, rsi
    mov r10, rdx
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
    cmp dword [wait_status], BOUNDS_STATUS
    jne .wrong_status
    ret

.child:
    mov rdi, r9
    mov rsi, r10
    xor edx, edx
    call r8
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

bits 64
default rel

extern sybilant_array_set
extern sybilant_exit
extern sybilant_mutable_array_capacity
extern sybilant_mutable_array_get
extern sybilant_mutable_array_insert
extern sybilant_mutable_array_new
extern sybilant_mutable_array_persistent
extern sybilant_mutable_array_reserve
extern sybilant_mutable_array_set

%include "lib/sybilant.constants.asm"

section .rodata
align 16
sybilant_test_thread:
    dq SYBILANT_THREAD_TYPE, 0

section .text
global sybilant_thread_current
sybilant_thread_current:
    lea rax, [sybilant_test_thread]
    ret

global _start
_start:
    mov edi, 2
    mov esi, SYBILANT_NIL
    call sybilant_mutable_array_new
    mov r12, rax
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], 2
    jne .new_failed
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET], 2
    jne .new_failed
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET], SYBILANT_NIL
    jne .new_failed

    mov rdi, r12
    xor esi, esi
    mov edx, 10
    call sybilant_mutable_array_set
    mov rdi, r12
    xor esi, esi
    call sybilant_mutable_array_get
    cmp rax, 10
    jne .set_failed

    mov rdi, r12
    mov esi, 2
    mov edx, 20
    call sybilant_mutable_array_insert
    mov r12, rax
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], 3
    jne .growth_failed
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET], 4
    jne .growth_failed

    mov rdi, r12
    mov esi, 9
    call sybilant_mutable_array_reserve
    mov r12, rax
    cmp qword [rax + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET], 16
    jne .reserve_failed

    mov rdi, r12
    call sybilant_mutable_array_persistent
    cmp qword [rax], SYBILANT_ARRAY_TYPE
    jne .persistent_failed
    cmp qword [rax + SYBILANT_ARRAY_EDITOR_OFFSET], 0
    jne .persistent_failed
    cmp qword [rax + SYBILANT_ARRAY_CAPACITY_OFFSET], 16
    jne .persistent_failed

    mov rdi, rax
    xor esi, esi
    mov edx, 30
    call sybilant_array_set
    cmp qword [rax + SYBILANT_ARRAY_CAPACITY_OFFSET], 3
    jne .trim_failed

    xor edi, edi
    jmp sybilant_exit

.new_failed: mov edi, 64
    jmp sybilant_exit
.set_failed: mov edi, 65
    jmp sybilant_exit
.growth_failed: mov edi, 66
    jmp sybilant_exit
.reserve_failed: mov edi, 67
    jmp sybilant_exit
.persistent_failed: mov edi, 68
    jmp sybilant_exit
.trim_failed: mov edi, 69
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

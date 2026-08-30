bits 64
default rel

extern sybilant_array_empty
extern sybilant_atom_compare_and_set
extern sybilant_atom_deref
extern sybilant_atom_new
extern sybilant_atom_p
extern sybilant_exit

%include "lib/sybilant.constants.asm"

section .text
global main
main:
    mov edi, 10
    call sybilant_atom_new
    mov r12, rax
    test rax, 15
    jnz .alignment_failed
    cmp qword [rax], SYBILANT_ATOM_TYPE
    jne .header_failed

    mov rdi, r12
    call sybilant_atom_p
    cmp rax, SYBILANT_TRUE
    jne .atom_predicate_failed
    mov edi, SYBILANT_FALSE
    call sybilant_atom_p
    cmp rax, SYBILANT_FALSE
    jne .immediate_predicate_failed
    lea rdi, [sybilant_array_empty]
    call sybilant_atom_p
    cmp rax, SYBILANT_FALSE
    jne .array_predicate_failed

    mov rdi, r12
    call sybilant_atom_deref
    cmp rax, 10
    jne .initial_value_failed

    mov rdi, r12
    mov esi, 10
    mov edx, 20
    call sybilant_atom_compare_and_set
    cmp rax, SYBILANT_TRUE
    jne .successful_cas_failed
    mov rdi, r12
    call sybilant_atom_deref
    cmp rax, 20
    jne .updated_value_failed

    mov rdi, r12
    mov esi, 10
    mov edx, 30
    call sybilant_atom_compare_and_set
    cmp rax, SYBILANT_FALSE
    jne .failed_cas_result_failed
    mov rdi, r12
    call sybilant_atom_deref
    cmp rax, 20
    jne .failed_cas_changed_value

    xor edi, edi
    jmp sybilant_exit

.alignment_failed: mov edi, 64
    jmp sybilant_exit
.header_failed: mov edi, 65
    jmp sybilant_exit
.initial_value_failed: mov edi, 66
    jmp sybilant_exit
.successful_cas_failed: mov edi, 67
    jmp sybilant_exit
.updated_value_failed: mov edi, 68
    jmp sybilant_exit
.failed_cas_result_failed: mov edi, 69
    jmp sybilant_exit
.failed_cas_changed_value: mov edi, 70
    jmp sybilant_exit
.atom_predicate_failed: mov edi, 71
    jmp sybilant_exit
.immediate_predicate_failed: mov edi, 72
    jmp sybilant_exit
.array_predicate_failed: mov edi, 73
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

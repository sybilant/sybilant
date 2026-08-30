bits 64
default rel

extern sybilant_alloc
extern sybilant_atom_new
extern sybilant_exit
extern sybilant_frontier
extern sybilant_type
extern sybilant_type_p

%include "lib/sybilant.constants.asm"

section .text
global main
main:
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

    mov edi, SYBILANT_NIL
    call sybilant_type
    cmp rax, SYBILANT_NIL
    jne .nil_type_failed
    mov edi, SYBILANT_FALSE
    call sybilant_type
    cmp rax, SYBILANT_BOOLEAN_TYPE
    jne .false_type_failed
    mov edi, SYBILANT_TRUE
    call sybilant_type
    cmp rax, SYBILANT_BOOLEAN_TYPE
    jne .true_type_failed
    mov edi, SYBILANT_CODEPOINT
    call sybilant_type
    cmp rax, SYBILANT_CODEPOINT_TYPE
    jne .codepoint_type_failed
    mov edi, SYBILANT_ATOM_TYPE
    call sybilant_type
    cmp rax, SYBILANT_TYPE_TYPE
    jne .type_type_failed
    mov edi, SYBILANT_NIL
    call sybilant_atom_new
    mov rdi, rax
    call sybilant_type
    cmp rax, SYBILANT_ATOM_TYPE
    jne .dynamic_type_failed
    mov rdi, SYBILANT_TRUE
    mov rsi, SYBILANT_BOOLEAN_TYPE
    call sybilant_type_p
    cmp rax, SYBILANT_TRUE
    jne .matching_type_predicate_failed
    mov rdi, SYBILANT_TRUE
    mov rsi, SYBILANT_CODEPOINT_TYPE
    call sybilant_type_p
    cmp rax, SYBILANT_FALSE
    jne .nonmatching_type_predicate_failed

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
.nil_type_failed:
    mov edi, 69
    jmp sybilant_exit
.false_type_failed:
    mov edi, 70
    jmp sybilant_exit
.true_type_failed:
    mov edi, 71
    jmp sybilant_exit
.codepoint_type_failed:
    mov edi, 72
    jmp sybilant_exit
.type_type_failed:
    mov edi, 73
    jmp sybilant_exit
.dynamic_type_failed:
    mov edi, 74
    jmp sybilant_exit
.matching_type_predicate_failed:
    mov edi, 75
    jmp sybilant_exit
.nonmatching_type_predicate_failed:
    mov edi, 76
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

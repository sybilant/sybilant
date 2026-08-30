bits 64
default rel

extern sybilant_array_concat
extern sybilant_array_delete
extern sybilant_array_empty
extern sybilant_array_get
extern sybilant_array_insert
extern sybilant_array_length
extern sybilant_array_p
extern sybilant_array_set
extern sybilant_array_slice
extern sybilant_atom_new
extern sybilant_exit

%include "lib/sybilant.constants.asm"

section .text
global main
main:
    lea rax, [sybilant_array_empty]
    lea rcx, [sybilant_array_empty]
    cmp rax, rcx
    jne .empty_failed
    cmp qword [rax], SYBILANT_ARRAY_TYPE
    jne .header_failed
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .header_failed

    mov rdi, rax
    call sybilant_array_p
    cmp rax, SYBILANT_TRUE
    jne .array_predicate_failed
    mov edi, SYBILANT_NIL
    call sybilant_array_p
    cmp rax, SYBILANT_FALSE
    jne .immediate_predicate_failed
    mov edi, SYBILANT_NIL
    call sybilant_atom_new
    mov rdi, rax
    call sybilant_array_p
    cmp rax, SYBILANT_FALSE
    jne .atom_predicate_failed

    lea rdi, [sybilant_array_empty]
    xor esi, esi
    mov edx, 10
    call sybilant_array_insert
    mov rdi, rax
    mov esi, 1
    mov edx, 20
    call sybilant_array_insert
    mov rdi, rax
    mov esi, 2
    mov edx, 30
    call sybilant_array_insert
    mov r12, rax
    mov rdi, r12
    call sybilant_array_length
    cmp rax, 3
    jne .length_failed

    mov rdi, r12
    mov rsi, r12
    call sybilant_array_concat
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 6
    jne .concat_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET], 10
    jne .concat_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET + 16], 30
    jne .concat_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET + 24], 10
    jne .concat_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET + 40], 30
    jne .concat_failed

    mov rdi, r12
    mov rsi, -2
    call sybilant_array_get
    cmp rax, 20
    jne .negative_get_failed

    mov rdi, r12
    mov rsi, -2
    mov edx, 15
    call sybilant_array_insert
    mov r13, rax
    cmp qword [r13 + SYBILANT_ARRAY_VALUES_OFFSET], 10
    jne .insert_failed
    cmp qword [r13 + SYBILANT_ARRAY_VALUES_OFFSET + 8], 15
    jne .insert_failed
    cmp qword [r13 + SYBILANT_ARRAY_VALUES_OFFSET + 16], 20
    jne .insert_failed

    mov rdi, r12
    mov rsi, -2
    call sybilant_array_delete
    mov r13, rax
    cmp qword [r13 + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    jne .delete_failed
    cmp qword [r13 + SYBILANT_ARRAY_VALUES_OFFSET], 10
    jne .delete_failed
    cmp qword [r13 + SYBILANT_ARRAY_VALUES_OFFSET + 8], 30
    jne .delete_failed

    mov rdi, r12
    mov rsi, -3
    mov rdx, -1
    call sybilant_array_slice
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    jne .slice_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET], 10
    jne .slice_failed
    cmp qword [rax + SYBILANT_ARRAY_VALUES_OFFSET + 8], 20
    jne .slice_failed

    mov rdi, r12
    mov rsi, -2
    mov rdx, -2
    call sybilant_array_slice
    lea rcx, [sybilant_array_empty]
    cmp rax, rcx
    jne .slice_empty_failed

    mov rdi, r12
    mov rsi, -2
    mov edx, 99
    call sybilant_array_set
    mov r13, rax
    mov rdi, r13
    mov esi, 1
    call sybilant_array_get
    cmp rax, 99
    jne .set_failed
    mov rdi, r12
    mov esi, 1
    call sybilant_array_get
    cmp rax, 20
    jne .original_changed

    xor edi, edi
    jmp sybilant_exit

.empty_failed: mov edi, 64
    jmp sybilant_exit
.header_failed: mov edi, 65
    jmp sybilant_exit
.length_failed: mov edi, 66
    jmp sybilant_exit
.insert_failed: mov edi, 71
    jmp sybilant_exit
.set_failed: mov edi, 72
    jmp sybilant_exit
.original_changed: mov edi, 73
    jmp sybilant_exit
.delete_failed: mov edi, 75
    jmp sybilant_exit
.slice_failed: mov edi, 76
    jmp sybilant_exit
.slice_empty_failed: mov edi, 77
    jmp sybilant_exit
.negative_get_failed: mov edi, 80
    jmp sybilant_exit
.concat_failed: mov edi, 81
    jmp sybilant_exit
.array_predicate_failed: mov edi, 82
    jmp sybilant_exit
.immediate_predicate_failed: mov edi, 83
    jmp sybilant_exit
.atom_predicate_failed: mov edi, 84
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

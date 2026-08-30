bits 64
default rel

extern sybilant_list_empty
extern sybilant_vector_empty
extern sybilant_rrbt_insert
extern sybilant_rrbt_delete
extern sybilant_rrbt_slice
extern sybilant_rrbt_get
extern sybilant_rrbt_set
extern sybilant_rrbt_concat
extern sybilant_rrbt_length
extern sybilant_exit
extern sybilant_type

%include "lib/sybilant.constants.asm"

    RRBT_EDITOR_OFFSET equ 8
    RRBT_LENGTH_OFFSET equ 16
    RRBT_HEIGHT_OFFSET equ 24
    RRBT_ROOT_OFFSET equ 32

section .text
global main

main:
    lea r12, [sybilant_vector_empty]
    xor r13d, r13d
.build:
    cmp r13, 40
    je .built
    mov rdi, r12
    mov rsi, r13
    lea rdx, [r13 + 100]
    call sybilant_rrbt_insert
    mov r12, rax
    inc r13
    jmp .build
.built:
    mov rdi, r12
    call sybilant_type
    cmp rax, SYBILANT_VECTOR_TYPE
    jne .type_failed

    mov rdi, r12
    call sybilant_rrbt_length
    cmp rax, 40
    jne .length_failed

    ;; Inserting into the second leaf copies that leaf and the root, while the
    ;; first leaf remains shared.
    mov rdi, r12
    mov esi, 35
    mov edx, 777
    call sybilant_rrbt_insert
    mov r15, rax
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rcx, [r15 + RRBT_ROOT_OFFSET]
    cmp rax, rcx
    je .insert_root_shared_failed
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rax, rcx
    jne .insert_subtree_shared_failed
    mov rdi, r15
    mov esi, 35
    call sybilant_rrbt_get
    cmp rax, 777
    jne .insert_value_failed

    ;; Inserting into the full first leaf splits it; the old second leaf is
    ;; still reused as the new root's third child.
    mov rdi, r12
    mov esi, 10
    mov edx, 888
    call sybilant_rrbt_insert
    mov r15, rax
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET + 16]
    mov rcx, [r15 + RRBT_ROOT_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET + 32]
    cmp rax, rcx
    jne .insert_split_shared_failed
    mov rdi, r15
    mov esi, 10
    call sybilant_rrbt_get
    cmp rax, 888
    jne .insert_split_value_failed

    mov rdi, r12
    mov esi, 32
    call sybilant_rrbt_get
    cmp rax, 132
    jne .get_failed

    ;; Concatenation rewrites only the touching leaves and their root. The
    ;; leftmost leaf of the left input and rightmost leaf of the right input
    ;; remain shared.
    mov rdi, r12
    mov rsi, r12
    call sybilant_rrbt_concat
    mov r15, rax
    cmp qword [r15 + RRBT_LENGTH_OFFSET], 80
    jne .concat_boundary_failed
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rcx, [r15 + RRBT_ROOT_OFFSET]
    mov rdx, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rdx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    jne .concat_left_shared_failed
    mov rdx, [rax + SYBILANT_ARRAY_VALUES_OFFSET + 16]
    cmp rdx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET + 48]
    jne .concat_right_shared_failed
    mov rdi, r15
    mov esi, 40
    call sybilant_rrbt_get
    cmp rax, 100
    jne .concat_boundary_failed
    mov rdi, r12
    mov rsi, -1
    call sybilant_rrbt_get
    cmp rax, 139
    jne .negative_failed

    mov rdi, r12
    mov esi, 32
    mov edx, 999
    call sybilant_rrbt_set
    mov r14, rax
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rcx, [r14 + RRBT_ROOT_OFFSET]
    cmp rax, rcx
    je .root_shared_failed
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rax, rcx
    jne .subtree_shared_failed
    mov rdi, r14
    mov esi, 32
    call sybilant_rrbt_get
    cmp rax, 999
    jne .set_failed
    mov rdi, r12
    mov esi, 32
    call sybilant_rrbt_get
    cmp rax, 132
    jne .immutable_failed

    mov rdi, r12
    mov esi, 10
    call sybilant_rrbt_delete
    mov r14, rax
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rcx, [r14 + RRBT_ROOT_OFFSET]
    cmp rax, rcx
    je .delete_root_shared_failed
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET + 16]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET + 16]
    cmp rax, rcx
    jne .delete_subtree_shared_failed
    mov rdi, r14
    mov esi, 10
    call sybilant_rrbt_get
    cmp rax, 111
    jne .delete_failed

    mov rdi, r12
    xor esi, esi
    mov edx, 35
    call sybilant_rrbt_slice
    mov r15, rax
    cmp qword [r15 + RRBT_LENGTH_OFFSET], 35
    jne .slice_shared_failed
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [r15 + RRBT_ROOT_OFFSET]
    cmp rax, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    jne .slice_shared_failed
    mov rdi, r15
    mov esi, 34
    call sybilant_rrbt_get
    cmp rax, 134
    jne .slice_shared_failed

    mov rdi, r12
    mov esi, 10
    mov edx, 10
    call sybilant_rrbt_slice
    lea rcx, [sybilant_vector_empty]
    cmp rax, rcx
    jne .slice_empty_failed

    mov rdi, r12
    xor esi, esi
    mov edx, 10
    call sybilant_rrbt_slice
    mov r14, rax
    mov rdi, r14
    call sybilant_rrbt_length
    cmp rax, 10
    jne .slice_failed

    mov rdi, r14
    mov rsi, r14
    call sybilant_rrbt_concat
    mov rdi, rax
    call sybilant_rrbt_length
    cmp rax, 20
    jne .concat_failed

    ;; Fill an entire height-1 root, then force its final leaf and the root to
    ;; split. The leftmost leaf must remain shared through the new height.
    lea r12, [sybilant_vector_empty]
    xor r13d, r13d
.build_deep:
    cmp r13, 1024
    je .deep_full
    mov rdi, r12
    mov rsi, r13
    mov rdx, r13
    call sybilant_rrbt_insert
    mov r12, rax
    inc r13
    jmp .build_deep
.deep_full:
    mov rdi, r12
    mov rsi, r12
    call sybilant_rrbt_concat
    mov r14, rax
    cmp qword [r14 + RRBT_LENGTH_OFFSET], 2048
    jne .concat_deep_failed
    cmp qword [r14 + RRBT_HEIGHT_OFFSET], 2
    jne .concat_deep_failed
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [r14 + RRBT_ROOT_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rax, rcx
    jne .concat_deep_shared_failed

    mov rdi, r12
    mov esi, 1024
    mov edx, 1024
    call sybilant_rrbt_insert
    mov r15, rax
    cmp qword [r15 + RRBT_HEIGHT_OFFSET], 2
    jne .insert_root_split_failed
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [r15 + RRBT_ROOT_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, [rcx + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rax, rcx
    jne .insert_deep_shared_failed
    mov rdi, r15
    mov esi, 1024
    call sybilant_rrbt_get
    cmp rax, 1024
    jne .insert_deep_value_failed

    mov rdi, r15
    mov esi, 1024
    call sybilant_rrbt_delete
    mov r14, rax
    cmp qword [r14 + RRBT_LENGTH_OFFSET], 1024
    jne .delete_collapse_failed
    cmp qword [r14 + RRBT_HEIGHT_OFFSET], 1
    jne .delete_collapse_failed
    mov rax, [r15 + RRBT_ROOT_OFFSET]
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    cmp rax, [r14 + RRBT_ROOT_OFFSET]
    jne .delete_collapse_shared_failed
    mov rdi, r14
    mov rsi, -1
    call sybilant_rrbt_get
    cmp rax, 1023
    jne .delete_collapse_value_failed

    ;; Lists share the RRBT implementation and preserve their runtime type.
    lea rdi, [sybilant_list_empty]
    xor esi, esi
    mov edx, 42
    call sybilant_rrbt_insert
    mov r12, rax
    cmp qword [rax], SYBILANT_LIST_TYPE
    jne .list_type_failed
    cmp qword [rax + RRBT_EDITOR_OFFSET], 0
    jne .editor_failed
    mov rdi, r12
    xor esi, esi
    call sybilant_rrbt_get
    cmp rax, 42
    jne .list_value_failed
    mov rdi, r12
    xor esi, esi
    call sybilant_rrbt_delete
    lea rcx, [sybilant_list_empty]
    cmp rax, rcx
    jne .list_empty_failed

    xor edi, edi
    jmp sybilant_exit
.length_failed: mov edi, 64
    jmp sybilant_exit
.get_failed: mov edi, 65
    jmp sybilant_exit
.negative_failed: mov edi, 66
    jmp sybilant_exit
.set_failed: mov edi, 67
    jmp sybilant_exit
.immutable_failed: mov edi, 68
    jmp sybilant_exit
.delete_failed: mov edi, 69
    jmp sybilant_exit
.slice_failed: mov edi, 70
    jmp sybilant_exit
.concat_failed: mov edi, 71
    jmp sybilant_exit
.root_shared_failed: mov edi, 72
    jmp sybilant_exit
.subtree_shared_failed: mov edi, 73
    jmp sybilant_exit
.insert_root_shared_failed: mov edi, 74
    jmp sybilant_exit
.insert_subtree_shared_failed: mov edi, 75
    jmp sybilant_exit
.insert_value_failed: mov edi, 76
    jmp sybilant_exit
.insert_split_shared_failed: mov edi, 77
    jmp sybilant_exit
.insert_split_value_failed: mov edi, 78
    jmp sybilant_exit
.insert_root_split_failed: mov edi, 79
    jmp sybilant_exit
.insert_deep_shared_failed: mov edi, 80
    jmp sybilant_exit
.insert_deep_value_failed: mov edi, 81
    jmp sybilant_exit
.delete_root_shared_failed: mov edi, 82
    jmp sybilant_exit
.delete_subtree_shared_failed: mov edi, 83
    jmp sybilant_exit
.delete_collapse_failed: mov edi, 84
    jmp sybilant_exit
.delete_collapse_shared_failed: mov edi, 85
    jmp sybilant_exit
.delete_collapse_value_failed: mov edi, 86
    jmp sybilant_exit
.concat_boundary_failed: mov edi, 87
    jmp sybilant_exit
.concat_left_shared_failed: mov edi, 88
    jmp sybilant_exit
.concat_right_shared_failed: mov edi, 89
    jmp sybilant_exit
.concat_deep_failed: mov edi, 90
    jmp sybilant_exit
.concat_deep_shared_failed: mov edi, 91
    jmp sybilant_exit
.slice_shared_failed: mov edi, 92
    jmp sybilant_exit
.slice_empty_failed: mov edi, 93
    jmp sybilant_exit
.type_failed: mov edi, 94
    jmp sybilant_exit
.list_type_failed: mov edi, 95
    jmp sybilant_exit
.list_value_failed: mov edi, 96
    jmp sybilant_exit
.list_empty_failed: mov edi, 97
    jmp sybilant_exit
.editor_failed: mov edi, 98
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

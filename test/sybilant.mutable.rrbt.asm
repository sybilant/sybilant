bits 64
default rel

extern sybilant_atom_deref
extern sybilant_exit
extern sybilant_list_empty
extern sybilant_main_thread
extern sybilant_mutable_rrbt_concat
extern sybilant_mutable_rrbt_delete
extern sybilant_mutable_rrbt_get
extern sybilant_mutable_rrbt_insert
extern sybilant_mutable_rrbt_length
extern sybilant_mutable_rrbt_new
extern sybilant_mutable_rrbt_p
extern sybilant_mutable_rrbt_persistent
extern sybilant_mutable_rrbt_set
extern sybilant_rrbt_get
extern sybilant_rrbt_insert
extern sybilant_thread_current
extern sybilant_vector_empty

%include "lib/sybilant.constants.asm"

    RRBT_EDITOR_OFFSET equ 8
    RRBT_LENGTH_OFFSET equ 16
    RRBT_HEIGHT_OFFSET equ 24
    RRBT_ROOT_OFFSET   equ 32
    RRBT_HEAD_OFFSET   equ 40
    RRBT_TAIL_OFFSET   equ 48

section .text
global main
main:
    ;; Conversion installs one edit atom on the tree, root, head, and tail.
    lea r12, [sybilant_vector_empty]
    xor r13d, r13d
.build_source:
    cmp r13, 70
    je .source_ready
    mov rdi, r12
    mov rsi, r13
    mov rdx, r13
    call sybilant_rrbt_insert
    mov r12, rax
    inc r13
    jmp .build_source
.source_ready:
    mov rdi, r12
    call sybilant_mutable_rrbt_new
    mov r13, rax
    mov r15, [r13 + RRBT_EDITOR_OFFSET]
    cmp qword [r15], SYBILANT_ATOM_TYPE
    jne .atom_failed
    mov rdi, r15
    call sybilant_atom_deref
    lea rcx, [sybilant_main_thread]
    cmp rax, rcx
    jne .atom_failed
    mov rdi, r13
    call sybilant_mutable_rrbt_p
    cmp rax, SYBILANT_TRUE
    jne .predicate_failed
    cmp qword [r13], SYBILANT_MUTABLE_VECTOR_TYPE
    jne .type_failed
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    cmp rax, [r13 + RRBT_ROOT_OFFSET]
    je .initial_editors_failed
    mov rax, [r13 + RRBT_ROOT_OFFSET]
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .initial_editors_failed
    mov rax, [r13 + RRBT_HEAD_OFFSET]
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .initial_editors_failed
    mov rax, [r13 + RRBT_TAIL_OFFSET]
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .initial_editors_failed

    ;; Appends and prepends retain their edge buffers and edit them in place.
    mov rdi, r13
    mov esi, 70
    mov edx, 70
    call sybilant_mutable_rrbt_insert
    cmp rax, r13
    jne .identity_failed
    mov rax, [r13 + RRBT_TAIL_OFFSET]
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .tail_editor_failed
    mov r14, rax
    mov rdi, r13
    mov esi, 71
    mov edx, 71
    call sybilant_mutable_rrbt_insert
    cmp r14, [r13 + RRBT_TAIL_OFFSET]
    jne .tail_identity_failed

    mov rdi, r13
    xor esi, esi
    mov rdx, -1
    call sybilant_mutable_rrbt_insert
    mov rax, [r13 + RRBT_HEAD_OFFSET]
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .head_editor_failed
    mov r14, rax
    mov rdi, r13
    xor esi, esi
    mov rdx, -2
    call sybilant_mutable_rrbt_insert
    cmp r14, [r13 + RRBT_HEAD_OFFSET]
    jne .head_identity_failed
    mov rdi, r13
    xor esi, esi
    call sybilant_mutable_rrbt_get
    cmp rax, -2
    jne .edge_value_failed
    mov rdi, r13
    mov rsi, -1
    call sybilant_mutable_rrbt_get
    cmp rax, 71
    jne .edge_value_failed

    ;; An interior edit promotes both buffers and creates atom-owned paths.
    mov rdi, r13
    mov esi, 35
    mov edx, 999
    call sybilant_mutable_rrbt_insert
    mov rax, [r13 + RRBT_HEAD_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .promotion_failed
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .promotion_failed
    mov rax, [r13 + RRBT_TAIL_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .promotion_failed
    cmp [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .promotion_failed
    mov r14, [r13 + RRBT_ROOT_OFFSET]
    cmp [r14 + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .root_editor_failed
    mov rdi, r13
    mov esi, 35
    call sybilant_mutable_rrbt_get
    cmp rax, 999
    jne .interior_insert_failed

    ;; Once a path is editable, associative updates reuse it.
    mov rdi, r13
    mov esi, 35
    mov edx, 1000
    call sybilant_mutable_rrbt_set
    cmp r14, [r13 + RRBT_ROOT_OFFSET]
    jne .root_identity_failed
    mov rdi, r13
    mov esi, 35
    call sybilant_mutable_rrbt_get
    cmp rax, 1000
    jne .set_failed
    mov rdi, r13
    mov esi, 35
    call sybilant_mutable_rrbt_delete
    cmp rax, r13
    jne .identity_failed
    mov rdi, r13
    mov esi, 35
    call sybilant_mutable_rrbt_get
    cmp rax, 33
    jne .delete_failed

    ;; The persistent source remains unchanged.
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 70
    jne .source_changed
    mov rdi, r12
    mov esi, 35
    call sybilant_rrbt_get
    cmp rax, 35
    jne .source_changed

    ;; Concatenation retains tail insertion rather than flattening the tree.
    lea r14, [sybilant_vector_empty]
    xor ebx, ebx
.build_right:
    cmp rbx, 3
    je .right_ready
    mov rdi, r14
    mov rsi, rbx
    lea rdx, [rbx + 800]
    call sybilant_rrbt_insert
    mov r14, rax
    inc rbx
    jmp .build_right
.right_ready:
    mov rdi, r13
    mov rsi, r14
    call sybilant_mutable_rrbt_concat
    cmp rax, r13
    jne .identity_failed
    mov rdi, r13
    mov rsi, -1
    call sybilant_mutable_rrbt_get
    cmp rax, 802
    jne .concat_failed

    ;; Build past two branch levels, then fill and promote a head.
    lea rdi, [sybilant_vector_empty]
    call sybilant_mutable_rrbt_new
    mov r12, rax
    xor r13d, r13d
.append_many:
    cmp r13, 1100
    je .prepend_many_start
    mov rdi, r12
    mov rsi, r13
    mov rdx, r13
    call sybilant_mutable_rrbt_insert
    inc r13
    jmp .append_many
.prepend_many_start:
    xor r13d, r13d
.prepend_many:
    cmp r13, 40
    je .many_ready
    mov rdi, r12
    xor esi, esi
    lea rdx, [r13 + 2000]
    call sybilant_mutable_rrbt_insert
    inc r13
    jmp .prepend_many
.many_ready:
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 1140
    jne .deep_failed
    cmp qword [r12 + RRBT_HEIGHT_OFFSET], 2
    jb .deep_failed
    mov rax, [r12 + RRBT_HEAD_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 8
    jne .deep_failed
    xor r13d, r13d
.check_prepends:
    cmp r13, 40
    je .check_appends_start
    mov rdi, r12
    mov rsi, r13
    call sybilant_mutable_rrbt_get
    mov rcx, 2039
    sub rcx, r13
    cmp rax, rcx
    jne .deep_failed
    inc r13
    jmp .check_prepends
.check_appends_start:
    xor r13d, r13d
.check_appends:
    cmp r13, 1100
    je .edit_regions
    mov rdi, r12
    lea rsi, [r13 + 40]
    call sybilant_mutable_rrbt_get
    cmp rax, r13
    jne .deep_failed
    inc r13
    jmp .check_appends

.edit_regions:
    mov rdi, r12
    xor esi, esi
    mov edx, 3000
    call sybilant_mutable_rrbt_set
    mov rdi, r12
    mov esi, 50
    mov edx, 3001
    call sybilant_mutable_rrbt_set
    mov rdi, r12
    mov rsi, -1
    mov edx, 3002
    call sybilant_mutable_rrbt_set
    mov rdi, r12
    xor esi, esi
    call sybilant_mutable_rrbt_delete
    mov rdi, r12
    mov esi, 49
    call sybilant_mutable_rrbt_delete
    mov rdi, r12
    mov rsi, -1
    call sybilant_mutable_rrbt_delete
    mov rdi, r12
    call sybilant_mutable_rrbt_length
    cmp rax, 1137
    jne .region_edit_failed

    ;; Persistence clears the shared atom without walking or copying the tree.
    mov r15, [r12 + RRBT_EDITOR_OFFSET]
    mov r14, [r12 + RRBT_ROOT_OFFSET]
    mov rdi, r12
    call sybilant_mutable_rrbt_persistent
    cmp rax, r12
    jne .persistent_failed
    cmp qword [r12], SYBILANT_VECTOR_TYPE
    jne .persistent_failed
    mov rdi, r15
    call sybilant_atom_deref
    cmp rax, SYBILANT_NIL
    jne .persistent_failed
    cmp r14, [r12 + RRBT_ROOT_OFFSET]
    jne .persistent_failed
    cmp [r14 + SYBILANT_ARRAY_EDITOR_OFFSET], r15
    jne .persistent_failed
    mov rdi, r12
    mov esi, 100
    call sybilant_rrbt_get
    mov rdi, r12
    mov rsi, [r12 + RRBT_LENGTH_OFFSET]
    mov edx, 4000
    call sybilant_rrbt_insert
    cmp rax, r12
    je .persistent_failed
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 1137
    jne .persistent_failed
    mov rdi, rax
    mov rsi, -1
    call sybilant_rrbt_get
    cmp rax, 4000
    jne .persistent_failed

    ;; Removing every root leaf updates cumulative sizes and collapses height.
    lea rdi, [sybilant_vector_empty]
    call sybilant_mutable_rrbt_new
    mov r12, rax
    xor r13d, r13d
.build_delete_tree:
    cmp r13, 1050
    je .delete_tree_ready
    mov rdi, r12
    mov rsi, r13
    mov rdx, r13
    call sybilant_mutable_rrbt_insert
    inc r13
    jmp .build_delete_tree
.delete_tree_ready:
    xor r13d, r13d
.delete_tree:
    cmp r13, 1050
    je .delete_tree_empty
    mov rdi, r12
    xor esi, esi
    call sybilant_mutable_rrbt_get
    cmp rax, r13
    jne .delete_collapse_failed
    mov rdi, r12
    xor esi, esi
    call sybilant_mutable_rrbt_delete
    inc r13
    jmp .delete_tree
.delete_tree_empty:
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 0
    jne .delete_collapse_failed
    cmp qword [r12 + RRBT_HEIGHT_OFFSET], 0
    jne .delete_collapse_failed

    ;; Lists use the same implementation and return to their persistent type.
    lea rdi, [sybilant_list_empty]
    call sybilant_mutable_rrbt_new
    mov r12, rax
    cmp qword [r12], SYBILANT_MUTABLE_LIST_TYPE
    jne .list_failed
    mov rdi, r12
    xor esi, esi
    mov edx, 42
    call sybilant_mutable_rrbt_insert
    mov rdi, r12
    call sybilant_mutable_rrbt_persistent
    cmp qword [rax], SYBILANT_LIST_TYPE
    jne .list_failed
    mov rdi, rax
    xor esi, esi
    call sybilant_rrbt_get
    cmp rax, 42
    jne .list_failed

    xor edi, edi
    jmp sybilant_exit

.atom_failed: mov edi, 64
    jmp sybilant_exit
.predicate_failed: mov edi, 65
    jmp sybilant_exit
.type_failed: mov edi, 66
    jmp sybilant_exit
.initial_editors_failed: mov edi, 67
    jmp sybilant_exit
.identity_failed: mov edi, 68
    jmp sybilant_exit
.tail_editor_failed: mov edi, 69
    jmp sybilant_exit
.tail_identity_failed: mov edi, 70
    jmp sybilant_exit
.head_editor_failed: mov edi, 71
    jmp sybilant_exit
.head_identity_failed: mov edi, 72
    jmp sybilant_exit
.edge_value_failed: mov edi, 73
    jmp sybilant_exit
.promotion_failed: mov edi, 74
    jmp sybilant_exit
.root_editor_failed: mov edi, 75
    jmp sybilant_exit
.interior_insert_failed: mov edi, 76
    jmp sybilant_exit
.root_identity_failed: mov edi, 77
    jmp sybilant_exit
.set_failed: mov edi, 78
    jmp sybilant_exit
.delete_failed: mov edi, 79
    jmp sybilant_exit
.source_changed: mov edi, 80
    jmp sybilant_exit
.concat_failed: mov edi, 81
    jmp sybilant_exit
.deep_failed: mov edi, 82
    jmp sybilant_exit
.region_edit_failed: mov edi, 83
    jmp sybilant_exit
.persistent_failed: mov edi, 84
    jmp sybilant_exit
.list_failed: mov edi, 85
    jmp sybilant_exit
.delete_collapse_failed: mov edi, 86
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

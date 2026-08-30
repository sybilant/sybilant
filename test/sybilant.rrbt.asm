bits 64
default rel

extern sybilant_exit
extern sybilant_list_empty
extern sybilant_rrbt_concat
extern sybilant_rrbt_delete
extern sybilant_rrbt_get
extern sybilant_rrbt_insert
extern sybilant_rrbt_length
extern sybilant_rrbt_set
extern sybilant_rrbt_slice
extern sybilant_type
extern sybilant_vector_empty

%include "lib/sybilant.constants.asm"

    RRBT_EDITOR_OFFSET equ 8
    RRBT_LENGTH_OFFSET equ 16
    RRBT_HEIGHT_OFFSET equ 24
    RRBT_ROOT_OFFSET equ 32
    RRBT_HEAD_OFFSET equ 40
    RRBT_TAIL_OFFSET equ 48

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
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 32
    jne .root_failed
    mov rax, [r12 + RRBT_TAIL_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 8
    jne .tail_failed
    mov rax, [r12 + RRBT_HEAD_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .head_failed

    mov rdi, r12
    mov esi, 32
    call sybilant_rrbt_get
    cmp rax, 132
    jne .get_failed
    mov rdi, r12
    mov rsi, -1
    call sybilant_rrbt_get
    cmp rax, 139
    jne .negative_failed

    ;; Appending with tail capacity shares the root and replaces only the tail.
    mov r14, [r12 + RRBT_ROOT_OFFSET]
    mov r15, [r12 + RRBT_TAIL_OFFSET]
    mov rdi, r12
    mov esi, 40
    mov edx, 140
    call sybilant_rrbt_insert
    cmp r14, [rax + RRBT_ROOT_OFFSET]
    jne .append_root_shared_failed
    cmp r15, [rax + RRBT_TAIL_OFFSET]
    je .append_tail_replaced_failed

    ;; Prepending with head capacity shares both the root and tail.
    mov r14, [r12 + RRBT_ROOT_OFFSET]
    mov r15, [r12 + RRBT_TAIL_OFFSET]
    mov rdi, r12
    xor esi, esi
    mov edx, 99
    call sybilant_rrbt_insert
    mov r13, rax
    cmp r14, [rax + RRBT_ROOT_OFFSET]
    jne .prepend_root_shared_failed
    cmp r15, [rax + RRBT_TAIL_OFFSET]
    jne .prepend_tail_shared_failed
    mov rax, [rax + RRBT_HEAD_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 1
    jne .prepend_head_failed
    mov rdi, r13
    xor esi, esi
    call sybilant_rrbt_get
    cmp rax, 99
    jne .prepend_value_failed
    mov rdi, r13
    mov esi, 1
    call sybilant_rrbt_get
    cmp rax, 100
    jne .prepend_value_failed

    ;; An arbitrary edit remains persistent across the edge/root boundary.
    mov rdi, r12
    mov esi, 35
    mov edx, 777
    call sybilant_rrbt_insert
    mov r14, rax
    mov rdi, r14
    mov esi, 35
    call sybilant_rrbt_get
    cmp rax, 777
    jne .insert_failed
    mov rdi, r12
    mov esi, 35
    call sybilant_rrbt_get
    cmp rax, 135
    jne .immutable_failed

    mov rdi, r12
    mov esi, 32
    mov edx, 999
    call sybilant_rrbt_set
    mov r14, rax
    mov rdi, r14
    mov esi, 32
    call sybilant_rrbt_get
    cmp rax, 999
    jne .set_failed

    mov rdi, r12
    mov esi, 10
    call sybilant_rrbt_delete
    mov r14, rax
    mov rdi, r14
    mov esi, 10
    call sybilant_rrbt_get
    cmp rax, 111
    jne .delete_failed

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

    ;; The 33rd prepend promotes a full head into the left side of the root.
    lea rdi, [sybilant_vector_empty]
    xor esi, esi
    mov edx, 1000
    call sybilant_rrbt_insert
    mov r12, rax
    xor r13d, r13d
.prepend_many:
    cmp r13, 33
    je .prepended_many
    mov rdi, r12
    xor esi, esi
    mov rdx, r13
    call sybilant_rrbt_insert
    mov r12, rax
    inc r13
    jmp .prepend_many
.prepended_many:
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 34
    jne .head_promotion_failed
    mov rax, [r12 + RRBT_HEAD_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 1
    jne .head_promotion_failed
    mov rdi, r12
    xor esi, esi
    call sybilant_rrbt_get
    cmp rax, 32
    jne .head_promotion_failed
    mov rdi, r12
    mov esi, 33
    call sybilant_rrbt_get
    cmp rax, 1000
    jne .head_promotion_failed

    ;; Repeated promotion builds a deeper root while preserving indexing.
    lea r12, [sybilant_vector_empty]
    xor r13d, r13d
.build_deep:
    cmp r13, 1025
    je .deep_built
    mov rdi, r12
    mov rsi, r13
    mov rdx, r13
    call sybilant_rrbt_insert
    mov r12, rax
    inc r13
    jmp .build_deep
.deep_built:
    cmp qword [r12 + RRBT_LENGTH_OFFSET], 1025
    jne .deep_failed
    mov rdi, r12
    mov esi, 1024
    call sybilant_rrbt_get
    cmp rax, 1024
    jne .deep_failed

    ;; Lists use the same representation and preserve their runtime type.
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

.type_failed: mov edi, 64
    jmp sybilant_exit
.length_failed: mov edi, 65
    jmp sybilant_exit
.root_failed: mov edi, 66
    jmp sybilant_exit
.tail_failed: mov edi, 67
    jmp sybilant_exit
.head_failed: mov edi, 83
    jmp sybilant_exit
.get_failed: mov edi, 68
    jmp sybilant_exit
.negative_failed: mov edi, 69
    jmp sybilant_exit
.append_root_shared_failed: mov edi, 70
    jmp sybilant_exit
.append_tail_replaced_failed: mov edi, 71
    jmp sybilant_exit
.prepend_root_shared_failed: mov edi, 84
    jmp sybilant_exit
.prepend_tail_shared_failed: mov edi, 85
    jmp sybilant_exit
.prepend_head_failed: mov edi, 86
    jmp sybilant_exit
.prepend_value_failed: mov edi, 87
    jmp sybilant_exit
.insert_failed: mov edi, 72
    jmp sybilant_exit
.immutable_failed: mov edi, 73
    jmp sybilant_exit
.set_failed: mov edi, 74
    jmp sybilant_exit
.delete_failed: mov edi, 75
    jmp sybilant_exit
.slice_failed: mov edi, 76
    jmp sybilant_exit
.concat_failed: mov edi, 77
    jmp sybilant_exit
.deep_failed: mov edi, 78
    jmp sybilant_exit
.head_promotion_failed: mov edi, 88
    jmp sybilant_exit
.list_type_failed: mov edi, 79
    jmp sybilant_exit
.editor_failed: mov edi, 80
    jmp sybilant_exit
.list_value_failed: mov edi, 81
    jmp sybilant_exit
.list_empty_failed: mov edi, 82
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

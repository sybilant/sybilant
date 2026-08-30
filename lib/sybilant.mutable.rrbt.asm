bits 64
default rel

extern sybilant_alloc
extern sybilant_atom_compare_and_set
extern sybilant_atom_deref
extern sybilant_atom_new
extern sybilant_exit
extern sybilant_rrbt_get
extern sybilant_thread_current
extern sybilant_type

%include "lib/sybilant.constants.asm"

    ;; Mutable and persistent RRBT layout.
    RRBT_EDITOR_OFFSET equ 8
    RRBT_LENGTH_OFFSET equ 16
    RRBT_HEIGHT_OFFSET equ 24
    RRBT_ROOT_OFFSET   equ 32
    RRBT_HEAD_OFFSET   equ 40
    RRBT_TAIL_OFFSET   equ 48
    RRBT_SIZE          equ 56
    RRBT_BRANCH_FACTOR equ 32

section .text
;; Create a mutable RRBT with editable root and edge nodes. Descendants remain
;; shared until an operation copies their path with the same edit atom.
;; rdi = persistent list or vector; rax = mutable tree.
global sybilant_mutable_rrbt_new
sybilant_mutable_rrbt_new:
    push rbx
    push r12
    push r13
    mov r12, rdi
    cmp qword [r12], SYBILANT_LIST_TYPE
    je .type_valid
    cmp qword [r12], SYBILANT_VECTOR_TYPE
    jne sybilant_mutable_rrbt_invalid_argument
.type_valid:
    call sybilant_thread_current
    mov rdi, rax
    call sybilant_atom_new
    mov rbx, rax
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov r13, SYBILANT_MUTABLE_VECTOR_TYPE
    cmp qword [r12], SYBILANT_LIST_TYPE
    jne .initialize
    mov r13, SYBILANT_MUTABLE_LIST_TYPE
.initialize:
    mov [rax], r13
    mov [rax + RRBT_EDITOR_OFFSET], rbx
    mov rcx, [r12 + RRBT_LENGTH_OFFSET]
    mov [rax + RRBT_LENGTH_OFFSET], rcx
    mov rcx, [r12 + RRBT_HEIGHT_OFFSET]
    mov [rax + RRBT_HEIGHT_OFFSET], rcx
    mov rcx, [r12 + RRBT_ROOT_OFFSET]
    mov [rax + RRBT_ROOT_OFFSET], rcx
    mov rcx, [r12 + RRBT_HEAD_OFFSET]
    mov [rax + RRBT_HEAD_OFFSET], rcx
    mov rcx, [r12 + RRBT_TAIL_OFFSET]
    mov [rax + RRBT_TAIL_OFFSET], rcx
    mov r13, rax

    mov edx, RRBT_BRANCH_FACTOR
    cmp qword [r13 + RRBT_HEIGHT_OFFSET], 0
    je .edit_root
    mov edx, RRBT_BRANCH_FACTOR * 2
.edit_root:
    mov rdi, [r13 + RRBT_ROOT_OFFSET]
    mov rsi, rbx
    call sybilant_mutable_rrbt_ensure_editable
    mov [r13 + RRBT_ROOT_OFFSET], rax
    mov rdi, [r13 + RRBT_HEAD_OFFSET]
    mov rsi, rbx
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov [r13 + RRBT_HEAD_OFFSET], rax
    mov rdi, [r13 + RRBT_TAIL_OFFSET]
    mov rsi, rbx
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov [r13 + RRBT_TAIL_OFFSET], rax
    mov rax, r13
    pop r13
    pop r12
    pop rbx
    ret

;; Return whether rdi is a mutable list or vector RRBT.
global sybilant_mutable_rrbt_p
sybilant_mutable_rrbt_p:
    sub rsp, 8
    call sybilant_type
    add rsp, 8
    cmp rax, SYBILANT_MUTABLE_LIST_TYPE
    je .true
    cmp rax, SYBILANT_MUTABLE_VECTOR_TYPE
    je .true
    mov eax, SYBILANT_FALSE
    ret
.true:
    mov eax, SYBILANT_TRUE
    ret

;; Return the number of elements after verifying ownership.
global sybilant_mutable_rrbt_length
sybilant_mutable_rrbt_length:
    push rdi
    call sybilant_mutable_rrbt_check_editor
    pop rdi
    mov rax, [rdi + RRBT_LENGTH_OFFSET]
    ret

;; Read an element after verifying ownership. Signed indices count from the back.
global sybilant_mutable_rrbt_get
sybilant_mutable_rrbt_get:
    push rsi
    call sybilant_mutable_rrbt_check_editor
    pop rsi
    jmp sybilant_rrbt_get

;; Insert before an index, mutating editable paths. The position length appends.
;; rdi = mutable tree, rsi = index, rdx = value; rax = mutable tree.
global sybilant_mutable_rrbt_insert
sybilant_mutable_rrbt_insert:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    call sybilant_mutable_rrbt_check_editor
    mov r15, [r12 + RRBT_EDITOR_OFFSET]
    mov rax, [r12 + RRBT_LENGTH_OFFSET]
    cmp rax, -1
    je sybilant_mutable_rrbt_invalid_argument
    cmp r13, rax
    je .append
    test r13, r13
    jz .prepend
    mov rdi, r12
    mov rsi, r13
    call sybilant_mutable_rrbt_normalize_index
    mov r13, rax

    ;; Interior insertion folds both edge buffers into the editable root first.
    mov rdi, r12
    call sybilant_mutable_rrbt_promote_head
    mov rdi, r12
    call sybilant_mutable_rrbt_promote_tail
    mov rdi, r12
    mov rsi, r13
    mov rdx, r14
    call sybilant_mutable_rrbt_insert_root
    inc qword [r12 + RRBT_LENGTH_OFFSET]
    mov rax, r12
    jmp .done

.prepend:
    mov rbx, [r12 + RRBT_HEAD_OFFSET]
    cmp qword [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR
    jb .edit_head
    mov rdi, r12
    call sybilant_mutable_rrbt_promote_head
    mov rbx, [r12 + RRBT_HEAD_OFFSET]
.edit_head:
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rbx, rax
    mov [r12 + RRBT_HEAD_OFFSET], rbx
    mov rdi, rbx
    xor esi, esi
    mov rdx, r14
    call sybilant_mutable_rrbt_array_insert
    inc qword [r12 + RRBT_LENGTH_OFFSET]
    mov rax, r12
    jmp .done

.append:
    mov rbx, [r12 + RRBT_TAIL_OFFSET]
    cmp qword [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR
    jb .edit_tail
    mov rdi, r12
    call sybilant_mutable_rrbt_promote_tail
    mov rbx, [r12 + RRBT_TAIL_OFFSET]
.edit_tail:
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rbx, rax
    mov [r12 + RRBT_TAIL_OFFSET], rbx
    mov rdi, rbx
    mov rsi, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rdx, r14
    call sybilant_mutable_rrbt_array_insert
    inc qword [r12 + RRBT_LENGTH_OFFSET]
    mov rax, r12
.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Delete one element in place. rdi = mutable tree, rsi = index.
global sybilant_mutable_rrbt_delete
sybilant_mutable_rrbt_delete:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    call sybilant_mutable_rrbt_check_editor
    mov r15, [r12 + RRBT_EDITOR_OFFSET]
    mov rdi, r12
    mov rsi, r13
    call sybilant_mutable_rrbt_normalize_index
    mov r13, rax
    mov rbx, [r12 + RRBT_HEAD_OFFSET]
    mov r14, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r13, r14
    jb .head
    mov rbx, [r12 + RRBT_TAIL_OFFSET]
    mov rax, [r12 + RRBT_LENGTH_OFFSET]
    sub rax, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r13, rax
    jae .tail

    ;; The root index excludes the insertion head.
    sub r13, r14
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r15
    call sybilant_mutable_rrbt_delete_node
    test rax, rax
    jnz .root_nonempty
    mov edi, RRBT_BRANCH_FACTOR
    mov rsi, r15
    call sybilant_mutable_rrbt_node_allocate
    mov qword [r12 + RRBT_HEIGHT_OFFSET], 0
.root_nonempty:
    mov [r12 + RRBT_ROOT_OFFSET], rax
.collapse_root:
    cmp qword [r12 + RRBT_HEIGHT_OFFSET], 0
    je .deleted
    mov rax, [r12 + RRBT_ROOT_OFFSET]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    jne .deleted
    mov rax, [rax + SYBILANT_ARRAY_VALUES_OFFSET]
    mov [r12 + RRBT_ROOT_OFFSET], rax
    dec qword [r12 + RRBT_HEIGHT_OFFSET]
    jmp .collapse_root

.head:
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rbx, rax
    mov [r12 + RRBT_HEAD_OFFSET], rbx
    mov rdi, rbx
    mov rsi, r13
    call sybilant_mutable_rrbt_array_delete
    jmp .deleted

.tail:
    sub r13, rax
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rbx, rax
    mov [r12 + RRBT_TAIL_OFFSET], rbx
    mov rdi, rbx
    mov rsi, r13
    call sybilant_mutable_rrbt_array_delete

.deleted:
    dec qword [r12 + RRBT_LENGTH_OFFSET]
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Replace one element in place. rdi = mutable tree, rsi = index, rdx = value.
global sybilant_mutable_rrbt_set
sybilant_mutable_rrbt_set:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    call sybilant_mutable_rrbt_check_editor
    mov r15, [r12 + RRBT_EDITOR_OFFSET]
    mov rdi, r12
    mov rsi, r13
    call sybilant_mutable_rrbt_normalize_index
    mov r13, rax
    mov rbx, [r12 + RRBT_HEAD_OFFSET]
    mov rax, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r13, rax
    jb .head
    sub r13, rax
    mov rbx, [r12 + RRBT_TAIL_OFFSET]
    mov rax, [r12 + RRBT_LENGTH_OFFSET]
    mov rcx, [r12 + RRBT_HEAD_OFFSET]
    sub rax, [rcx + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rax, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r13, rax
    jae .tail

    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r14
    mov r8, r15
    call sybilant_mutable_rrbt_assoc_node
    mov [r12 + RRBT_ROOT_OFFSET], rax
    jmp .done

.head:
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov [r12 + RRBT_HEAD_OFFSET], rax
    mov [rax + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8], r14
    jmp .done

.tail:
    sub r13, rax
    mov rdi, rbx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov [r12 + RRBT_TAIL_OFFSET], rax
    mov [rax + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8], r14
.done:
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Append a persistent RRBT to a mutable RRBT.
;; rdi = mutable left tree, rsi = persistent right tree; rax = mutable left tree.
global sybilant_mutable_rrbt_concat
sybilant_mutable_rrbt_concat:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    call sybilant_mutable_rrbt_check_editor
    cmp qword [r13], SYBILANT_LIST_TYPE
    je .type_valid
    cmp qword [r13], SYBILANT_VECTOR_TYPE
    jne sybilant_mutable_rrbt_invalid_argument
.type_valid:
    mov r14, [r13 + RRBT_LENGTH_OFFSET]
    mov rax, [r12 + RRBT_LENGTH_OFFSET]
    add rax, r14
    jc sybilant_mutable_rrbt_invalid_argument
    xor r15d, r15d
.loop:
    cmp r15, r14
    je .done
    mov rdi, r13
    mov rsi, r15
    call sybilant_rrbt_get
    mov rdx, rax
    mov rdi, r12
    mov rsi, [r12 + RRBT_LENGTH_OFFSET]
    call sybilant_mutable_rrbt_insert
    inc r15
    jmp .loop
.done:
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Persist in O(1) by clearing the shared edit atom.
global sybilant_mutable_rrbt_persistent
sybilant_mutable_rrbt_persistent:
    push rbx
    push r12
    push r13
    mov r12, rdi
    call sybilant_mutable_rrbt_check_editor
    mov r13, [r12 + RRBT_EDITOR_OFFSET]
    call sybilant_thread_current
    mov rdi, r13
    mov rsi, rax
    mov edx, SYBILANT_NIL
    call sybilant_atom_compare_and_set
    cmp rax, SYBILANT_TRUE
    jne sybilant_mutable_rrbt_invalid_argument
    mov rax, SYBILANT_VECTOR_TYPE
    cmp qword [r12], SYBILANT_MUTABLE_LIST_TYPE
    jne .set_type
    mov rax, SYBILANT_LIST_TYPE
.set_type:
    mov [r12], rax
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    ret

;; Verify that this mutable RRBT's shared atom contains the current thread.
sybilant_mutable_rrbt_check_editor:
    cmp qword [rdi], SYBILANT_MUTABLE_LIST_TYPE
    je .type_valid
    cmp qword [rdi], SYBILANT_MUTABLE_VECTOR_TYPE
    jne sybilant_mutable_rrbt_invalid_argument
.type_valid:
    mov rdx, [rdi + RRBT_EDITOR_OFFSET]
    test rdx, rdx
    jz sybilant_mutable_rrbt_invalid_argument
    push rdi
    push rdx
    sub rsp, 8
    call sybilant_thread_current
    mov [rsp], rax
    mov rdi, [rsp + 8]
    call sybilant_atom_deref
    cmp rax, [rsp]
    jne sybilant_mutable_rrbt_invalid_argument
    add rsp, 8
    pop rdx
    pop rdi
    ret

;; Promote the head into the root in reverse insertion order, preserving order.
sybilant_mutable_rrbt_promote_head:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, [r12 + RRBT_HEAD_OFFSET]
    mov r14, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    test r14, r14
    jz .unchanged
.loop:
    dec r14
    mov rdx, [r13 + SYBILANT_ARRAY_VALUES_OFFSET + r14 * 8]
    mov rdi, r12
    xor esi, esi
    call sybilant_mutable_rrbt_insert_root
    test r14, r14
    jnz .loop
    mov edi, RRBT_BRANCH_FACTOR
    mov rsi, [r12 + RRBT_EDITOR_OFFSET]
    call sybilant_mutable_rrbt_node_allocate
    mov [r12 + RRBT_HEAD_OFFSET], rax
.unchanged:
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Promote the tail into the root in forward order, preserving order.
sybilant_mutable_rrbt_promote_tail:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, [r12 + RRBT_TAIL_OFFSET]
    mov r14, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    test r14, r14
    jz .unchanged
    xor r15d, r15d
.loop:
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    call sybilant_mutable_rrbt_node_size
    mov rsi, rax
    mov rdx, [r13 + SYBILANT_ARRAY_VALUES_OFFSET + r15 * 8]
    mov rdi, r12
    call sybilant_mutable_rrbt_insert_root
    inc r15
    cmp r15, r14
    jne .loop
    mov edi, RRBT_BRANCH_FACTOR
    mov rsi, [r12 + RRBT_EDITOR_OFFSET]
    call sybilant_mutable_rrbt_node_allocate
    mov [r12 + RRBT_TAIL_OFFSET], rax
.unchanged:
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Insert into the root without changing the tree's total length.
;; rdi = tree, rsi = root position, rdx = value; rax = tree.
sybilant_mutable_rrbt_insert_root:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, [r12 + RRBT_EDITOR_OFFSET]
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r14
    mov r8, r15
    call sybilant_mutable_rrbt_insert_node
    mov rbx, rax
    test rdx, rdx
    jz .same_height
    mov r13, rdx
    mov edi, RRBT_BRANCH_FACTOR * 2
    mov rsi, r15
    call sybilant_mutable_rrbt_node_allocate
    mov r14, rax
    mov rdi, rbx
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    call sybilant_mutable_rrbt_node_size
    mov rdi, r14
    mov rsi, rbx
    mov rdx, rax
    call sybilant_mutable_rrbt_branch_append
    mov r14, rax
    mov rdi, r13
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    call sybilant_mutable_rrbt_node_size
    mov rdi, r14
    mov rsi, r13
    mov rdx, rax
    call sybilant_mutable_rrbt_branch_append
    mov rbx, rax
    inc qword [r12 + RRBT_HEIGHT_OFFSET]
.same_height:
    mov [r12 + RRBT_ROOT_OFFSET], rbx
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Copy an edited path as needed and replace one leaf value.
;; rdi = node, rsi = height, rdx = index, rcx = value, r8 = edit atom.
sybilant_mutable_rrbt_assoc_node:
    test rsi, rsi
    jnz .branch
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rdx
    mov r14, rcx
    mov r15, r8
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov [rax + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8], r14
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.branch:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    mov [rbp - 48], r8
    xor ebx, ebx
    mov qword [rbp - 56], 0
.find:
    mov rax, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, rax
    jb .found
    mov [rbp - 56], rax
    add rbx, 2
    cmp rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jb .find
    jmp sybilant_mutable_rrbt_invalid_argument
.found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, [rbp - 56]
    mov rcx, r15
    mov r8, [rbp - 48]
    call sybilant_mutable_rrbt_assoc_node
    mov [rbp - 64], rax
    mov rdi, r12
    mov rsi, [rbp - 48]
    mov edx, RRBT_BRANCH_FACTOR * 2
    call sybilant_mutable_rrbt_ensure_editable
    mov rdx, [rbp - 64]
    mov [rax + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8], rdx
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Insert into an editable path, splitting a full node when necessary.
;; rdi = node, rsi = height, rdx = position, rcx = value, r8 = edit atom.
;; rax = left/result node; rdx = optional right split node.
sybilant_mutable_rrbt_insert_node:
    test rsi, rsi
    jnz .branch
    cmp qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR
    je .split_leaf

    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rdx
    mov r14, rcx
    mov r15, r8
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rdi, rax
    mov rsi, r13
    mov rdx, r14
    call sybilant_mutable_rrbt_array_insert
    xor edx, edx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.split_leaf:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8
    mov r12, rdi
    mov r13, rdx
    mov r14, rcx
    mov r15, r8
    mov edi, RRBT_BRANCH_FACTOR
    mov rsi, r15
    call sybilant_mutable_rrbt_node_allocate
    mov rbx, rax
    mov edi, RRBT_BRANCH_FACTOR
    mov rsi, r15
    call sybilant_mutable_rrbt_node_allocate
    mov [rbp - 48], rax
    xor r8d, r8d
    xor r9d, r9d
.copy_leaf:
    cmp r8, RRBT_BRANCH_FACTOR + 1
    je .leaf_done
    cmp r8, r13
    je .insert_value
    mov rax, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + r9 * 8]
    inc r9
    jmp .store_value
.insert_value:
    mov rax, r14
.store_value:
    cmp r8, RRBT_BRANCH_FACTOR
    je .store_right
    mov [rbx + SYBILANT_ARRAY_VALUES_OFFSET + r8 * 8], rax
    jmp .next_value
.store_right:
    mov rdx, [rbp - 48]
    mov [rdx + SYBILANT_ARRAY_VALUES_OFFSET], rax
.next_value:
    inc r8
    jmp .copy_leaf
.leaf_done:
    mov qword [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR
    mov rdx, [rbp - 48]
    mov qword [rdx + SYBILANT_ARRAY_LENGTH_OFFSET], 1
    mov rax, rbx
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

.branch:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 72
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    mov [rbp - 48], r8
    xor ebx, ebx
    mov qword [rbp - 56], 0
.find_child:
    mov rax, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, rax
    jb .child_found
    mov rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rcx, 2
    cmp rbx, rcx
    je .child_found
    mov [rbp - 56], rax
    add rbx, 2
    jmp .find_child
.child_found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, [rbp - 56]
    mov rcx, r15
    mov r8, [rbp - 48]
    call sybilant_mutable_rrbt_insert_node
    mov [rbp - 64], rax
    mov [rbp - 72], rdx
    test rdx, rdx
    jnz .rebuild_split

    mov rdi, r12
    mov rsi, [rbp - 48]
    mov edx, RRBT_BRANCH_FACTOR * 2
    call sybilant_mutable_rrbt_ensure_editable
    mov r12, rax
    mov rax, [rbp - 64]
    mov [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8], rax
    lea rcx, [rbx + 1]
.increment_sizes:
    cmp rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae .branch_done
    inc qword [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8]
    add rcx, 2
    jmp .increment_sizes
.branch_done:
    mov rax, r12
    xor edx, edx
    jmp .branch_return

.rebuild_split:
    mov edi, RRBT_BRANCH_FACTOR * 2
    mov rsi, [rbp - 48]
    call sybilant_mutable_rrbt_node_allocate
    mov [rbp - 80], rax
    mov qword [rbp - 88], 0
    cmp qword [r12 + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR * 2
    jne .branches_ready
    mov edi, RRBT_BRANCH_FACTOR * 2
    mov rsi, [rbp - 48]
    call sybilant_mutable_rrbt_node_allocate
    mov [rbp - 88], rax
.branches_ready:
    mov qword [rbp - 96], 0
    mov qword [rbp - 104], 0
.rebuild_loop:
    mov rax, [rbp - 96]
    cmp rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .rebuilt
    cmp rax, rbx
    jne .append_old
    mov rsi, [rbp - 64]
    call .append_rebuilt
    mov rsi, [rbp - 72]
    call .append_rebuilt
    jmp .advance_source
.append_old:
    mov rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    call .append_rebuilt
.advance_source:
    add qword [rbp - 96], 2
    jmp .rebuild_loop

.append_rebuilt:
    push rsi
    mov rdi, rsi
    lea rsi, [r13 - 1]
    call sybilant_mutable_rrbt_node_size
    mov rdx, rax
    pop rsi
    cmp qword [rbp - 104], RRBT_BRANCH_FACTOR
    jae .append_right
    push rsi
    mov rdi, [rbp - 80]
    call sybilant_mutable_rrbt_branch_append
    pop rcx
    mov [rbp - 80], rax
    inc qword [rbp - 104]
    ret
.append_right:
    push rsi
    mov rdi, [rbp - 88]
    call sybilant_mutable_rrbt_branch_append
    pop rcx
    mov [rbp - 88], rax
    inc qword [rbp - 104]
    ret

.rebuilt:
    mov rax, [rbp - 80]
    mov rdx, [rbp - 88]
.branch_return:
    add rsp, 72
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Delete from an editable path, removing empty nodes.
;; rdi = node, rsi = height, rdx = index, rcx = edit atom.
;; rax = edited node or zero.
sybilant_mutable_rrbt_delete_node:
    test rsi, rsi
    jnz .branch
    cmp qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], 1
    je .empty
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rdx
    mov r15, rcx
    mov rsi, r15
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_mutable_rrbt_ensure_editable
    mov rdi, rax
    mov rsi, r13
    call sybilant_mutable_rrbt_array_delete
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
.empty:
    xor eax, eax
    ret

.branch:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov [rbp - 48], rcx
    xor ebx, ebx
    mov qword [rbp - 56], 0
.find_child:
    mov rax, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, rax
    jb .child_found
    mov [rbp - 56], rax
    add rbx, 2
    cmp rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jb .find_child
    jmp sybilant_mutable_rrbt_invalid_argument
.child_found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, [rbp - 56]
    mov rcx, [rbp - 48]
    call sybilant_mutable_rrbt_delete_node
    mov [rbp - 64], rax
    mov rdi, r12
    mov rsi, [rbp - 48]
    mov edx, RRBT_BRANCH_FACTOR * 2
    call sybilant_mutable_rrbt_ensure_editable
    mov r12, rax
    mov rax, [rbp - 64]
    test rax, rax
    jz .remove_child
    mov [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8], rax
    lea rcx, [rbx + 1]
    jmp .decrement_sizes

.remove_child:
    mov rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rcx, rbx
    sub rcx, 2
    lea rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [rdi + 16]
    rep movsq
    sub qword [r12 + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    lea rcx, [rbx + 1]
.decrement_sizes:
    cmp rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae .delete_done
    dec qword [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8]
    add rcx, 2
    jmp .decrement_sizes
.delete_done:
    cmp qword [r12 + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    je .branch_empty
    mov rax, r12
    jmp .delete_return
.branch_empty:
    xor eax, eax
.delete_return:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Return node itself when owned, otherwise copy it with the requested capacity.
;; rdi = node, rsi = edit atom, rdx = capacity; rax = editable node.
sybilant_mutable_rrbt_ensure_editable:
    cmp [rdi + SYBILANT_ARRAY_EDITOR_OFFSET], rsi
    jne .copy
    mov rax, rdi
    ret
.copy:
    push rbx
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    mov rbx, rdx
    mov rdi, rbx
    mov rsi, r13
    call sybilant_mutable_rrbt_node_allocate
    mov rbx, rax
    mov rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], rcx
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET]
    rep movsq
    mov rax, rbx
    pop r13
    pop r12
    pop rbx
    ret

;; Allocate an empty internal array node. rdi = capacity, rsi = edit atom.
sybilant_mutable_rrbt_node_allocate:
    mov rax, rdi
    mov r8, 8
    mul r8
    test rdx, rdx
    jnz sybilant_mutable_rrbt_invalid_argument
    add rax, SYBILANT_ARRAY_VALUES_OFFSET
    jc sybilant_mutable_rrbt_invalid_argument
    push rbx
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    mov rdi, rax
    call sybilant_alloc
    mov qword [rax], SYBILANT_ARRAY_TYPE
    mov [rax + SYBILANT_ARRAY_EDITOR_OFFSET], r13
    mov qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    mov [rax + SYBILANT_ARRAY_CAPACITY_OFFSET], r12
    pop r13
    pop r12
    pop rbx
    ret

;; Insert into an editable internal array with sufficient capacity.
sybilant_mutable_rrbt_array_insert:
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r8, [rdi + SYBILANT_ARRAY_CAPACITY_OFFSET]
    jae sybilant_mutable_rrbt_invalid_argument
    cmp rsi, r8
    ja sybilant_mutable_rrbt_invalid_argument
    mov rcx, r8
    sub rcx, rsi
    lea r8, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rsi * 8]
    lea r9, [r8 + rcx * 8]
.shift:
    test rcx, rcx
    jz .store
    mov rax, [r9 - 8]
    mov [r9], rax
    sub r9, 8
    dec rcx
    jmp .shift
.store:
    mov [r8], rdx
    inc qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rax, rdi
    ret

;; Delete from an editable internal array.
sybilant_mutable_rrbt_array_delete:
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp rsi, r8
    jae sybilant_mutable_rrbt_bounds
    mov rcx, r8
    sub rcx, rsi
    dec rcx
    lea r8, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rsi * 8]
.shift:
    test rcx, rcx
    jz .done
    mov rax, [r8 + 8]
    mov [r8], rax
    add r8, 8
    dec rcx
    jmp .shift
.done:
    dec qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rax, rdi
    ret

;; Append a child and its cumulative size to an editable branch.
sybilant_mutable_rrbt_branch_append:
    mov rcx, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    lea r8, [rcx + 2]
    cmp r8, [rdi + SYBILANT_ARRAY_CAPACITY_OFFSET]
    ja sybilant_mutable_rrbt_invalid_argument
    test rcx, rcx
    jz .no_previous
    add rdx, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8 - 8]
.no_previous:
    mov [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8], rsi
    mov [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8 + 8], rdx
    mov [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], r8
    mov rax, rdi
    ret

;; Count values below a node. rdi = node, rsi = height.
sybilant_mutable_rrbt_node_size:
    test rsi, rsi
    jnz .branch
    mov rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    ret
.branch:
    mov rcx, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    test rcx, rcx
    jz .empty
    mov rax, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8 - 8]
    ret
.empty:
    xor eax, eax
    ret

sybilant_mutable_rrbt_normalize_index:
    mov rax, rsi
    test rax, rax
    jns .check
    add rax, [rdi + RRBT_LENGTH_OFFSET]
    js sybilant_mutable_rrbt_bounds
.check:
    cmp rax, [rdi + RRBT_LENGTH_OFFSET]
    jae sybilant_mutable_rrbt_bounds
    ret

sybilant_mutable_rrbt_invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit
sybilant_mutable_rrbt_bounds:
    mov edi, SYBILANT_EXIT_BOUNDS
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

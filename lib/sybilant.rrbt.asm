bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_array_empty
extern sybilant_array_concat
extern sybilant_array_insert
extern sybilant_array_delete
extern sybilant_array_slice
extern sybilant_array_get
extern sybilant_array_set

%include "lib/sybilant.constants.asm"

    ;; Relaxed radix-balanced tree layout.
    RRBT_EDITOR_OFFSET equ 8
    RRBT_LENGTH_OFFSET equ 16
    RRBT_HEIGHT_OFFSET equ 24
    RRBT_ROOT_OFFSET   equ 32
    RRBT_TAIL_OFFSET   equ 40
    RRBT_SIZE          equ 48
    RRBT_BRANCH_FACTOR equ 32

section .rodata
align 16
global sybilant_list_empty
sybilant_list_empty:
    dq SYBILANT_LIST_TYPE, 0, 0, 0, sybilant_array_empty, sybilant_array_empty

global sybilant_vector_empty
sybilant_vector_empty:
    dq SYBILANT_VECTOR_TYPE, 0, 0, 0, sybilant_array_empty, sybilant_array_empty

section .text
;; Return the number of elements. rdi = tree; rax = length.
global sybilant_rrbt_length
sybilant_rrbt_length:
    mov rax, [rdi + RRBT_LENGTH_OFFSET]
    ret

;; Read an element. Signed indices count from the back when negative.
;; rdi = tree, rsi = index; rax = element.
global sybilant_rrbt_get
sybilant_rrbt_get:
    call sybilant_rrbt_normalize_index
    mov r8, [rdi + RRBT_TAIL_OFFSET]
    mov r9, [r8 + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov r10, [rdi + RRBT_LENGTH_OFFSET]
    sub r10, r9
    cmp rax, r10
    jb .root
    sub rax, r10
    mov rax, [r8 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    ret
.root:
    mov rsi, rax
    mov rcx, [rdi + RRBT_HEIGHT_OFFSET]
    mov rdi, [rdi + RRBT_ROOT_OFFSET]
.descend:
    test rcx, rcx
    jz .leaf
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    test r8, 1
    jnz sybilant_rrbt_invalid_argument
    xor r9d, r9d
    xor r10d, r10d
.scan:
    cmp r9, r8
    jae sybilant_rrbt_invalid_argument
    mov r11, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + r9 * 8 + 8]
    cmp rsi, r11
    jb .child
    mov r10, r11
    add r9, 2
    jmp .scan
.child:
    sub rsi, r10
    mov rdi, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + r9 * 8]
    dec rcx
    jmp .descend
.leaf:
    mov rax, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rsi * 8]
    ret

;; Insert before an index. The positive position length appends.
;; rdi = tree, rsi = index, rdx = element; rax = new tree.
global sybilant_rrbt_insert
sybilant_rrbt_insert:
    mov r8, [rdi + RRBT_LENGTH_OFFSET]
    cmp r8, -1
    je sybilant_rrbt_invalid_argument
    cmp rsi, r8
    je sybilant_rrbt_append
    push rsi
    push rdx
    sub rsp, 8
    call sybilant_rrbt_materialize_tail
    add rsp, 8
    pop rdx
    pop rsi
    mov rdi, rax
    mov r8, [rdi + RRBT_LENGTH_OFFSET]
    call sybilant_rrbt_normalize_index
    mov rsi, rax
.position_valid:
    push rbx
    push r12
    push r13
    push r14
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    test r8, r8
    jnz .nonempty
    lea rdi, [sybilant_array_empty]
    xor esi, esi
    mov rdx, r14
    call sybilant_array_insert
    mov rbx, rax
    xor r14d, r14d
    jmp .make_tree
.nonempty:
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r14
    call sybilant_rrbt_insert_node
    mov rbx, rax
    test rdx, rdx
    jz .same_height
    mov r13, rdx
    lea rdi, [sybilant_array_empty]
    mov rsi, rbx
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    call sybilant_rrbt_branch_append
    mov rbx, rax
    mov rdi, r13
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    call sybilant_rrbt_node_size
    mov rdx, rax
    mov rdi, rbx
    mov rsi, r13
    call sybilant_rrbt_branch_append
    mov rbx, rax
    mov r14, [r12 + RRBT_HEIGHT_OFFSET]
    inc r14
    jmp .make_tree
.same_height:
    mov r14, [r12 + RRBT_HEIGHT_OFFSET]
.make_tree:
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    inc rdx
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov [rax + RRBT_HEIGHT_OFFSET], r14
    mov [rax + RRBT_ROOT_OFFSET], rbx
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Delete one element. rdi = tree, rsi = index; rax = new tree.
global sybilant_rrbt_delete
sybilant_rrbt_delete:
    push rsi
    call sybilant_rrbt_materialize_tail
    pop rsi
    mov rdi, rax
    call sybilant_rrbt_normalize_index
    cmp qword [rdi + RRBT_LENGTH_OFFSET], 1
    jne .nonempty_result
    call sybilant_rrbt_empty_like
    ret
.nonempty_result:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rax
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    call sybilant_rrbt_delete_node
    mov rbx, rax
    mov r14, [r12 + RRBT_HEIGHT_OFFSET]
.collapse_root:
    test r14, r14
    jz .make_tree
    cmp qword [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    jne .make_tree
    mov rbx, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    dec r14
    jmp .collapse_root
.make_tree:
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    dec rdx
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov [rax + RRBT_HEIGHT_OFFSET], r14
    mov [rax + RRBT_ROOT_OFFSET], rbx
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Copy the half-open range [start, end), using signed indices.
;; rdi = tree, rsi = start, rdx = end; rax = new tree.
global sybilant_rrbt_slice
sybilant_rrbt_slice:
    push rsi
    push rdx
    sub rsp, 8
    call sybilant_rrbt_materialize_tail
    add rsp, 8
    pop rdx
    pop rsi
    mov rdi, rax
    mov r8, rdx
    call sybilant_rrbt_normalize_index
    mov r9, rax
    mov rsi, r8
    call sybilant_rrbt_normalize_index
    cmp r9, rax
    ja sybilant_rrbt_bounds
    jne .nonempty
    call sybilant_rrbt_empty_like
    ret
.nonempty:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, r9
    mov r14, rax
    mov [rsp], r13
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r14
    call sybilant_rrbt_slice_node
    mov rbx, rax
    mov r13, [r12 + RRBT_HEIGHT_OFFSET]
.collapse_root:
    test r13, r13
    jz .make_tree
    cmp qword [rbx + SYBILANT_ARRAY_LENGTH_OFFSET], 2
    jne .make_tree
    mov rbx, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    dec r13
    jmp .collapse_root
.make_tree:
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, r14
    sub rdx, [rsp]
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov [rax + RRBT_HEIGHT_OFFSET], r13
    mov [rax + RRBT_ROOT_OFFSET], rbx
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Replace one element. rdi = tree, rsi = index, rdx = value.
global sybilant_rrbt_set
sybilant_rrbt_set:
    push rsi
    push rdx
    sub rsp, 8
    call sybilant_rrbt_materialize_tail
    add rsp, 8
    pop rdx
    pop rsi
    mov rdi, rax
    call sybilant_rrbt_normalize_index
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rax
    mov r14, rdx
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r13
    mov rcx, r14
    call sybilant_rrbt_assoc_node
    mov rbx, rax
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov rdx, [r12 + RRBT_HEIGHT_OFFSET]
    mov [rax + RRBT_HEIGHT_OFFSET], rdx
    mov [rax + RRBT_ROOT_OFFSET], rbx
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Concatenate two trees. rdi = left, rsi = right; rax = new tree.
global sybilant_rrbt_concat
sybilant_rrbt_concat:
    push rsi
    call sybilant_rrbt_materialize_tail
    pop rsi
    push rax
    mov rdi, rsi
    call sybilant_rrbt_materialize_tail
    mov rdi, rax
    pop r8
    cmp qword [r8 + RRBT_LENGTH_OFFSET], 0
    jne .left_nonempty
    mov rax, rdi
    ret
.left_nonempty:
    cmp qword [rdi + RRBT_LENGTH_OFFSET], 0
    jne .both_nonempty
    mov rax, r8
    ret
.both_nonempty:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16
    mov r12, r8
    mov r13, rdi
    mov r14, [r12 + RRBT_LENGTH_OFFSET]
    add r14, [r13 + RRBT_LENGTH_OFFSET]
    jc sybilant_rrbt_invalid_argument
    mov r15, [r12 + RRBT_HEIGHT_OFFSET]
    mov rax, [r13 + RRBT_HEIGHT_OFFSET]
    cmp r15, rax
    cmovb r15, rax
    mov rdi, [r12 + RRBT_ROOT_OFFSET]
    mov rsi, [r12 + RRBT_HEIGHT_OFFSET]
    mov rdx, r15
    call sybilant_rrbt_lift_node
    mov rbx, rax
    mov rdi, [r13 + RRBT_ROOT_OFFSET]
    mov rsi, [r13 + RRBT_HEIGHT_OFFSET]
    mov rdx, r15
    call sybilant_rrbt_lift_node
    mov rdi, rbx
    mov rsi, rax
    mov rdx, r15
    call sybilant_rrbt_merge_nodes
    mov rbx, rax
    test rdx, rdx
    jz .root_ready
    mov [rsp], rdx
    mov rdi, rbx
    mov rsi, r15
    call sybilant_rrbt_node_size
    mov rdx, rax
    lea rdi, [sybilant_array_empty]
    mov rsi, rbx
    call sybilant_rrbt_branch_append
    mov rbx, rax
    mov rdi, [rsp]
    mov rsi, r15
    call sybilant_rrbt_node_size
    mov rdx, rax
    mov rdi, rbx
    mov rsi, [rsp]
    call sybilant_rrbt_branch_append
    mov rbx, rax
    inc r15
.root_ready:
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov [rax + RRBT_LENGTH_OFFSET], r14
    mov [rax + RRBT_HEIGHT_OFFSET], r15
    mov [rax + RRBT_ROOT_OFFSET], rbx
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Append to the insertion tail, promoting a full tail into the root.
;; rdi = tree, rdx = value; rax = new tree.
sybilant_rrbt_append:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rdx
    mov r14, [r12 + RRBT_TAIL_OFFSET]
    mov rbx, [r14 + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp rbx, RRBT_BRANCH_FACTOR
    jb .have_space
    mov rdi, r12
    call sybilant_rrbt_materialize_tail
    mov r12, rax
    mov r14, [r12 + RRBT_TAIL_OFFSET]
    xor ebx, ebx
.have_space:
    mov rdi, r14
    mov rsi, rbx
    mov rdx, r13
    call sybilant_array_insert
    mov rbx, rax
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    inc rdx
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov rdx, [r12 + RRBT_HEIGHT_OFFSET]
    mov [rax + RRBT_HEIGHT_OFFSET], rdx
    mov rdx, [r12 + RRBT_ROOT_OFFSET]
    mov [rax + RRBT_ROOT_OFFSET], rdx
    mov [rax + RRBT_TAIL_OFFSET], rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Move every tail value into the tree, leaving an empty tail.
sybilant_rrbt_materialize_tail:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, [r12 + RRBT_ROOT_OFFSET]
    mov r14, [r12 + RRBT_HEIGHT_OFFSET]
    mov r15, [r12 + RRBT_LENGTH_OFFSET]
    mov rax, [r12 + RRBT_TAIL_OFFSET]
    mov rdx, [rax + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub r15, rdx
    test rdx, rdx
    jz .unchanged
    test r15, r15
    jnz .start_loop
    mov r13, rax
    mov r15, [r12 + RRBT_LENGTH_OFFSET]
    jmp .make_tree
.start_loop:
    xor ebx, ebx
.loop:
    mov rax, [r12 + RRBT_TAIL_OFFSET]
    cmp rbx, [rax + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .make_tree
    mov rcx, [rax + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    test r15, r15
    jnz .insert
    lea rdi, [sybilant_array_empty]
    xor esi, esi
    mov rdx, rcx
    call sybilant_array_insert
    mov r13, rax
    xor r14d, r14d
    jmp .next
.insert:
    mov rdi, r13
    mov rsi, r14
    mov rdx, r15
    call sybilant_rrbt_insert_node
    mov r13, rax
    test rdx, rdx
    jz .next
    push rdx
    sub rsp, 8
    lea rdi, [sybilant_array_empty]
    mov rsi, r13
    mov rdx, r15
    call sybilant_rrbt_branch_append
    mov r13, rax
    add rsp, 8
    pop rsi
    push rsi
    sub rsp, 8
    mov rdi, rsi
    mov rsi, r14
    call sybilant_rrbt_node_size
    mov rdx, rax
    add rsp, 8
    pop rsi
    mov rdi, r13
    call sybilant_rrbt_branch_append
    mov r13, rax
    inc r14
.next:
    inc r15
    inc rbx
    jmp .loop
.make_tree:
    mov edi, RRBT_SIZE
    call sybilant_alloc
    mov rdx, [r12]
    mov [rax], rdx
    mov qword [rax + RRBT_EDITOR_OFFSET], 0
    mov rdx, [r12 + RRBT_LENGTH_OFFSET]
    mov [rax + RRBT_LENGTH_OFFSET], rdx
    mov [rax + RRBT_HEIGHT_OFFSET], r14
    mov [rax + RRBT_ROOT_OFFSET], r13
    lea rdx, [sybilant_array_empty]
    mov [rax + RRBT_TAIL_OFFSET], rdx
    jmp .done
.unchanged:
    mov rax, r12
.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Internal: path-copy a node for an associative update.
;; rdi = node, rsi = height, rdx = local index, rcx = value; rax = new node.
sybilant_rrbt_assoc_node:
    test rsi, rsi
    jnz .branch
    mov rsi, rdx
    mov rdx, rcx
    jmp sybilant_array_set
.branch:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    xor ebx, ebx
    xor r8d, r8d
.scan:
    mov r9, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, r9
    jb .found
    mov r8, r9
    add rbx, 2
    cmp rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jb .scan
    jmp sybilant_rrbt_invalid_argument
.found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, r8
    mov rcx, r15
    call sybilant_rrbt_assoc_node
    mov rdx, rax
    mov rdi, r12
    mov rsi, rbx
    call sybilant_array_set
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Internal: insert into a node with path copying.
;; rdi = node, rsi = height, rdx = local position, rcx = value.
;; rax = left/result node, rdx = optional right split node (zero if none).
sybilant_rrbt_insert_node:
    test rsi, rsi
    jnz .branch
    push r12
    push r13
    push r14
    mov r12, rdi
    mov r13, rdx
    mov r14, rcx
    mov rsi, r13
    mov rdx, r14
    call sybilant_array_insert
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR
    jbe .leaf_done
    mov r12, rax
    mov rdi, r12
    xor esi, esi
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_array_slice
    mov r13, rax
    mov rdi, r12
    mov esi, RRBT_BRANCH_FACTOR
    call sybilant_array_get
    mov rdx, rax
    lea rdi, [sybilant_array_empty]
    xor esi, esi
    call sybilant_array_insert
    mov rdx, rax
    mov rax, r13
    pop r14
    pop r13
    pop r12
    ret

.leaf_done:
    xor edx, edx
    pop r14
    pop r13
    pop r12
    ret

.branch:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    xor ebx, ebx
    mov qword [rbp - 48], 0
.find:
    mov r8, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, r8
    jb .found
    ;; Insertion at the node's end belongs to its final child.
    mov r9, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub r9, 2
    cmp rbx, r9
    je .found
    mov [rbp - 48], r8
    add rbx, 2
    jmp .find
.found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, [rbp - 48]
    mov rcx, r15
    call sybilant_rrbt_insert_node
    mov [rbp - 56], rax
    mov [rbp - 64], rdx
    lea rax, [sybilant_array_empty]
    mov [rbp - 72], rax
    mov qword [rbp - 80], 0
.rebuild:
    mov rax, [rbp - 80]
    cmp rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .rebuilt
    cmp rax, rbx
    jne .copy_old
    mov rsi, [rbp - 56]
    call .append
    mov rsi, [rbp - 64]
    test rsi, rsi
    jz .advance
    call .append
    jmp .advance
.copy_old:
    mov rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    call .append
.advance:
    add qword [rbp - 80], 2
    jmp .rebuild
.append:
    push rsi
    mov rdi, rsi
    lea rsi, [r13 - 1]
    call sybilant_rrbt_node_size
    mov rdx, rax
    pop rsi
    mov rdi, [rbp - 72]
    call sybilant_rrbt_branch_append
    mov [rbp - 72], rax
    ret
.rebuilt:
    mov rax, [rbp - 72]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], RRBT_BRANCH_FACTOR * 2
    jbe .branch_done
    mov r12, rax
    mov rdi, r12
    xor esi, esi
    mov edx, RRBT_BRANCH_FACTOR * 2
    call sybilant_array_slice
    mov r13, rax
    mov rdi, r12
    mov esi, RRBT_BRANCH_FACTOR * 2
    call sybilant_array_get
    mov r14, rax
    mov rdi, r12
    mov esi, RRBT_BRANCH_FACTOR * 2 + 1
    call sybilant_array_get
    mov rdx, rax
    lea rdi, [sybilant_array_empty]
    mov rsi, r14
    ;; Rebase the right node's only cumulative size.
    mov rax, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + RRBT_BRANCH_FACTOR * 16 - 8]
    sub rdx, rax
    call sybilant_rrbt_branch_append
    mov rdx, rax
    mov rax, r13
    jmp .branch_return
.branch_done:
    xor edx, edx
.branch_return:
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Internal: delete from a node with path copying.
;; rdi = node, rsi = height, rdx = local index; rax = new node or zero.
sybilant_rrbt_delete_node:
    test rsi, rsi
    jnz .branch
    cmp qword [rdi + SYBILANT_ARRAY_LENGTH_OFFSET], 1
    je .empty_leaf
    mov rsi, rdx
    jmp sybilant_array_delete
.empty_leaf:
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
    xor ebx, ebx
    mov qword [rbp - 48], 0
.find:
    mov r8, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    cmp r14, r8
    jb .found
    mov [rbp - 48], r8
    add rbx, 2
    cmp rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jb .find
    jmp sybilant_rrbt_invalid_argument
.found:
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    lea rsi, [r13 - 1]
    mov rdx, r14
    sub rdx, [rbp - 48]
    call sybilant_rrbt_delete_node
    mov r15, rax
    lea rax, [sybilant_array_empty]
    mov [rbp - 56], rax
    mov qword [rbp - 64], 0
.rebuild:
    mov rax, [rbp - 64]
    cmp rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .done
    cmp rax, rbx
    jne .old_child
    mov rsi, r15
    test rsi, rsi
    jz .advance
    jmp .append
.old_child:
    mov rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
.append:
    push rsi
    mov rdi, rsi
    lea rsi, [r13 - 1]
    call sybilant_rrbt_node_size
    mov rdx, rax
    pop rsi
    mov rdi, [rbp - 56]
    call sybilant_rrbt_branch_append
    mov [rbp - 56], rax
.advance:
    add qword [rbp - 64], 2
    jmp .rebuild
.done:
    mov rax, [rbp - 56]
    cmp qword [rax + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .return
    xor eax, eax
.return:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Internal: wrap a node in single-child branches until it reaches target height.
;; rdi = node, rsi = current height, rdx = target height; rax = lifted node.
sybilant_rrbt_lift_node:
    cmp rsi, rdx
    jne .lift
    mov rax, rdi
    ret
.lift:
    push r12
    push r13
    push r14
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
.loop:
    mov rdi, r12
    mov rsi, r13
    call sybilant_rrbt_node_size
    mov rdx, rax
    lea rdi, [sybilant_array_empty]
    mov rsi, r12
    call sybilant_rrbt_branch_append
    mov r12, rax
    inc r13
    cmp r13, r14
    jne .loop
    mov rax, r12
    pop r14
    pop r13
    pop r12
    ret

;; Internal: retain the half-open range [start,end) from a node.
;; rdi = node, rsi = height, rdx = start, rcx = end; rax = sliced node.
sybilant_rrbt_slice_node:
    test rsi, rsi
    jnz .branch
    test rdx, rdx
    jnz .copy_leaf
    cmp rcx, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    jne .copy_leaf
    mov rax, rdi
    ret
.copy_leaf:
    mov rsi, rdx
    mov rdx, rcx
    jmp sybilant_rrbt_array_copy_range

.branch:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    lea rax, [sybilant_array_empty]
    mov [rbp - 48], rax
    mov qword [rbp - 56], 0
    xor ebx, ebx
.child_loop:
    cmp rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .done
    mov r8, [rbp - 56]
    mov r9, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8 + 8]
    mov [rbp - 80], r9
    cmp r15, r8
    jbe .done
    cmp r14, r9
    jae .next
    ;; Local start is max(start - previous cumulative size, 0).
    xor r10d, r10d
    cmp r14, r8
    jbe .have_start
    mov r10, r14
    sub r10, r8
.have_start:
    mov [rbp - 72], r10
    ;; Local end is min(end - previous cumulative size, child size).
    mov r11, r9
    sub r11, r8
    mov rax, r15
    sub rax, r8
    cmp rax, r11
    cmova rax, r11
    mov [rbp - 64], rax
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    test r10, r10
    jnz .slice_child
    cmp rax, r11
    jne .slice_child
    mov rsi, rdi
    jmp .append_child
.slice_child:
    lea rsi, [r13 - 1]
    mov rdx, r10
    mov rcx, rax
    call sybilant_rrbt_slice_node
    mov rsi, rax
.append_child:
    mov rdx, [rbp - 64]
    sub rdx, [rbp - 72]
    mov rdi, [rbp - 48]
    call sybilant_rrbt_branch_append
    mov [rbp - 48], rax
.next:
    mov rax, [rbp - 80]
    mov [rbp - 56], rax
    add rbx, 2
    jmp .child_loop
.done:
    mov rax, [rbp - 48]
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Internal: merge two nodes of equal height, returning one or two nodes.
;; rdi = left, rsi = right, rdx = height; rax = first, rdx = optional second.
sybilant_rrbt_merge_nodes:
    test rdx, rdx
    jnz .branches
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov r9, [rsi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov r10, r8
    add r10, r9
    cmp r10, RRBT_BRANCH_FACTOR
    jbe .merge_leaf
    cmp r8, RRBT_BRANCH_FACTOR
    je .keep_leaves
    push rbx
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    call sybilant_array_concat
    mov rbx, rax
    mov rdi, rbx
    xor esi, esi
    mov edx, RRBT_BRANCH_FACTOR
    call sybilant_array_slice
    mov r12, rax
    mov rdi, rbx
    mov esi, RRBT_BRANCH_FACTOR
    mov rdx, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    call sybilant_rrbt_array_copy_range
    mov rdx, rax
    mov rax, r12
    pop r13
    pop r12
    pop rbx
    ret
.keep_leaves:
    mov rax, rdi
    mov rdx, rsi
    ret
.merge_leaf:
    call sybilant_array_concat
    xor edx, edx
    ret

.branches:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 56
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov rbx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rbx, 2
    mov rdi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rbx * 8]
    mov rsi, [r13 + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rdx, [r14 - 1]
    call sybilant_rrbt_merge_nodes
    mov [rbp - 48], rax
    mov [rbp - 56], rdx
    lea rax, [sybilant_array_empty]
    mov [rbp - 64], rax
    mov [rbp - 72], rax
    mov qword [rbp - 80], 0
    mov qword [rbp - 88], 0
.left_loop:
    mov rax, [rbp - 88]
    cmp rax, rbx
    je .middle
    mov rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    call .append
    add qword [rbp - 88], 2
    jmp .left_loop
.middle:
    mov rsi, [rbp - 48]
    call .append
    mov rsi, [rbp - 56]
    test rsi, rsi
    jz .right_start
    call .append
.right_start:
    mov qword [rbp - 88], 2
.right_loop:
    mov rax, [rbp - 88]
    cmp rax, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    je .merge_done
    mov rsi, [r13 + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    call .append
    add qword [rbp - 88], 2
    jmp .right_loop
.append:
    push rsi
    mov rdi, rsi
    lea rsi, [r14 - 1]
    call sybilant_rrbt_node_size
    mov rdx, rax
    pop rsi
    cmp qword [rbp - 80], RRBT_BRANCH_FACTOR
    jb .append_left
    mov rdi, [rbp - 72]
    call sybilant_rrbt_branch_append
    mov [rbp - 72], rax
    inc qword [rbp - 80]
    ret
.append_left:
    mov rdi, [rbp - 64]
    call sybilant_rrbt_branch_append
    mov [rbp - 64], rax
    inc qword [rbp - 80]
    ret
.merge_done:
    mov rax, [rbp - 64]
    mov rdx, [rbp - 72]
    cmp qword [rdx + SYBILANT_ARRAY_LENGTH_OFFSET], 0
    jne .merge_return
    xor edx, edx
.merge_return:
    add rsp, 56
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; Internal: copy [start,end) from an array. Unlike the public slice contract,
;; this helper permits end == length for boundary-node construction.
sybilant_rrbt_array_copy_range:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    lea r15, [sybilant_array_empty]
.loop:
    cmp r13, r14
    je .done
    mov rdi, r12
    mov rsi, r13
    call sybilant_array_get
    mov rdx, rax
    mov rdi, r15
    mov rsi, [r15 + SYBILANT_ARRAY_LENGTH_OFFSET]
    call sybilant_array_insert
    mov r15, rax
    inc r13
    jmp .loop
.done:
    mov rax, r15
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Internal: append a child and its size to a relaxed branch array.
;; rdi = branch array, rsi = child, rdx = child size; rax = new branch.
sybilant_rrbt_branch_append:
    push rbx
    push r12
    push r13
    mov rbx, rdi
    mov r12, rsi
    mov r13, rdx
    mov rcx, [rbx + SYBILANT_ARRAY_LENGTH_OFFSET]
    test rcx, rcx
    jz .no_previous
    add r13, [rbx + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8 - 8]
.no_previous:
    mov rsi, rcx
    mov rdx, r12
    call sybilant_array_insert
    mov rdi, rax
    mov rsi, [rax + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rdx, r13
    call sybilant_array_insert
    pop r13
    pop r12
    pop rbx
    ret

;; Internal: count values below a node. rdi = node, rsi = height.
sybilant_rrbt_node_size:
    test rsi, rsi
    jnz .branch
    mov rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    ret
.branch:
    mov rcx, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rax, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rcx * 8 - 8]
    ret

;; Return the type-specific empty RRBT for rdi.
sybilant_rrbt_empty_like:
    lea rax, [sybilant_vector_empty]
    cmp qword [rdi], SYBILANT_LIST_TYPE
    jne .done
    lea rax, [sybilant_list_empty]
.done:
    ret

sybilant_rrbt_normalize_index:
    mov rax, rsi
    test rax, rax
    jns .check
    add rax, [rdi + RRBT_LENGTH_OFFSET]
    js sybilant_rrbt_bounds
.check:
    cmp rax, [rdi + RRBT_LENGTH_OFFSET]
    jae sybilant_rrbt_bounds
    ret

sybilant_rrbt_invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit
sybilant_rrbt_bounds:
    mov edi, SYBILANT_EXIT_BOUNDS
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_thread_current
extern sybilant_type_p

%include "lib/sybilant.constants.asm"

section .text
;; Create a mutable array. rdi = capacity; rsi = source array or SYBILANT_NIL.
;; Without a source, length equals capacity and every value is SYBILANT_NIL.
global sybilant_mutable_array_new
sybilant_mutable_array_new:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    cmp r13, SYBILANT_NIL
    je .without_source
    cmp qword [r13], SYBILANT_ARRAY_TYPE
    jne sybilant_mutable_array_invalid_argument
    mov r14, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp r14, r12
    ja sybilant_mutable_array_invalid_argument
    jmp .allocate
.without_source:
    mov r14, r12
.allocate:
    mov rdi, r12
    call sybilant_mutable_array_allocate
    mov rbx, rax
    mov [rbx + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], r14
    lea rdi, [rbx + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET]
    mov rcx, r14
    cmp r13, SYBILANT_NIL
    jne .copy
    mov rax, SYBILANT_NIL
    rep stosq
    jmp .done
.copy:
    lea rsi, [r13 + SYBILANT_ARRAY_VALUES_OFFSET]
    rep movsq
.done:
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Return whether rdi is a mutable array.
global sybilant_mutable_array_p
sybilant_mutable_array_p:
    mov esi, SYBILANT_MUTABLE_ARRAY_TYPE
    jmp sybilant_type_p

;; Append an immutable array. rdi = mutable array; rsi = array; rax = mutable array.
global sybilant_mutable_array_concat
sybilant_mutable_array_concat:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    call sybilant_mutable_array_check_editor
    cmp qword [r13], SYBILANT_ARRAY_TYPE
    jne sybilant_mutable_array_invalid_argument
    mov r14, [r12 + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    add r14, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jc sybilant_mutable_array_invalid_argument
    mov rdi, r12
    mov rsi, r14
    call sybilant_mutable_array_ensure_capacity
    mov rbx, rax
    mov rcx, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov rdx, r14
    sub rdx, rcx
    lea rdi, [rbx + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + rdx * 8]
    lea rsi, [r13 + SYBILANT_ARRAY_VALUES_OFFSET]
    rep movsq
    mov [rbx + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], r14
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Insert before an index. rdi = mutable array; rsi = index; rdx = value.
global sybilant_mutable_array_insert
sybilant_mutable_array_insert:
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    call sybilant_mutable_array_check_editor
    mov r15, [r12 + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    cmp r13, r15
    je .position_valid
    call sybilant_mutable_array_normalize_index
    mov r13, rax
.position_valid:
    cmp r15, -1
    je sybilant_mutable_array_invalid_argument
    mov rdi, r12
    lea rsi, [r15 + 1]
    call sybilant_mutable_array_ensure_capacity
    mov rbx, rax
    lea rdi, [rbx + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + r13 * 8 + 8]
    lea rsi, [rdi - 8]
    mov rcx, r15
    sub rcx, r13
    std
    lea rdi, [rdi + rcx * 8 - 8]
    lea rsi, [rsi + rcx * 8 - 8]
    rep movsq
    cld
    mov [rbx + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + r13 * 8], r14
    inc qword [rbx + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    mov rax, rbx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Delete one item. rdi = mutable array; rsi = index; rax = mutable array.
global sybilant_mutable_array_delete
sybilant_mutable_array_delete:
    push r12
    mov r12, rdi
    call sybilant_mutable_array_check_editor
    call sybilant_mutable_array_normalize_index
    mov rcx, [r12 + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    sub rcx, rax
    dec rcx
    lea rdi, [r12 + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + rax * 8]
    lea rsi, [rdi + 8]
    rep movsq
    dec qword [r12 + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    mov rax, r12
    pop r12
    ret

;; Read one value. rdi = mutable array; rsi = index; rax = value.
global sybilant_mutable_array_get
sybilant_mutable_array_get:
    call sybilant_mutable_array_check_editor
    call sybilant_mutable_array_normalize_index
    mov rax, [rdi + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + rax * 8]
    ret

;; Replace one value in place. rdi = mutable array; rsi = index; rdx = value.
global sybilant_mutable_array_set
sybilant_mutable_array_set:
    push rdx
    call sybilant_mutable_array_check_editor
    call sybilant_mutable_array_normalize_index
    pop rdx
    mov [rdi + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET + rax * 8], rdx
    mov rax, rdi
    ret

;; Return the length after verifying ownership.
global sybilant_mutable_array_length
sybilant_mutable_array_length:
    call sybilant_mutable_array_check_editor
    mov rax, [rdi + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    ret

;; Return the capacity after verifying ownership.
global sybilant_mutable_array_capacity
sybilant_mutable_array_capacity:
    call sybilant_mutable_array_check_editor
    mov rax, [rdi + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET]
    ret

;; Ensure at least rsi slots of capacity. rax may be a replacement allocation.
global sybilant_mutable_array_reserve
sybilant_mutable_array_reserve:
    call sybilant_mutable_array_check_editor
    jmp sybilant_mutable_array_ensure_capacity

;; Persist in O(1), retaining capacity and invalidating mutable access.
global sybilant_mutable_array_persistent
sybilant_mutable_array_persistent:
    call sybilant_mutable_array_check_editor
    mov qword [rdi + SYBILANT_MUTABLE_ARRAY_EDITOR_OFFSET], 0
    mov qword [rdi], SYBILANT_ARRAY_TYPE
    mov rax, rdi
    ret

sybilant_mutable_array_check_editor:
    push rdi
    call sybilant_thread_current
    pop rdi
    cmp [rdi + SYBILANT_MUTABLE_ARRAY_EDITOR_OFFSET], rax
    jne sybilant_mutable_array_invalid_argument
    ret

sybilant_mutable_array_normalize_index:
    mov rax, rsi
    test rax, rax
    jns .check
    add rax, [rdi + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    js sybilant_mutable_array_bounds
.check:
    cmp rax, [rdi + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    jae sybilant_mutable_array_bounds
    ret

;; Grow geometrically until capacity is at least rsi.
sybilant_mutable_array_ensure_capacity:
    cmp rsi, [rdi + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET]
    jbe .unchanged
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, [r12 + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET]
    test r14, r14
    jnz .double
    mov r14, 1
.double:
    cmp r14, r13
    jae .allocate
    add r14, r14
    jc sybilant_mutable_array_invalid_argument
    jmp .double
.allocate:
    mov rdi, r14
    call sybilant_mutable_array_allocate
    mov rbx, rax
    mov rcx, [r12 + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET]
    mov [rbx + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], rcx
    lea rdi, [rbx + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET]
    rep movsq
    mov qword [r12 + SYBILANT_MUTABLE_ARRAY_EDITOR_OFFSET], 0
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
.unchanged:
    mov rax, rdi
    ret

;; Allocate capacity rdi and install the current thread as editor.
sybilant_mutable_array_allocate:
    mov rax, rdi
    mov r8, 8
    mul r8
    test rdx, rdx
    jnz sybilant_mutable_array_invalid_argument
    add rax, SYBILANT_MUTABLE_ARRAY_VALUES_OFFSET
    jc sybilant_mutable_array_invalid_argument
    push r12
    mov r12, rdi
    mov rdi, rax
    call sybilant_alloc
    mov qword [rax], SYBILANT_MUTABLE_ARRAY_TYPE
    mov qword [rax + SYBILANT_MUTABLE_ARRAY_LENGTH_OFFSET], 0
    mov [rax + SYBILANT_MUTABLE_ARRAY_CAPACITY_OFFSET], r12
    push rax
    sub rsp, 8
    call sybilant_thread_current
    add rsp, 8
    mov rdx, rax
    pop rax
    mov [rax + SYBILANT_MUTABLE_ARRAY_EDITOR_OFFSET], rdx
    pop r12
    ret

sybilant_mutable_array_invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit
sybilant_mutable_array_bounds:
    mov edi, SYBILANT_EXIT_BOUNDS
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

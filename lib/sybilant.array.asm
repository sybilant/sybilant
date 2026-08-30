bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_type_p

%include "lib/sybilant.constants.asm"

section .rodata
align 16
global sybilant_array_empty
sybilant_array_empty:
    dq SYBILANT_ARRAY_TYPE, 0, 0, 0

section .text
;; Return whether rdi is an array. rax = SYBILANT_TRUE or SYBILANT_FALSE.
global sybilant_array_p
sybilant_array_p:
    mov esi, SYBILANT_ARRAY_TYPE
    jmp sybilant_type_p

;; Concatenate two immutable arrays.
;; rdi = left array, rsi = right array; rax = new array
global sybilant_array_concat
sybilant_array_concat:
    mov r8, rdi
    mov rdi, rsi
    mov r9, [r8 + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov r10, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    mov r11, r9
    add r11, r10
    jc sybilant_array_invalid_argument
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, r8
    mov r13, rdi
    mov r14, r9
    mov r15, r10
    mov rdi, r11
    call sybilant_array_allocate
    mov rbx, rax
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, r14
    rep movsq
    lea rsi, [r13 + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, r15
    rep movsq
    mov rax, rbx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Insert before an index. Positions 0 through length are valid.
;; rdi = array, rsi = index, rdx = item; rax = new array.
global sybilant_array_insert
sybilant_array_insert:
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp rsi, r8
    je .position_valid
    call sybilant_array_normalize_index
    mov rsi, rax
.position_valid:
    cmp r8, -1
    je sybilant_array_invalid_argument
    push rbx
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, r8
    lea rdi, [r15 + 1]
    call sybilant_array_allocate
    mov rbx, rax
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, r13
    rep movsq
    mov [rbx + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8], r14
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8 + 8]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8]
    mov rcx, r15
    sub rcx, r13
    rep movsq
    mov rax, rbx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Delete one item at an index.
;; rdi = array, rsi = index; rax = new array.
global sybilant_array_delete
sybilant_array_delete:
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    call sybilant_array_normalize_index
    mov rsi, rax
    cmp r8, 1
    je .empty
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, r8
    lea rdi, [r14 - 1]
    call sybilant_array_allocate
    mov rbx, rax
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET]
    mov rcx, r13
    rep movsq
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8 + 8]
    mov rcx, r14
    sub rcx, r13
    dec rcx
    rep movsq
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
.empty:
    lea rax, [sybilant_array_empty]
    ret

;; Copy the half-open range [start, end).
;; rdi = array, rsi = start, rdx = end; rax = new array.
;; Requires 0 <= start <= end and end <= length - 1.
global sybilant_array_slice
sybilant_array_slice:
    mov r8, rdx
    call sybilant_array_normalize_index
    mov r9, rax
    mov rsi, r8
    call sybilant_array_normalize_index
    mov rdx, rax
    mov rsi, r9
    cmp rsi, rdx
    ja sybilant_array_bounds
    je .empty
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    sub r14, r13
    mov rdi, r14
    call sybilant_array_allocate
    mov rbx, rax
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8]
    mov rcx, r14
    rep movsq
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
.empty:
    lea rax, [sybilant_array_empty]
    ret

;; Read one qword. rdi = array, rsi = zero-based index; rax = item.
global sybilant_array_get
sybilant_array_get:
    call sybilant_array_normalize_index
    mov rax, [rdi + SYBILANT_ARRAY_VALUES_OFFSET + rax * 8]
    ret

;; Replace one qword. rdi = array, rsi = index, rdx = item; rax = new array.
global sybilant_array_set
sybilant_array_set:
    mov r8, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    call sybilant_array_normalize_index
    mov rsi, rax
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov rdi, r8
    call sybilant_array_allocate
    mov rbx, rax
    mov rcx, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    lea rdi, [rbx + SYBILANT_ARRAY_VALUES_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_VALUES_OFFSET]
    rep movsq
    mov [rbx + SYBILANT_ARRAY_VALUES_OFFSET + r13 * 8], r14
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Return the number of items. rdi = array; rax = length.
global sybilant_array_length
sybilant_array_length:
    mov rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    ret

;; Return the allocated item capacity. rdi = array; rax = capacity.
global sybilant_array_capacity
sybilant_array_capacity:
    mov rax, [rdi + SYBILANT_ARRAY_CAPACITY_OFFSET]
    ret

;; Internal: normalize a signed element index in rsi. rax = front index.
sybilant_array_normalize_index:
    mov rax, rsi
    test rax, rax
    jns .check
    add rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    js sybilant_array_bounds
.check:
    cmp rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_array_bounds
    ret

;; Internal: allocate and initialize an array containing rdi uninitialized items.
sybilant_array_allocate:
    test rdi, rdi
    jz .empty
    mov rax, rdi
    mov r8, 8
    mul r8
    test rdx, rdx
    jnz sybilant_array_invalid_argument
    add rax, SYBILANT_ARRAY_VALUES_OFFSET
    jc sybilant_array_invalid_argument
    push r12
    mov r12, rdi
    mov rdi, rax
    call sybilant_alloc
    mov qword [rax], SYBILANT_ARRAY_TYPE
    mov qword [rax + SYBILANT_ARRAY_EDITOR_OFFSET], 0
    mov [rax + SYBILANT_ARRAY_LENGTH_OFFSET], r12
    mov [rax + SYBILANT_ARRAY_CAPACITY_OFFSET], r12
    pop r12
    ret
.empty:
    lea rax, [sybilant_array_empty]
    ret

sybilant_array_invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit
sybilant_array_bounds:
    mov edi, SYBILANT_EXIT_BOUNDS
    jmp sybilant_exit

;; Local Variables:
;; mode: nasm
;; End:

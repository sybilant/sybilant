bits 64
default rel

extern sybilant_alloc
extern sybilant_exit

SYBILANT_EXIT_INVALID_ARGUMENT equ 2
SYBILANT_EXIT_BOUNDS equ 3

ARRAY_TYPE equ 0
ARRAY_TYPE_OFFSET equ 0
ARRAY_COUNT_OFFSET equ 8
ARRAY_ITEMS_OFFSET equ 16

section .rodata
align 16
global sybilant_array_empty
sybilant_array_empty: dq ARRAY_TYPE, 0

section .text
global sybilant_array_concat
global sybilant_array_insert
global sybilant_array_delete
global sybilant_array_slice
global sybilant_array_get
global sybilant_array_set
global sybilant_array_length

; Concatenate two immutable arrays.
; rdi = left array, rsi = right array; rax = new array
sybilant_array_concat:
    call sybilant_array_validate
    mov r8, rdi
    mov rdi, rsi
    call sybilant_array_validate
    mov r9, [r8 + ARRAY_COUNT_OFFSET]
    mov r10, [rdi + ARRAY_COUNT_OFFSET]
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
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET]
    mov rcx, r14
    rep movsq
    lea rsi, [r13 + ARRAY_ITEMS_OFFSET]
    mov rcx, r15
    rep movsq
    mov rax, rbx
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; Insert before an index. Positions 0 through length are valid.
; rdi = array, rsi = index, rdx = item; rax = new array.
sybilant_array_insert:
    call sybilant_array_validate
    mov r8, [rdi + ARRAY_COUNT_OFFSET]
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
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET]
    mov rcx, r13
    rep movsq
    mov [rbx + ARRAY_ITEMS_OFFSET + r13 * 8], r14
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET + r13 * 8 + 8]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET + r13 * 8]
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

; Delete one item at an index.
; rdi = array, rsi = index; rax = new array.
sybilant_array_delete:
    call sybilant_array_validate
    mov r8, [rdi + ARRAY_COUNT_OFFSET]
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
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET]
    mov rcx, r13
    rep movsq
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET + r13 * 8]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET + r13 * 8 + 8]
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

; Copy the half-open range [start, end).
; rdi = array, rsi = start, rdx = end; rax = new array.
; Requires 0 <= start <= end and end <= length - 1.
sybilant_array_slice:
    call sybilant_array_validate
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
    lea rdi, [rbx + ARRAY_ITEMS_OFFSET]
    lea rsi, [r12 + ARRAY_ITEMS_OFFSET + r13 * 8]
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

; Read one qword. rdi = array, rsi = zero-based index; rax = item.
sybilant_array_get:
    call sybilant_array_validate
    call sybilant_array_normalize_index
    mov rax, [rdi + ARRAY_ITEMS_OFFSET + rax * 8]
    ret

; Replace one qword. rdi = array, rsi = index, rdx = item; rax = new array.
sybilant_array_set:
    call sybilant_array_validate
    mov r8, [rdi + ARRAY_COUNT_OFFSET]
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
    mov rcx, [r12 + ARRAY_COUNT_OFFSET]
    add rcx, 2
    mov rdi, rbx
    mov rsi, r12
    rep movsq
    mov [rbx + ARRAY_ITEMS_OFFSET + r13 * 8], r14
    mov rax, rbx
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; Return the number of items. rdi = array; rax = length.
sybilant_array_length:
    call sybilant_array_validate
    mov rax, [rdi + ARRAY_COUNT_OFFSET]
    ret

; Internal: validate that rdi points to an array. Preserves argument registers.
sybilant_array_validate:
    test rdi, rdi
    jz sybilant_array_invalid_argument
    cmp qword [rdi + ARRAY_TYPE_OFFSET], ARRAY_TYPE
    jne sybilant_array_invalid_argument
    ret

; Internal: normalize a signed element index in rsi. rax = front index.
sybilant_array_normalize_index:
    mov rax, rsi
    test rax, rax
    jns .check
    add rax, [rdi + ARRAY_COUNT_OFFSET]
    js sybilant_array_bounds
.check:
    cmp rax, [rdi + ARRAY_COUNT_OFFSET]
    jae sybilant_array_bounds
    ret

; Internal: allocate and initialize an array containing rdi uninitialized items.
sybilant_array_allocate:
    test rdi, rdi
    jz .empty
    mov rax, rdi
    mov r8, 8
    mul r8
    test rdx, rdx
    jnz sybilant_array_invalid_argument
    add rax, ARRAY_ITEMS_OFFSET
    jc sybilant_array_invalid_argument
    push r12
    mov r12, rdi
    mov rdi, rax
    call sybilant_alloc
    mov qword [rax + ARRAY_TYPE_OFFSET], ARRAY_TYPE
    mov [rax + ARRAY_COUNT_OFFSET], r12
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

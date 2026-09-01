bits 64
default rel

%include "lib/constants.asm"

section .text
global _start
global sybilant_Dboolean_q
global sybilant_Dboolean_q_Dunchecked
global sybilant_Dexit
global sybilant_Dexit_Dunchecked
global sybilant_Dinstance_q
global sybilant_Dinstance_q_Dunchecked
global sybilant_Dmalloc
global sybilant_Dmalloc_Dunchecked
global sybilant_Dtype
global sybilant_Dtype_Dunchecked
extern sybilant_Dmain

_start:
    mov rax, SYBILANT_MALLOC_START
    mov [rel sybilant_Dmalloc_Dstart], rax
    mov [rel sybilant_Dmalloc_Dmaximum], rax

    call sybilant_Dmain

    mov edi, eax
    call sybilant_Dexit
    ud2

;; Check the status and exit the process.
;; Arguments: rdi = status (uint8). Return type: never.
sybilant_Dexit:
    cmp rdi, 0xff
    jbe sybilant_Dexit_Dunchecked

    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT

;; Exit the process with a proven status.
;; Arguments: rdi = status (uint8). Return type: never.
sybilant_Dexit_Dunchecked:
    mov eax, SYS_EXIT
    syscall
    ud2

;; Check a value and return its runtime type, or nil for nil.
;; Arguments: rdi = value (value). Return type: type or nil.
sybilant_Dtype:
    cmp rdi, SYBILANT_NIL
    je .valid_argument

    cmp rdi, SYBILANT_FALSE
    je .valid_argument

    cmp rdi, SYBILANT_TRUE
    je .valid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz .invalid_state

.valid_argument:
    sub rsp, 8
    call sybilant_Dtype_Dunchecked
    add rsp, 8

    cmp rax, SYBILANT_NIL
    je .valid_return

    mov rdx, rax
    and edx, SYBILANT_EXTENDED_TAG_MASK
    cmp edx, SYBILANT_EXTENDED_TAG_TYPE
    jne .invalid_state

.valid_return:
    ret

.invalid_state:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Dexit_Dunchecked

;; Return the runtime type of a proven value, or nil for nil.
;; Arguments: rdi = value (value). Return type: type or nil.
sybilant_Dtype_Dunchecked:
    cmp rdi, SYBILANT_NIL
    je .nil

    cmp rdi, SYBILANT_FALSE
    je .boolean

    cmp rdi, SYBILANT_TRUE
    je .boolean

    mov rax, [rdi]
    ret

.nil:
    mov eax, SYBILANT_NIL
    ret

.boolean:
    mov eax, SYBILANT_BOOLEAN_TYPE
    ret

;; Check the arguments and return whether a value is an instance of a type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: boolean.
sybilant_Dinstance_q:
    mov rax, rsi
    and eax, SYBILANT_EXTENDED_TAG_MASK
    cmp eax, SYBILANT_EXTENDED_TAG_TYPE
    jne .invalid_argument

    push rsi
    call sybilant_Dtype
    pop rsi
    jmp sybilant_Dinstance_q_Dunchecked.compare

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Dexit_Dunchecked

;; Return whether a proven value is an instance of a proven type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: boolean.
sybilant_Dinstance_q_Dunchecked:
    push rsi
    call sybilant_Dtype_Dunchecked
    pop rsi

.compare:
    cmp rax, rsi
    je .true

    mov eax, SYBILANT_FALSE
    ret

.true:
    mov eax, SYBILANT_TRUE
    ret

;; Check a value and return whether it is a boolean.
;; Arguments: rdi = value (value). Return type: boolean.
sybilant_Dboolean_q:
    mov esi, SYBILANT_BOOLEAN_TYPE
    jmp sybilant_Dinstance_q

;; Return whether a proven value is a boolean.
;; Arguments: rdi = value (value). Return type: boolean.
sybilant_Dboolean_q_Dunchecked:
    mov esi, SYBILANT_BOOLEAN_TYPE
    jmp sybilant_Dinstance_q_Dunchecked

;; Allocate a contiguous region with a dynamically supplied byte count.
;; Arguments: rdi = byte count (uint64). Return type: Pointer.
sybilant_Dmalloc:

;; Allocate a contiguous region with a proven byte count.
;; Arguments: rdi = byte count (uint64). Return type: Pointer.
sybilant_Dmalloc_Dunchecked:
    test rdi, rdi
    jz .invalid_argument

    push r12
    push r13

    mov r12, [rel sybilant_Dmalloc_Dstart]
    mov r13, r12
    add r13, rdi
    jc .out_of_memory

    cmp r13, [rel sybilant_Dmalloc_Dmaximum]
    jbe .allocated

    mov rsi, r13
    add rsi, PAGE_SIZE - 1
    jc .out_of_memory
    and rsi, -PAGE_SIZE

    mov rdi, [rel sybilant_Dmalloc_Dmaximum]
    sub rsi, rdi
    mov edx, PROT_READ | PROT_WRITE
    mov r10d, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE
    mov r8, -1
    xor r9d, r9d
    mov eax, SYS_MMAP
    syscall

    cmp rax, rdi
    jne .out_of_memory

    add rdi, rsi
    mov [rel sybilant_Dmalloc_Dmaximum], rdi

.allocated:
    mov [rel sybilant_Dmalloc_Dstart], r13
    mov rax, r12

    pop r13
    pop r12
    ret

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Dexit_Dunchecked

.out_of_memory:
    mov edi, SYBILANT_ERROR_OUT_OF_MEMORY
    jmp sybilant_Dexit_Dunchecked

section .bss
align 8
sybilant_Dmalloc_Dstart:
    resq 1
sybilant_Dmalloc_Dmaximum:
    resq 1

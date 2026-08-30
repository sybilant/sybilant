bits 64
default rel

%include "lib/sybilant.constants.asm"

    ALLOC_ALIGNMENT equ 16

section .rodata
align 16

global sybilant_dynamic_type
sybilant_dynamic_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_DYNAMIC_TYPE

global sybilant_maybe_type
sybilant_maybe_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MAYBE_TYPE

global sybilant_boolean_type
sybilant_boolean_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_BOOLEAN_TYPE

global sybilant_codepoint_type
sybilant_codepoint_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_CODEPOINT_TYPE

global sybilant_type_type
sybilant_type_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_TYPE_TYPE

global sybilant_uint8_type
sybilant_uint8_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_UINT8_TYPE

global sybilant_uint16_type
sybilant_uint16_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_UINT16_TYPE

global sybilant_uint32_type
sybilant_uint32_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_UINT32_TYPE

global sybilant_uint64_type
sybilant_uint64_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_UINT64_TYPE

global sybilant_int8_type
sybilant_int8_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_INT8_TYPE

global sybilant_int16_type
sybilant_int16_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_INT16_TYPE

global sybilant_int32_type
sybilant_int32_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_INT32_TYPE

global sybilant_int64_type
sybilant_int64_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_INT64_TYPE

global sybilant_decimal32_type
sybilant_decimal32_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_DECIMAL32_TYPE

global sybilant_decimal64_type
sybilant_decimal64_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_DECIMAL64_TYPE

global sybilant_decimal128_type
sybilant_decimal128_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_DECIMAL128_TYPE

global sybilant_float16_type
sybilant_float16_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_FLOAT16_TYPE

global sybilant_float32_type
sybilant_float32_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_FLOAT32_TYPE

global sybilant_float64_type
sybilant_float64_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_FLOAT64_TYPE

global sybilant_float128_type
sybilant_float128_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_FLOAT128_TYPE

global sybilant_float256_type
sybilant_float256_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_FLOAT256_TYPE

global sybilant_array_type
sybilant_array_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_ARRAY_TYPE

global sybilant_mutable_array_type
sybilant_mutable_array_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_ARRAY_TYPE

global sybilant_string_type
sybilant_string_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_STRING_TYPE

global sybilant_mutable_string_type
sybilant_mutable_string_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_STRING_TYPE

global sybilant_symbol_type
sybilant_symbol_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_SYMBOL_TYPE

global sybilant_keyword_type
sybilant_keyword_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_KEYWORD_TYPE

global sybilant_list_type
sybilant_list_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_LIST_TYPE

global sybilant_mutable_list_type
sybilant_mutable_list_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_LIST_TYPE

global sybilant_vector_type
sybilant_vector_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_VECTOR_TYPE

global sybilant_mutable_vector_type
sybilant_mutable_vector_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_VECTOR_TYPE

global sybilant_arraymap_type
sybilant_arraymap_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_ARRAYMAP_TYPE

global sybilant_mutable_arraymap_type
sybilant_mutable_arraymap_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_ARRAYMAP_TYPE

global sybilant_hashmap_type
sybilant_hashmap_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_HASHMAP_TYPE

global sybilant_mutable_hashmap_type
sybilant_mutable_hashmap_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_MUTABLE_HASHMAP_TYPE

global sybilant_nil
sybilant_nil:
    dq SYBILANT_NIL
global sybilant_false
sybilant_false:
    dq SYBILANT_FALSE
global sybilant_true
sybilant_true:
    dq SYBILANT_TRUE

section .bss
align 8
global sybilant_frontier
sybilant_frontier:
    resq 1

section .text
;; Return the runtime type of rdi in rax.
global sybilant_type
sybilant_type:
    mov rax, rdi
    cmp rax, SYBILANT_NIL
    je .nil
    and eax, 111b
    cmp eax, SYBILANT_DYNAMIC
    je .dynamic
    cmp eax, SYBILANT_TRUE
    jne .convert_tag
    mov eax, SYBILANT_FALSE
.convert_tag:
    shl eax, 8
    or eax, SYBILANT_TYPE_TAG
    ret
.dynamic:
    mov rax, [rdi]
    ret
.nil:
    mov eax, SYBILANT_NIL
    ret

;; Return whether rdi has type rsi. rax = SYBILANT_TRUE or SYBILANT_FALSE.
global sybilant_type_p
sybilant_type_p:
    push rsi
    call sybilant_type
    pop rsi
    cmp rax, rsi
    mov eax, SYBILANT_FALSE
    jne .done
    mov eax, SYBILANT_TRUE
.done:
    ret

;; Allocate rdi bytes and return a 16-byte-aligned address in rax.
;; Allocations are never moved or reused. This initial allocator is intentionally
;; process-global and single-threaded.
global sybilant_alloc
sybilant_alloc:
    test rdi, rdi
    jz .invalid_argument

    add rdi, ALLOC_ALIGNMENT - 1
    jc .out_of_memory
    and rdi, -ALLOC_ALIGNMENT
    mov rdx, rdi

    mov r10, qword sybilant_frontier
    mov r8, [r10]
    test r8, r8
    jnz .have_frontier

    xor edi, edi
    mov eax, SYS_BRK
    syscall
    test rax, rax
    jz .out_of_memory
    add rax, ALLOC_ALIGNMENT - 1
    jc .out_of_memory
    and rax, -ALLOC_ALIGNMENT
    mov r8, rax

.have_frontier:
    mov r9, r8
    add r9, rdx
    jc .out_of_memory

    mov rdi, r9
    mov eax, SYS_BRK
    syscall
    cmp rax, r9
    jne .out_of_memory

    mov [r10], r9
    mov rax, r8
    ret

.invalid_argument:
    mov edi, SYBILANT_EXIT_INVALID_ARGUMENT
    jmp sybilant_exit

.out_of_memory:
    mov edi, SYBILANT_EXIT_OUT_OF_MEMORY
    jmp sybilant_exit

;; Terminate the process with exit code rdi.
global sybilant_exit
sybilant_exit:
    mov eax, SYS_EXIT
    syscall
    ud2

;; Local Variables:
;; mode: nasm
;; End:

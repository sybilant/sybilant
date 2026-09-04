bits 64
default rel

%include "lib/constants.asm"

section .text
global sybilant_darray_S_e
global sybilant_darray_Sget
global sybilant_darray_Sget_Dunchecked
global sybilant_darray_Slength
global sybilant_darray_Slength_Dunchecked
extern sybilant_S_e
extern sybilant_Sbox_Dint8
extern sybilant_Sbox_Dint16
extern sybilant_Sbox_Dint32
extern sybilant_Sbox_Dint64
extern sybilant_Sbox_Dnat8
extern sybilant_Sbox_Dnat16
extern sybilant_Sbox_Dnat32
extern sybilant_Sbox_Dnat64
extern sybilant_Sbox_Duint8
extern sybilant_Sbox_Duint16
extern sybilant_Sbox_Duint32
extern sybilant_Sbox_Duint64
extern sybilant_Sexit_Dunchecked

;; Return an array element as a dynamic value.
;; Arguments: rdi = array (value); rsi = index (uint64). Return type: value.
sybilant_darray_Sget:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    call sybilant_darray_Sguard
    mov [rsp + 16], rax

    mov rdi, [rsp]
    mov rsi, [rsp + 8]
    call sybilant_darray_Sget_Dunchecked

    mov rdx, [rsp + 16]
    add rsp, 24

    mov rdi, rax

    cmp rdx, SYBILANT_UINT8_TYPE
    je sybilant_Sbox_Duint8

    cmp rdx, SYBILANT_UINT16_TYPE
    je sybilant_Sbox_Duint16

    cmp rdx, SYBILANT_UINT32_TYPE
    je sybilant_Sbox_Duint32

    cmp rdx, SYBILANT_UINT64_TYPE
    je sybilant_Sbox_Duint64

    cmp rdx, SYBILANT_INT8_TYPE
    je sybilant_Sbox_Dint8

    cmp rdx, SYBILANT_INT16_TYPE
    je sybilant_Sbox_Dint16

    cmp rdx, SYBILANT_INT32_TYPE
    je sybilant_Sbox_Dint32

    cmp rdx, SYBILANT_INT64_TYPE
    je sybilant_Sbox_Dint64

    cmp rdx, SYBILANT_NAT8_TYPE
    je sybilant_Sbox_Dnat8

    cmp rdx, SYBILANT_NAT16_TYPE
    je sybilant_Sbox_Dnat16

    cmp rdx, SYBILANT_NAT32_TYPE
    je sybilant_Sbox_Dnat32

    cmp rdx, SYBILANT_NAT64_TYPE
    je sybilant_Sbox_Dnat64

    mov rax, rdi
    ret

;; Return a proven array's element without boxing it.
;; Arguments: rdi = array (array); rsi = index (uint64).
;; Return type: element type.
sybilant_darray_Sget_Dunchecked:
    cmp rsi, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_darray_Sout_of_bounds

    mov rdx, [rdi + SYBILANT_ARRAY_TYPE_OFFSET]
    mov edx, [rdx + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]

    cmp rdx, 1
    je .byte

    cmp rdx, 2
    je .word

    cmp rdx, 4
    je .doubleword

    cmp rdx, 8
    jne sybilant_darray_Sinvalid_state

    mov rax, [rdi + rsi * 8 + SYBILANT_ARRAY_DATA_OFFSET]
    ret

.byte:
    mov al, [rdi + rsi + SYBILANT_ARRAY_DATA_OFFSET]
    ret

.word:
    mov ax, [rdi + rsi * 2 + SYBILANT_ARRAY_DATA_OFFSET]
    ret

.doubleword:
    mov eax, [rdi + rsi * 4 + SYBILANT_ARRAY_DATA_OFFSET]
    ret

;; Return an array's length.
;; Arguments: rdi = array (value). Return type: uint64.
sybilant_darray_Slength:
    sub rsp, 8
    call sybilant_darray_Sguard
    add rsp, 8
    jmp sybilant_darray_Slength_Dunchecked

;; Return a proven array's length.
;; Arguments: rdi = array (array). Return type: uint64.
sybilant_darray_Slength_Dunchecked:
    mov rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    ret

;; Return whether two distinct array values of the given array type contain
;; equal elements.
;; Arguments: rdi = left (array); rsi = right (array); rdx = array type
;; (type). Return type: boolean.
sybilant_darray_S_e:
    push rbx
    push rbp
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12, rdi
    mov r13, rsi
    mov r14, [rdx + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov eax, [rdx + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov [rsp], rax
    mov rbp, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp rbp, [r13 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jne .different

    xor r15d, r15d

.compare_element:
    cmp r15, rbp
    jae .equal

    mov rdi, r12
    mov rsi, r15
    call sybilant_darray_Sget_Dunchecked
    mov rbx, rax

    mov rdi, r13
    mov rsi, r15
    call sybilant_darray_Sget_Dunchecked

    cmp qword [rsp], 1
    je .compare_byte_element

    cmp qword [rsp], 2
    je .compare_word_element

    cmp qword [rsp], 4
    je .compare_doubleword_element

    cmp qword [rsp], 8
    jne .unsupported_stride

    cmp r14, SYBILANT_UINT8_TYPE
    jb .compare_dynamic_element

    cmp r14, SYBILANT_NAT64_TYPE
    ja .compare_dynamic_element

    cmp rbx, rax
    jne .different
    jmp .next_element

.compare_byte_element:
    cmp bl, al
    jne .different
    jmp .next_element

.compare_word_element:
    cmp bx, ax
    jne .different
    jmp .next_element

.compare_doubleword_element:
    cmp ebx, eax
    jne .different
    jmp .next_element

.compare_dynamic_element:
    mov rdi, rbx
    mov rsi, rax
    call sybilant_S_e
    cmp rax, SYBILANT_TRUE
    jne .different

.next_element:
    inc r15
    jmp .compare_element

.unsupported_stride:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

.different:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    mov eax, SYBILANT_FALSE
    ret

.equal:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbp
    pop rbx
    mov eax, SYBILANT_TRUE
    ret

;; Validate an array and return its element type.
;; Arguments: rdi = array (value). Return type: type.
sybilant_darray_Sguard:
    cmp rdi, SYBILANT_NIL
    je sybilant_darray_Sinvalid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_darray_Sinvalid_argument

    mov rax, [rdi + SYBILANT_ARRAY_TYPE_OFFSET]
    cmp rax, SYBILANT_NIL
    je sybilant_darray_Sinvalid_argument

    test rax, SYBILANT_TAG_MASK
    jnz sybilant_darray_Sinvalid_argument

    cmp qword [rax + SYBILANT_ARRAY_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    jne sybilant_darray_Sinvalid_argument

    cmp qword [rax + SYBILANT_ARRAY_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    jne sybilant_darray_Sinvalid_argument

    cmp dword [rax + SYBILANT_ARRAY_TYPE_LAYOUT_FLAGS_OFFSET], 0
    jne sybilant_darray_Sinvalid_argument

    mov rax, [rax + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    cmp rax, SYBILANT_NIL
    je sybilant_darray_Sinvalid_argument

    test rax, SYBILANT_TAG_MASK
    jz .pointer_element_type

    mov rdx, rax
    and edx, SYBILANT_EXTENDED_TAG_MASK
    cmp edx, SYBILANT_EXTENDED_TAG_TYPE
    jne sybilant_darray_Sinvalid_argument
    ret

.pointer_element_type:
    cmp qword [rax], SYBILANT_TYPE_TYPE
    jne sybilant_darray_Sinvalid_argument
    ret

sybilant_darray_Sinvalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

sybilant_darray_Sinvalid_state:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

sybilant_darray_Sout_of_bounds:
    mov edi, SYBILANT_ERROR_OUT_OF_BOUNDS
    jmp sybilant_Sexit_Dunchecked

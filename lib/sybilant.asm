bits 64
default rel

%include "lib/constants.asm"

section .text
global _start
global sybilant_Dbox_Duint8
global sybilant_Dbox_Duint16
global sybilant_Dbox_Duint32
global sybilant_Dbox_Duint64
global sybilant_Dbox_Dint8
global sybilant_Dbox_Dint16
global sybilant_Dbox_Dint32
global sybilant_Dbox_Dint64
global sybilant_Dbox_Dnat8
global sybilant_Dbox_Dnat16
global sybilant_Dbox_Dnat32
global sybilant_Dbox_Dnat64
global sybilant_Dboolean_q
global sybilant_Dboolean_q_Dunchecked
global sybilant_D_e
global sybilant_D_e_Dunchecked
global sybilant_Dexit
global sybilant_Dexit_Dunchecked
global sybilant_Dinstance_q
global sybilant_Dinstance_q_Dunchecked
global sybilant_Dmalloc
global sybilant_Dmalloc_Dunchecked
global sybilant_Dtype
global sybilant_Dtype_Dunchecked
global sybilant_Dunbox_Duint8
global sybilant_Dunbox_Duint8_Dunchecked
global sybilant_Dunbox_Duint16
global sybilant_Dunbox_Duint16_Dunchecked
global sybilant_Dunbox_Duint32
global sybilant_Dunbox_Duint32_Dunchecked
global sybilant_Dunbox_Duint64
global sybilant_Dunbox_Duint64_Dunchecked
global sybilant_Dunbox_Dint8
global sybilant_Dunbox_Dint8_Dunchecked
global sybilant_Dunbox_Dint16
global sybilant_Dunbox_Dint16_Dunchecked
global sybilant_Dunbox_Dint32
global sybilant_Dunbox_Dint32_Dunchecked
global sybilant_Dunbox_Dint64
global sybilant_Dunbox_Dint64_Dunchecked
global sybilant_Dunbox_Dnat8
global sybilant_Dunbox_Dnat8_Dunchecked
global sybilant_Dunbox_Dnat16
global sybilant_Dunbox_Dnat16_Dunchecked
global sybilant_Dunbox_Dnat32
global sybilant_Dunbox_Dnat32_Dunchecked
global sybilant_Dunbox_Dnat64
global sybilant_Dunbox_Dnat64_Dunchecked
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

    mov eax, edi
    and eax, SYBILANT_EXTENDED_TAG_MASK

    cmp eax, SYBILANT_EXTENDED_TAG_UINT8
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_UINT16
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_UINT32
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_INT8
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_INT16
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_INT32
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_NAT8
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_NAT16
    je .valid_argument

    cmp eax, SYBILANT_EXTENDED_TAG_NAT32
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

    mov eax, edi
    and eax, SYBILANT_EXTENDED_TAG_MASK

    cmp eax, SYBILANT_EXTENDED_TAG_UINT8
    je .uint8

    cmp eax, SYBILANT_EXTENDED_TAG_UINT16
    je .uint16

    cmp eax, SYBILANT_EXTENDED_TAG_UINT32
    je .uint32

    cmp eax, SYBILANT_EXTENDED_TAG_INT8
    je .int8

    cmp eax, SYBILANT_EXTENDED_TAG_INT16
    je .int16

    cmp eax, SYBILANT_EXTENDED_TAG_INT32
    je .int32

    cmp eax, SYBILANT_EXTENDED_TAG_NAT8
    je .nat8

    cmp eax, SYBILANT_EXTENDED_TAG_NAT16
    je .nat16

    cmp eax, SYBILANT_EXTENDED_TAG_NAT32
    je .nat32

    mov rax, [rdi]
    ret

.nil:
    mov eax, SYBILANT_NIL
    ret

.boolean:
    mov eax, SYBILANT_BOOLEAN_TYPE
    ret

.uint8:
    mov eax, SYBILANT_UINT8_TYPE
    ret

.uint16:
    mov eax, SYBILANT_UINT16_TYPE
    ret

.uint32:
    mov eax, SYBILANT_UINT32_TYPE
    ret

.int8:
    mov eax, SYBILANT_INT8_TYPE
    ret

.int16:
    mov eax, SYBILANT_INT16_TYPE
    ret

.int32:
    mov eax, SYBILANT_INT32_TYPE
    ret

.nat8:
    mov eax, SYBILANT_NAT8_TYPE
    ret

.nat16:
    mov eax, SYBILANT_NAT16_TYPE
    ret

.nat32:
    mov eax, SYBILANT_NAT32_TYPE
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
    mov rdi, rax
    jmp sybilant_D_e_Dunchecked

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Dexit_Dunchecked

;; Return whether a proven value is an instance of a proven type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: boolean.
sybilant_Dinstance_q_Dunchecked:
    push rsi
    call sybilant_Dtype_Dunchecked
    pop rsi
    mov rdi, rax
    jmp sybilant_D_e_Dunchecked

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

;; Check two values and return whether they are equal.
;; Arguments: rdi = left (value); rsi = right (value). Return type: boolean.
sybilant_D_e:
    cmp rdi, SYBILANT_NIL
    je .left_immediate

    test rdi, SYBILANT_TAG_MASK
    jnz .left_immediate

    cmp rsi, SYBILANT_NIL
    je .right_immediate

    test rsi, SYBILANT_TAG_MASK
    jnz .right_immediate

    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, rsi
    call sybilant_Dtype
    mov r14, rax

    mov rdi, r13
    call sybilant_Dtype

    mov rdi, r14
    mov rsi, rax
    call sybilant_D_e_Dunchecked

    cmp rax, SYBILANT_TRUE
    jne .different_heap_types

    mov rdx, r14
    mov rdi, r12
    mov rsi, r13

    pop r14
    pop r13
    pop r12
    jmp sybilant_D_e_Dunchecked.dispatch

.different_heap_types:
    pop r14
    pop r13
    pop r12
    jmp sybilant_D_e_Dunchecked.false

.left_immediate:
    cmp rsi, SYBILANT_NIL
    je sybilant_D_e_Dunchecked

    test rsi, SYBILANT_TAG_MASK
    jnz sybilant_D_e_Dunchecked

    sub rsp, 8
    mov rdi, rsi
    call sybilant_Dtype
    add rsp, 8
    jmp sybilant_D_e_Dunchecked.false

.right_immediate:
    sub rsp, 8
    call sybilant_Dtype
    add rsp, 8
    jmp sybilant_D_e_Dunchecked.false

;; Return whether two proven values are equal.
;; Arguments: rdi = left (value); rsi = right (value). Return type: boolean.
sybilant_D_e_Dunchecked:
    cmp rdi, SYBILANT_NIL
    je .compare_immediates

    test rdi, SYBILANT_TAG_MASK
    jnz .compare_immediates

    cmp rsi, SYBILANT_NIL
    je .false

    test rsi, SYBILANT_TAG_MASK
    jnz .false

    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, rsi
    mov r14, [rdi]

    mov rdi, r14
    mov rsi, [rsi]
    call sybilant_D_e_Dunchecked

    cmp rax, SYBILANT_TRUE
    jne .different_heap_types

    mov rdx, r14
    mov rdi, r12
    mov rsi, r13

    pop r14
    pop r13
    pop r12
    jmp .dispatch

.different_heap_types:
    pop r14
    pop r13
    pop r12
    jmp .false

.dispatch:
;; Dispatch equality for type rdx with values rdi and rsi.
    cmp rdx, SYBILANT_UINT64_TYPE
    je .integer64

    cmp rdx, SYBILANT_INT64_TYPE
    je .integer64

    cmp rdx, SYBILANT_NAT64_TYPE
    je .integer64

    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Dexit_Dunchecked

.integer64:
    mov rax, [rdi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    cmp rax, [rsi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    je .true
    jmp .false

.compare_immediates:
    cmp rdi, rsi
    je .true

.false:
    mov eax, SYBILANT_FALSE
    ret

.true:
    mov eax, SYBILANT_TRUE
    ret

;; Box a uint8 value for dynamic storage.
;; Arguments: rdi = value (uint8). Return type: value.
sybilant_Dbox_Duint8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT8
    ret

;; Box a uint16 value for dynamic storage.
;; Arguments: rdi = value (uint16). Return type: value.
sybilant_Dbox_Duint16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT16
    ret

;; Box a uint32 value for dynamic storage.
;; Arguments: rdi = value (uint32). Return type: value.
sybilant_Dbox_Duint32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT32
    ret

;; Box a uint64 value for dynamic storage.
;; Arguments: rdi = value (uint64). Return type: value.
sybilant_Dbox_Duint64:
    mov esi, SYBILANT_UINT64_TYPE
    jmp sybilant_Dbox_Dinteger64

;; Box an int8 value for dynamic storage.
;; Arguments: rdi = value (int8). Return type: value.
sybilant_Dbox_Dint8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT8
    ret

;; Box an int16 value for dynamic storage.
;; Arguments: rdi = value (int16). Return type: value.
sybilant_Dbox_Dint16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT16
    ret

;; Box an int32 value for dynamic storage.
;; Arguments: rdi = value (int32). Return type: value.
sybilant_Dbox_Dint32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT32
    ret

;; Box an int64 value for dynamic storage.
;; Arguments: rdi = value (int64). Return type: value.
sybilant_Dbox_Dint64:
    mov esi, SYBILANT_INT64_TYPE
    jmp sybilant_Dbox_Dinteger64

;; Box a nat8 value for dynamic storage.
;; Arguments: rdi = value (nat8). Return type: value.
sybilant_Dbox_Dnat8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT8
    ret

;; Box a nat16 value for dynamic storage.
;; Arguments: rdi = value (nat16). Return type: value.
sybilant_Dbox_Dnat16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT16
    ret

;; Box a nat32 value for dynamic storage.
;; Arguments: rdi = value (nat32). Return type: value.
sybilant_Dbox_Dnat32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT32
    ret

;; Box a nat64 value for dynamic storage.
;; Arguments: rdi = value (nat64). Return type: value.
sybilant_Dbox_Dnat64:
    mov esi, SYBILANT_NAT64_TYPE
    jmp sybilant_Dbox_Dinteger64

;; Box a 64-bit integer with a supplied runtime type.
;; Arguments: rdi = payload (uint64); rsi = type (type). Return type: value.
sybilant_Dbox_Dinteger64:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    mov edi, SYBILANT_BOXED_INTEGER_SIZE + SYBILANT_TAG_MASK
    call sybilant_Dmalloc_Dunchecked

    mov rdx, [rsp]
    mov rcx, [rsp + 8]
    add rsp, 24

    add rax, SYBILANT_TAG_MASK
    and rax, -8
    mov [rax + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], rcx
    mov [rax + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET], rdx
    ret

;; Check and unbox a uint8 value.
;; Arguments: rdi = value (value). Return type: uint8.
sybilant_Dunbox_Duint8:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT8
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Duint8_Dunchecked

;; Unbox a proven uint8 value.
;; Arguments: rdi = value (uint8). Return type: uint8.
sybilant_Dunbox_Duint8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dil
    ret

;; Check and unbox a uint16 value.
;; Arguments: rdi = value (value). Return type: uint16.
sybilant_Dunbox_Duint16:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT16
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Duint16_Dunchecked

;; Unbox a proven uint16 value.
;; Arguments: rdi = value (uint16). Return type: uint16.
sybilant_Dunbox_Duint16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, di
    ret

;; Check and unbox a uint32 value.
;; Arguments: rdi = value (value). Return type: uint32.
sybilant_Dunbox_Duint32:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT32
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Duint32_Dunchecked

;; Unbox a proven uint32 value.
;; Arguments: rdi = value (uint32). Return type: uint32.
sybilant_Dunbox_Duint32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    mov eax, edi
    ret

;; Check and unbox a uint64 value.
;; Arguments: rdi = value (value). Return type: uint64.
sybilant_Dunbox_Duint64:
    mov esi, SYBILANT_UINT64_TYPE
    jmp sybilant_Dunbox_Dinteger64

;; Check and unbox an int8 value.
;; Arguments: rdi = value (value). Return type: int8.
sybilant_Dunbox_Dint8:
    cmp dil, SYBILANT_EXTENDED_TAG_INT8
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dint8_Dunchecked

;; Unbox a proven int8 value.
;; Arguments: rdi = value (int8). Return type: int8.
sybilant_Dunbox_Dint8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, dil
    ret

;; Check and unbox an int16 value.
;; Arguments: rdi = value (value). Return type: int16.
sybilant_Dunbox_Dint16:
    cmp dil, SYBILANT_EXTENDED_TAG_INT16
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dint16_Dunchecked

;; Unbox a proven int16 value.
;; Arguments: rdi = value (int16). Return type: int16.
sybilant_Dunbox_Dint16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, di
    ret

;; Check and unbox an int32 value.
;; Arguments: rdi = value (value). Return type: int32.
sybilant_Dunbox_Dint32:
    cmp dil, SYBILANT_EXTENDED_TAG_INT32
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dint32_Dunchecked

;; Unbox a proven int32 value.
;; Arguments: rdi = value (int32). Return type: int32.
sybilant_Dunbox_Dint32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsxd rax, edi
    ret

;; Check and unbox an int64 value.
;; Arguments: rdi = value (value). Return type: int64.
sybilant_Dunbox_Dint64:
    mov esi, SYBILANT_INT64_TYPE
    jmp sybilant_Dunbox_Dinteger64

;; Check and unbox a nat8 value.
;; Arguments: rdi = value (value). Return type: nat8.
sybilant_Dunbox_Dnat8:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT8
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dnat8_Dunchecked

;; Unbox a proven nat8 value.
;; Arguments: rdi = value (nat8). Return type: nat8.
sybilant_Dunbox_Dnat8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dil
    ret

;; Check and unbox a nat16 value.
;; Arguments: rdi = value (value). Return type: nat16.
sybilant_Dunbox_Dnat16:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT16
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dnat16_Dunchecked

;; Unbox a proven nat16 value.
;; Arguments: rdi = value (nat16). Return type: nat16.
sybilant_Dunbox_Dnat16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, di
    ret

;; Check and unbox a nat32 value.
;; Arguments: rdi = value (value). Return type: nat32.
sybilant_Dunbox_Dnat32:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT32
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Dunbox_Dnat32_Dunchecked

;; Unbox a proven nat32 value.
;; Arguments: rdi = value (nat32). Return type: nat32.
sybilant_Dunbox_Dnat32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    mov eax, edi
    ret

;; Check and unbox a nat64 value.
;; Arguments: rdi = value (value). Return type: nat64.
sybilant_Dunbox_Dnat64:
    mov esi, SYBILANT_NAT64_TYPE
    jmp sybilant_Dunbox_Dinteger64

;; Check and unbox a 64-bit integer with a supplied runtime type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: integer.
sybilant_Dunbox_Dinteger64:
    test rdi, rdi
    jz sybilant_Dunbox_Dinteger_Dinvalid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_Dunbox_Dinteger_Dinvalid_argument

    cmp [rdi + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], rsi
    jne sybilant_Dunbox_Dinteger_Dinvalid_argument

    jmp sybilant_Dunbox_Duint64_Dunchecked

;; Unbox a proven uint64 value.
;; Arguments: rdi = value (uint64). Return type: uint64.
sybilant_Dunbox_Duint64_Dunchecked:
;; Unbox a proven int64 value.
;; Arguments: rdi = value (int64). Return type: int64.
sybilant_Dunbox_Dint64_Dunchecked:
;; Unbox a proven nat64 value.
;; Arguments: rdi = value (nat64). Return type: nat64.
sybilant_Dunbox_Dnat64_Dunchecked:
    mov rax, [rdi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    ret

sybilant_Dunbox_Dinteger_Dinvalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Dexit_Dunchecked

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

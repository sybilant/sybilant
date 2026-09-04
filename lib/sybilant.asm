bits 64
default rel

%include "lib/constants.asm"

section .text
global _start
global sybilant_Sbox_Dcodepoint
global sybilant_Sbox_Duint8
global sybilant_Sbox_Duint16
global sybilant_Sbox_Duint32
global sybilant_Sbox_Duint64
global sybilant_Sbox_Dint8
global sybilant_Sbox_Dint16
global sybilant_Sbox_Dint32
global sybilant_Sbox_Dint64
global sybilant_Sbox_Dnat8
global sybilant_Sbox_Dnat16
global sybilant_Sbox_Dnat32
global sybilant_Sbox_Dnat64
global sybilant_Sboolean_q
global sybilant_Sboolean_q_Dunchecked
global sybilant_S_e
global sybilant_Sexit
global sybilant_Sexit_Dunchecked
global sybilant_Sinstance_q
global sybilant_Sinstance_q_Dunchecked
global sybilant_Smalloc
global sybilant_Smalloc_Dunchecked
global sybilant_Stype
global sybilant_Stype_Dunchecked
global sybilant_Stype_e
global sybilant_Sunbox_Dcodepoint
global sybilant_Sunbox_Dcodepoint_Dunchecked
global sybilant_Sunbox_Duint8
global sybilant_Sunbox_Duint8_Dunchecked
global sybilant_Sunbox_Duint16
global sybilant_Sunbox_Duint16_Dunchecked
global sybilant_Sunbox_Duint32
global sybilant_Sunbox_Duint32_Dunchecked
global sybilant_Sunbox_Duint64
global sybilant_Sunbox_Duint64_Dunchecked
global sybilant_Sunbox_Dint8
global sybilant_Sunbox_Dint8_Dunchecked
global sybilant_Sunbox_Dint16
global sybilant_Sunbox_Dint16_Dunchecked
global sybilant_Sunbox_Dint32
global sybilant_Sunbox_Dint32_Dunchecked
global sybilant_Sunbox_Dint64
global sybilant_Sunbox_Dint64_Dunchecked
global sybilant_Sunbox_Dnat8
global sybilant_Sunbox_Dnat8_Dunchecked
global sybilant_Sunbox_Dnat16
global sybilant_Sunbox_Dnat16_Dunchecked
global sybilant_Sunbox_Dnat32
global sybilant_Sunbox_Dnat32_Dunchecked
global sybilant_Sunbox_Dnat64
global sybilant_Sunbox_Dnat64_Dunchecked
extern sybilant_darray_S_e
extern sybilant_datom_S_e
extern sybilant_dstring_S_e
extern sybilant_dthread_Sinitialize_Dmain
extern sybilant_Smain

_start:
    call sybilant_dthread_Sinitialize_Dmain

    mov rax, SYBILANT_MALLOC_START
    mov [rel sybilant_Dmalloc_Dstart], rax
    mov [rel sybilant_Dmalloc_Dmaximum], rax

    call sybilant_Smain

    mov edi, eax
    call sybilant_Sexit
    ud2

;; Check the status and exit the process.
;; Arguments: rdi = status (uint8). Return type: never.
sybilant_Sexit:
    cmp rdi, 0xff
    jbe sybilant_Sexit_Dunchecked

    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT

;; Exit the process with a proven status.
;; Arguments: rdi = status (uint8). Return type: never.
sybilant_Sexit_Dunchecked:
    mov eax, SYS_EXIT
    syscall
    ud2

;; Return a value's runtime type, checking the result, or nil for nil.
;; Arguments: rdi = value (value). Return type: type or nil.
sybilant_Stype:
    sub rsp, 8
    call sybilant_Stype_Dunchecked
    add rsp, 8

    cmp rax, SYBILANT_NIL
    je .valid_return

    test rax, SYBILANT_TAG_MASK
    jz .pointer_type

    mov rdx, rax
    and edx, SYBILANT_EXTENDED_TAG_MASK
    cmp edx, SYBILANT_EXTENDED_TAG_TYPE
    jne .invalid_state
    jmp .valid_return

.pointer_type:
    cmp qword [rax], SYBILANT_TYPE_TYPE
    jne .invalid_state

.valid_return:
    ret

.invalid_state:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

;; Return the runtime type of a proven value, or nil for nil.
;; Arguments: rdi = value (value). Return type: type or nil.
sybilant_Stype_Dunchecked:
    cmp rdi, SYBILANT_NIL
    je .nil

    cmp rdi, SYBILANT_FALSE
    je .boolean

    cmp rdi, SYBILANT_TRUE
    je .boolean

    mov eax, edi
    and eax, SYBILANT_EXTENDED_TAG_MASK

    cmp eax, SYBILANT_EXTENDED_TAG_TYPE
    je .type

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

    cmp eax, SYBILANT_EXTENDED_TAG_CODEPOINT
    je .codepoint

    mov rax, [rdi]
    ret

.nil:
    mov eax, SYBILANT_NIL
    ret

.boolean:
    mov eax, SYBILANT_BOOLEAN_TYPE
    ret

.type:
    mov eax, SYBILANT_TYPE_TYPE
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

.codepoint:
    mov eax, SYBILANT_CODEPOINT_TYPE
    ret

;; Check the arguments and return whether a value is an instance of a type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: boolean.
sybilant_Sinstance_q:
    cmp rsi, SYBILANT_NIL
    je .invalid_argument

    test rsi, SYBILANT_TAG_MASK
    jz .pointer_type

    mov rax, rsi
    and eax, SYBILANT_EXTENDED_TAG_MASK
    cmp eax, SYBILANT_EXTENDED_TAG_TYPE
    jne .invalid_argument
    jmp .valid_type

.pointer_type:
    cmp qword [rsi], SYBILANT_TYPE_TYPE
    jne .invalid_argument

.valid_type:
    push rsi
    call sybilant_Stype
    pop rsi
    mov rdi, rax
    jmp sybilant_S_e

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

;; Return whether a proven value is an instance of a proven type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: boolean.
sybilant_Sinstance_q_Dunchecked:
    push rsi
    call sybilant_Stype_Dunchecked
    pop rsi
    mov rdi, rax
    jmp sybilant_S_e

;; Check a value and return whether it is a boolean.
;; Arguments: rdi = value (value). Return type: boolean.
sybilant_Sboolean_q:
    mov esi, SYBILANT_BOOLEAN_TYPE
    jmp sybilant_Sinstance_q

;; Return whether a proven value is a boolean.
;; Arguments: rdi = value (value). Return type: boolean.
sybilant_Sboolean_q_Dunchecked:
    mov esi, SYBILANT_BOOLEAN_TYPE
    jmp sybilant_Sinstance_q_Dunchecked

;; Check two values and return whether they are equal.
;; Arguments: rdi = left (value); rsi = right (value). Return type: boolean.
sybilant_S_e:
    cmp rdi, rsi
    je .true

    cmp rdi, SYBILANT_NIL
    je .false

    test rdi, SYBILANT_TAG_MASK
    jnz .false

    cmp rsi, SYBILANT_NIL
    je .false

    test rsi, SYBILANT_TAG_MASK
    jnz .false

    push r12
    push r13
    push r14

    mov r12, rdi
    mov r13, rsi
    call sybilant_Stype
    mov r14, rax

    mov rdi, r13
    call sybilant_Stype

    mov rdi, r14
    mov rsi, rax
    call sybilant_S_e

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
;; Dispatch equality for type rdx with distinct heap values rdi and rsi.
    cmp rdx, SYBILANT_TYPE_TYPE
    je sybilant_Stype_e

    cmp rdx, SYBILANT_UINT64_TYPE
    je .integer64

    cmp rdx, SYBILANT_INT64_TYPE
    je .integer64

    cmp rdx, SYBILANT_NAT64_TYPE
    je .integer64

    cmp rdx, SYBILANT_STRING_TYPE
    je sybilant_dstring_S_e

;; A heap type without a defined equality compares by identity, which the
;; entry check already handled, so distinct values are unequal.
    test rdx, SYBILANT_TAG_MASK
    jnz .false

    mov rax, [rdx + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET]

    cmp rax, SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    je sybilant_darray_S_e

    cmp rax, SYBILANT_ATOM_TYPE_CONSTRUCTOR
    je sybilant_datom_S_e
    jmp .false

.integer64:
    mov rax, [rdi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    cmp rax, [rsi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    je .true
    jmp .false

.false:
    mov eax, SYBILANT_FALSE
    ret

.true:
    mov eax, SYBILANT_TRUE
    ret

;; Return whether two distinct type values describe the same type.
;; Arguments: rdi = left (type); rsi = right (type). Return type: boolean.
sybilant_Stype_e:
    mov rax, [rdi + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET]
    cmp rax, [rsi + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET]
    jne .false

    cmp rax, SYBILANT_ATOM_TYPE_CONSTRUCTOR
    je .atom_type

;; A type descriptor with an unknown constructor compares by identity, which
;; the entry check already handled, so distinct descriptors are unequal.
    cmp rax, SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    jne .false

    mov eax, [rdi + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    cmp eax, [rsi + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    jne .false

    mov rdi, [rdi + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov rsi, [rsi + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    jmp sybilant_S_e

.atom_type:
    mov rdi, [rdi + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET]
    mov rsi, [rsi + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET]
    jmp sybilant_S_e

.false:
    mov eax, SYBILANT_FALSE
    ret

;; Box a codepoint value for dynamic storage.
;; Arguments: rdi = value (codepoint). Return type: value.
sybilant_Sbox_Dcodepoint:
    mov eax, edi
    and eax, SYBILANT_CODEPOINT_PAYLOAD_MASK
    shl rax, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_CODEPOINT
    ret

;; Check and unbox a codepoint value.
;; Arguments: rdi = value (value). Return type: codepoint.
sybilant_Sunbox_Dcodepoint:
    cmp dil, SYBILANT_EXTENDED_TAG_CODEPOINT
    jne .invalid_argument
    jmp sybilant_Sunbox_Dcodepoint_Dunchecked

.invalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

;; Unbox a proven codepoint value.
;; Arguments: rdi = value (codepoint). Return type: codepoint.
sybilant_Sunbox_Dcodepoint_Dunchecked:
    shr rdi, SYBILANT_CODEPOINT_PAYLOAD_SHIFT
    mov eax, edi
    and eax, SYBILANT_CODEPOINT_PAYLOAD_MASK
    ret

;; Box a uint8 value for dynamic storage.
;; Arguments: rdi = value (uint8). Return type: value.
sybilant_Sbox_Duint8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT8
    ret

;; Box a uint16 value for dynamic storage.
;; Arguments: rdi = value (uint16). Return type: value.
sybilant_Sbox_Duint16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT16
    ret

;; Box a uint32 value for dynamic storage.
;; Arguments: rdi = value (uint32). Return type: value.
sybilant_Sbox_Duint32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_UINT32
    ret

;; Box a uint64 value for dynamic storage.
;; Arguments: rdi = value (uint64). Return type: value.
sybilant_Sbox_Duint64:
    mov esi, SYBILANT_UINT64_TYPE
    jmp sybilant_Sbox_Dinteger64

;; Box an int8 value for dynamic storage.
;; Arguments: rdi = value (int8). Return type: value.
sybilant_Sbox_Dint8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT8
    ret

;; Box an int16 value for dynamic storage.
;; Arguments: rdi = value (int16). Return type: value.
sybilant_Sbox_Dint16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT16
    ret

;; Box an int32 value for dynamic storage.
;; Arguments: rdi = value (int32). Return type: value.
sybilant_Sbox_Dint32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_INT32
    ret

;; Box an int64 value for dynamic storage.
;; Arguments: rdi = value (int64). Return type: value.
sybilant_Sbox_Dint64:
    mov esi, SYBILANT_INT64_TYPE
    jmp sybilant_Sbox_Dinteger64

;; Box a nat8 value for dynamic storage.
;; Arguments: rdi = value (nat8). Return type: value.
sybilant_Sbox_Dnat8:
    movzx eax, dil
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT8
    ret

;; Box a nat16 value for dynamic storage.
;; Arguments: rdi = value (nat16). Return type: value.
sybilant_Sbox_Dnat16:
    movzx eax, di
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT16
    ret

;; Box a nat32 value for dynamic storage.
;; Arguments: rdi = value (nat32). Return type: value.
sybilant_Sbox_Dnat32:
    mov eax, edi
    shl rax, SYBILANT_INTEGER_PAYLOAD_SHIFT
    or rax, SYBILANT_EXTENDED_TAG_NAT32
    ret

;; Box a nat64 value for dynamic storage.
;; Arguments: rdi = value (nat64). Return type: value.
sybilant_Sbox_Dnat64:
    mov esi, SYBILANT_NAT64_TYPE
    jmp sybilant_Sbox_Dinteger64

;; Box a 64-bit integer with a supplied runtime type.
;; Arguments: rdi = payload (uint64); rsi = type (type). Return type: value.
sybilant_Sbox_Dinteger64:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    mov edi, SYBILANT_BOXED_INTEGER_SIZE + SYBILANT_TAG_MASK
    call sybilant_Smalloc_Dunchecked

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
sybilant_Sunbox_Duint8:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT8
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Duint8_Dunchecked

;; Unbox a proven uint8 value.
;; Arguments: rdi = value (uint8). Return type: uint8.
sybilant_Sunbox_Duint8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dil
    ret

;; Check and unbox a uint16 value.
;; Arguments: rdi = value (value). Return type: uint16.
sybilant_Sunbox_Duint16:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT16
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Duint16_Dunchecked

;; Unbox a proven uint16 value.
;; Arguments: rdi = value (uint16). Return type: uint16.
sybilant_Sunbox_Duint16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, di
    ret

;; Check and unbox a uint32 value.
;; Arguments: rdi = value (value). Return type: uint32.
sybilant_Sunbox_Duint32:
    cmp dil, SYBILANT_EXTENDED_TAG_UINT32
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Duint32_Dunchecked

;; Unbox a proven uint32 value.
;; Arguments: rdi = value (uint32). Return type: uint32.
sybilant_Sunbox_Duint32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    mov eax, edi
    ret

;; Check and unbox a uint64 value.
;; Arguments: rdi = value (value). Return type: uint64.
sybilant_Sunbox_Duint64:
    mov esi, SYBILANT_UINT64_TYPE
    jmp sybilant_Sunbox_Dinteger64

;; Check and unbox an int8 value.
;; Arguments: rdi = value (value). Return type: int8.
sybilant_Sunbox_Dint8:
    cmp dil, SYBILANT_EXTENDED_TAG_INT8
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dint8_Dunchecked

;; Unbox a proven int8 value.
;; Arguments: rdi = value (int8). Return type: int8.
sybilant_Sunbox_Dint8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, dil
    ret

;; Check and unbox an int16 value.
;; Arguments: rdi = value (value). Return type: int16.
sybilant_Sunbox_Dint16:
    cmp dil, SYBILANT_EXTENDED_TAG_INT16
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dint16_Dunchecked

;; Unbox a proven int16 value.
;; Arguments: rdi = value (int16). Return type: int16.
sybilant_Sunbox_Dint16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, di
    ret

;; Check and unbox an int32 value.
;; Arguments: rdi = value (value). Return type: int32.
sybilant_Sunbox_Dint32:
    cmp dil, SYBILANT_EXTENDED_TAG_INT32
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dint32_Dunchecked

;; Unbox a proven int32 value.
;; Arguments: rdi = value (int32). Return type: int32.
sybilant_Sunbox_Dint32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsxd rax, edi
    ret

;; Check and unbox an int64 value.
;; Arguments: rdi = value (value). Return type: int64.
sybilant_Sunbox_Dint64:
    mov esi, SYBILANT_INT64_TYPE
    jmp sybilant_Sunbox_Dinteger64

;; Check and unbox a nat8 value.
;; Arguments: rdi = value (value). Return type: nat8.
sybilant_Sunbox_Dnat8:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT8
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dnat8_Dunchecked

;; Unbox a proven nat8 value.
;; Arguments: rdi = value (nat8). Return type: nat8.
sybilant_Sunbox_Dnat8_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dil
    ret

;; Check and unbox a nat16 value.
;; Arguments: rdi = value (value). Return type: nat16.
sybilant_Sunbox_Dnat16:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT16
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dnat16_Dunchecked

;; Unbox a proven nat16 value.
;; Arguments: rdi = value (nat16). Return type: nat16.
sybilant_Sunbox_Dnat16_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, di
    ret

;; Check and unbox a nat32 value.
;; Arguments: rdi = value (value). Return type: nat32.
sybilant_Sunbox_Dnat32:
    cmp dil, SYBILANT_EXTENDED_TAG_NAT32
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument
    jmp sybilant_Sunbox_Dnat32_Dunchecked

;; Unbox a proven nat32 value.
;; Arguments: rdi = value (nat32). Return type: nat32.
sybilant_Sunbox_Dnat32_Dunchecked:
    shr rdi, SYBILANT_INTEGER_PAYLOAD_SHIFT
    mov eax, edi
    ret

;; Check and unbox a nat64 value.
;; Arguments: rdi = value (value). Return type: nat64.
sybilant_Sunbox_Dnat64:
    mov esi, SYBILANT_NAT64_TYPE
    jmp sybilant_Sunbox_Dinteger64

;; Check and unbox a 64-bit integer with a supplied runtime type.
;; Arguments: rdi = value (value); rsi = type (type). Return type: integer.
sybilant_Sunbox_Dinteger64:
    test rdi, rdi
    jz sybilant_Sunbox_Dinteger_Dinvalid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_Sunbox_Dinteger_Dinvalid_argument

    cmp [rdi + SYBILANT_BOXED_INTEGER_TYPE_OFFSET], rsi
    jne sybilant_Sunbox_Dinteger_Dinvalid_argument

    jmp sybilant_Sunbox_Duint64_Dunchecked

;; Unbox a proven uint64 value.
;; Arguments: rdi = value (uint64). Return type: uint64.
sybilant_Sunbox_Duint64_Dunchecked:
;; Unbox a proven int64 value.
;; Arguments: rdi = value (int64). Return type: int64.
sybilant_Sunbox_Dint64_Dunchecked:
;; Unbox a proven nat64 value.
;; Arguments: rdi = value (nat64). Return type: nat64.
sybilant_Sunbox_Dnat64_Dunchecked:
    mov rax, [rdi + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    ret

sybilant_Sunbox_Dinteger_Dinvalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

;; Allocate a contiguous region with a dynamically supplied byte count.
;; Arguments: rdi = byte count (uint64). Return type: Pointer.
sybilant_Smalloc:

;; Allocate a contiguous region with a proven byte count.
;; Arguments: rdi = byte count (uint64). Return type: Pointer.
sybilant_Smalloc_Dunchecked:
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
    jmp sybilant_Sexit_Dunchecked

.out_of_memory:
    mov edi, SYBILANT_ERROR_OUT_OF_MEMORY
    jmp sybilant_Sexit_Dunchecked

section .bss
align 8
sybilant_Dmalloc_Dstart:
    resq 1
sybilant_Dmalloc_Dmaximum:
    resq 1

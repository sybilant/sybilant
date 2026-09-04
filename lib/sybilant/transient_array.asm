bits 64
default rel

%include "lib/constants.asm"

section .text

global sybilant_darray_Stransient_Dnew_B
global sybilant_darray_Stransient_Dnew_B_Dunchecked
global sybilant_darray_Stransient_Dlength_B
global sybilant_darray_Stransient_Dlength_B_Dunchecked
global sybilant_darray_Stransient_Dget_B
global sybilant_darray_Stransient_Dget_B_Dunchecked
global sybilant_darray_Stransient_Dset_B
global sybilant_darray_Stransient_Dset_B_Dunchecked
global sybilant_darray_Stransient_Dinsert_B
global sybilant_darray_Stransient_Dinsert_B_Dunchecked
global sybilant_darray_Stransient_Ddelete_B
global sybilant_darray_Stransient_Ddelete_B_Dunchecked
global sybilant_darray_Stransient_Dresize_B
global sybilant_darray_Stransient_Dresize_B_Dunchecked
global sybilant_darray_Stransient_Dpersistent_B
global sybilant_darray_Stransient_Dpersistent_B_Dunchecked

extern sybilant_Sbox_Dint8
extern sybilant_Sbox_Dint16
extern sybilant_Sbox_Dint32
extern sybilant_Sbox_Dint64
extern sybilant_Sbox_Dnat8
extern sybilant_Sbox_Dnat16
extern sybilant_Sbox_Dnat32
extern sybilant_Sbox_Dnat64
extern sybilant_Sbox_Duint8
extern sybilant_Sbox_Dcodepoint
extern sybilant_Sbox_Duint16
extern sybilant_Sbox_Duint32
extern sybilant_Sbox_Duint64
extern sybilant_Sexit_Dunchecked
extern sybilant_Sinstance_q
extern sybilant_Smalloc_Dunchecked
extern sybilant_Sunbox_Dint8
extern sybilant_Sunbox_Dint16
extern sybilant_Sunbox_Dint32
extern sybilant_Sunbox_Dint64
extern sybilant_Sunbox_Dnat8
extern sybilant_Sunbox_Dnat16
extern sybilant_Sunbox_Dnat32
extern sybilant_Sunbox_Dnat64
extern sybilant_Sunbox_Duint8
extern sybilant_Sunbox_Dcodepoint
extern sybilant_Sunbox_Duint16
extern sybilant_Sunbox_Duint32
extern sybilant_Sunbox_Duint64
extern sybilant_datom_Sderef
extern sybilant_dthread_Sself

;; Create a transient array after its type, length, and editor have been
;; validated. Arguments: rdi = element type; rsi = editor; rdx = length;
;; rcx = packed element stride.
sybilant_darray_Stransient_Dallocate:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov r15, rcx
    mov rax, r14
    mul r15
    jo sybilant_darray_Stransient_Dinvalid_Dargument
    add rax, SYBILANT_ARRAY_TYPE_SIZE + SYBILANT_ARRAY_DATA_OFFSET + SYBILANT_TAG_MASK
    jc sybilant_darray_Stransient_Dinvalid_Dargument
    mov rdi, rax
    call sybilant_Smalloc_Dunchecked
    mov [rsp], rax

    add rax, SYBILANT_TAG_MASK
    and rax, -8
    mov qword [rax + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    mov qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR
    mov [rax + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET], r12
    mov [rax + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET], r15d
    mov dword [rax + SYBILANT_ARRAY_TYPE_LAYOUT_FLAGS_OFFSET], 0

    lea rdx, [rax + SYBILANT_ARRAY_TYPE_SIZE]
    mov [rdx + SYBILANT_ARRAY_TYPE_OFFSET], rax
    mov [rdx + SYBILANT_ARRAY_EDITOR_OFFSET], r13
    mov [rdx + SYBILANT_ARRAY_LENGTH_OFFSET], r14
    mov rdi, rdx
    mov rsi, r14
    mov rax, r15
    mul rsi
    mov rcx, rax
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    xor eax, eax
    rep stosb
    mov rax, [rsp]
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    add rax, SYBILANT_TAG_MASK
    and rax, -8
    add rax, SYBILANT_ARRAY_TYPE_SIZE
    ret

;; Validate an element type and return its packed stride in eax.
sybilant_darray_Stransient_Dstride:
    cmp rdi, SYBILANT_NIL
    je sybilant_darray_Stransient_Dinvalid_Dargument
    test rdi, SYBILANT_TAG_MASK
    jz .pointer
    mov rax, rdi
    and eax, SYBILANT_EXTENDED_TAG_MASK
    cmp eax, SYBILANT_EXTENDED_TAG_TYPE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    cmp rdi, SYBILANT_UINT8_TYPE
    je .one
    cmp rdi, SYBILANT_INT8_TYPE
    je .one
    cmp rdi, SYBILANT_NAT8_TYPE
    je .one
    cmp rdi, SYBILANT_UINT16_TYPE
    je .two
    cmp rdi, SYBILANT_INT16_TYPE
    je .two
    cmp rdi, SYBILANT_NAT16_TYPE
    je .two
    cmp rdi, SYBILANT_UINT32_TYPE
    je .four
    cmp rdi, SYBILANT_INT32_TYPE
    je .four
    cmp rdi, SYBILANT_NAT32_TYPE
    je .four
    cmp rdi, SYBILANT_CODEPOINT_TYPE
    je .four
    cmp rdi, SYBILANT_UINT64_TYPE
    je .eight
    cmp rdi, SYBILANT_INT64_TYPE
    je .eight
    cmp rdi, SYBILANT_NAT64_TYPE
    je .eight
    cmp rdi, SYBILANT_BOOLEAN_TYPE
    je .eight
    jmp sybilant_darray_Stransient_Dinvalid_Dstate
.pointer:
    cmp qword [rdi + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
.eight:
    mov eax, 8
    ret
.four:
    mov eax, 4
    ret
.two:
    mov eax, 2
    ret
.one:
    mov eax, 1
    ret

;; Validate a transient array and return its element type.
sybilant_darray_Stransient_Dguard:
    cmp rdi, SYBILANT_NIL
    je sybilant_darray_Stransient_Dinvalid_Dargument
    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_darray_Stransient_Dinvalid_Dargument
    mov rax, [rdi + SYBILANT_ARRAY_TYPE_OFFSET]
    test rax, SYBILANT_TAG_MASK
    jnz sybilant_darray_Stransient_Dinvalid_Dargument
    cmp qword [rax + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    cmp qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_TRANSIENT_ARRAY_TYPE_CONSTRUCTOR
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    cmp dword [rax + SYBILANT_ARRAY_TYPE_LAYOUT_FLAGS_OFFSET], 0
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    mov rdx, [rax + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov rsi, [rax + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov rdi, rdx
    call sybilant_darray_Stransient_Dstride
    cmp eax, esi
    jne sybilant_darray_Stransient_Dinvalid_Dstate
    mov rax, rdx
    ret

;; Require that a transient's editor atom contains the current thread.
sybilant_darray_Stransient_Dcheck_Deditor:
    push r12
    mov r12, rdi
    mov rdi, [r12 + SYBILANT_ARRAY_EDITOR_OFFSET]
    test rdi, rdi
    jz sybilant_darray_Stransient_Dinvalid_Dstate
    call sybilant_datom_Sderef
    mov rdx, rax
    call sybilant_dthread_Sself
    cmp rdx, rax
    pop r12
    jne sybilant_darray_Stransient_Dinvalid_Dstate
    ret

;; Decode a dynamic non-negative integer length into rax.
sybilant_darray_Stransient_Dlength_Dvalue:
    mov rdx, rdi
    test rdx, rdx
    jz sybilant_darray_Stransient_Dinvalid_Dargument
    mov eax, edi
    and eax, SYBILANT_EXTENDED_TAG_MASK
    cmp eax, SYBILANT_EXTENDED_TAG_UINT8
    je .uint8
    cmp eax, SYBILANT_EXTENDED_TAG_UINT16
    je .uint16
    cmp eax, SYBILANT_EXTENDED_TAG_UINT32
    je .uint32
    cmp eax, SYBILANT_EXTENDED_TAG_NAT8
    je .nat8
    cmp eax, SYBILANT_EXTENDED_TAG_NAT16
    je .nat16
    cmp eax, SYBILANT_EXTENDED_TAG_NAT32
    je .nat32
    cmp eax, SYBILANT_EXTENDED_TAG_INT8
    je .int8
    cmp eax, SYBILANT_EXTENDED_TAG_INT16
    je .int16
    cmp eax, SYBILANT_EXTENDED_TAG_INT32
    je .int32
    test rdx, SYBILANT_TAG_MASK
    jnz sybilant_darray_Stransient_Dinvalid_Dargument
    mov rax, [rdx + SYBILANT_BOXED_INTEGER_TYPE_OFFSET]
    cmp rax, SYBILANT_UINT64_TYPE
    je .boxed
    cmp rax, SYBILANT_NAT64_TYPE
    je .boxed
    cmp rax, SYBILANT_INT64_TYPE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
.boxed:
    mov rax, [rdx + SYBILANT_BOXED_INTEGER_PAYLOAD_OFFSET]
    test rax, rax
    js sybilant_darray_Stransient_Dinvalid_Dargument
    ret
.uint8:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dl
    ret
.uint16:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movzx eax, dx
    ret
.uint32:
.nat32:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    mov eax, edx
    ret
.nat8:
    jmp .uint8
.nat16:
    jmp .uint16
.int8:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, dl
    test rax, rax
    js sybilant_darray_Stransient_Dinvalid_Dargument
    ret
.int16:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsx rax, dx
    test rax, rax
    js sybilant_darray_Stransient_Dinvalid_Dargument
    ret
.int32:
    shr rdx, SYBILANT_INTEGER_PAYLOAD_SHIFT
    movsxd rax, edx
    test rax, rax
    js sybilant_darray_Stransient_Dinvalid_Dargument
    ret

;; Read an element without type or editor checks. The index is raw.
sybilant_darray_Stransient_Dget_Draw:
    cmp rsi, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rdx, [rdi + SYBILANT_ARRAY_TYPE_OFFSET]
    mov edx, [rdx + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    cmp edx, 1
    je .byte
    cmp edx, 2
    je .word
    cmp edx, 4
    je .doubleword
    cmp edx, 8
    jne sybilant_darray_Stransient_Dinvalid_Dstate
    mov rax, [rdi + rsi * 8 + SYBILANT_ARRAY_DATA_OFFSET]
    ret
.byte:
    movzx eax, byte [rdi + rsi + SYBILANT_ARRAY_DATA_OFFSET]
    ret
.word:
    movzx eax, word [rdi + rsi * 2 + SYBILANT_ARRAY_DATA_OFFSET]
    ret
.doubleword:
    mov eax, [rdi + rsi * 4 + SYBILANT_ARRAY_DATA_OFFSET]
    ret

;; Store a raw element without type or editor checks. The value is unboxed.
sybilant_darray_Stransient_Dset_Draw:
    cmp rsi, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rcx, [rdi + SYBILANT_ARRAY_TYPE_OFFSET]
    mov ecx, [rcx + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    cmp ecx, 1
    je .byte
    cmp ecx, 2
    je .word
    cmp ecx, 4
    je .doubleword
    cmp ecx, 8
    jne sybilant_darray_Stransient_Dinvalid_Dstate
    mov [rdi + rsi * 8 + SYBILANT_ARRAY_DATA_OFFSET], rdx
    ret
.byte:
    mov [rdi + rsi + SYBILANT_ARRAY_DATA_OFFSET], dl
    ret
.word:
    mov [rdi + rsi * 2 + SYBILANT_ARRAY_DATA_OFFSET], dx
    ret
.doubleword:
    mov [rdi + rsi * 4 + SYBILANT_ARRAY_DATA_OFFSET], edx
    ret

;; Copy rcx bytes from rsi to rdi.
sybilant_darray_Stransient_Dcopy:
    rep movsb
    ret

;; Convert a checked dynamic value to its raw element representation.
;; Arguments: rdi = value; rsi = element type. Return: rax = raw value.
sybilant_darray_Stransient_Dvalue_Draw:
    cmp rsi, SYBILANT_UINT8_TYPE
    je sybilant_Sunbox_Duint8
    cmp rsi, SYBILANT_UINT16_TYPE
    je sybilant_Sunbox_Duint16
    cmp rsi, SYBILANT_UINT32_TYPE
    je sybilant_Sunbox_Duint32
    cmp rsi, SYBILANT_UINT64_TYPE
    je sybilant_Sunbox_Duint64
    cmp rsi, SYBILANT_INT8_TYPE
    je sybilant_Sunbox_Dint8
    cmp rsi, SYBILANT_INT16_TYPE
    je sybilant_Sunbox_Dint16
    cmp rsi, SYBILANT_INT32_TYPE
    je sybilant_Sunbox_Dint32
    cmp rsi, SYBILANT_INT64_TYPE
    je sybilant_Sunbox_Dint64
    cmp rsi, SYBILANT_NAT8_TYPE
    je sybilant_Sunbox_Dnat8
    cmp rsi, SYBILANT_NAT16_TYPE
    je sybilant_Sunbox_Dnat16
    cmp rsi, SYBILANT_NAT32_TYPE
    je sybilant_Sunbox_Dnat32
    cmp rsi, SYBILANT_NAT64_TYPE
    je sybilant_Sunbox_Dnat64
    cmp rsi, SYBILANT_CODEPOINT_TYPE
    je sybilant_Sunbox_Dcodepoint
    mov rax, rdi
    ret

;; Box a raw element according to its element type.
sybilant_darray_Stransient_Dvalue_Dboxed:
    cmp rsi, SYBILANT_UINT8_TYPE
    je sybilant_Sbox_Duint8
    cmp rsi, SYBILANT_UINT16_TYPE
    je sybilant_Sbox_Duint16
    cmp rsi, SYBILANT_UINT32_TYPE
    je sybilant_Sbox_Duint32
    cmp rsi, SYBILANT_UINT64_TYPE
    je sybilant_Sbox_Duint64
    cmp rsi, SYBILANT_INT8_TYPE
    je sybilant_Sbox_Dint8
    cmp rsi, SYBILANT_INT16_TYPE
    je sybilant_Sbox_Dint16
    cmp rsi, SYBILANT_INT32_TYPE
    je sybilant_Sbox_Dint32
    cmp rsi, SYBILANT_INT64_TYPE
    je sybilant_Sbox_Dint64
    cmp rsi, SYBILANT_NAT8_TYPE
    je sybilant_Sbox_Dnat8
    cmp rsi, SYBILANT_NAT16_TYPE
    je sybilant_Sbox_Dnat16
    cmp rsi, SYBILANT_NAT32_TYPE
    je sybilant_Sbox_Dnat32
    cmp rsi, SYBILANT_NAT64_TYPE
    je sybilant_Sbox_Dnat64
    cmp rsi, SYBILANT_CODEPOINT_TYPE
    je sybilant_Sbox_Dcodepoint
    mov rax, rdi
    ret

;; Dynamic transient constructor: element type, boxed length, editor atom.
sybilant_darray_Stransient_Dnew_B:
    push r12
    push r13
    push r14
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov rdi, r12
    call sybilant_darray_Stransient_Dstride
    mov [rsp], rax
    mov rdi, r13
    call sybilant_darray_Stransient_Dlength_Dvalue
    mov r13, rax
    mov rdi, r14
    call sybilant_datom_Sderef
    mov rdx, rax
    call sybilant_dthread_Sself
    cmp rdx, rax
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    mov rdi, r12
    mov rsi, r14
    mov rdx, r13
    mov rcx, [rsp]
    call sybilant_darray_Stransient_Dallocate
    add rsp, 8
    pop r14
    pop r13
    pop r12
    ret

;; Unchecked transient constructor: element type, raw length, editor atom.
sybilant_darray_Stransient_Dnew_B_Dunchecked:
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    mov rdi, r12
    call sybilant_darray_Stransient_Dstride
    mov rcx, rax
    mov rdi, r12
    mov rsi, rdx
    mov rdx, r13
    call sybilant_darray_Stransient_Dallocate
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dlength_B:
    push r12
    mov r12, rdi
    call sybilant_darray_Stransient_Dguard
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    mov rdi, r12
    pop r12
sybilant_darray_Stransient_Dlength_B_Dunchecked:
    mov rax, [rdi + SYBILANT_ARRAY_LENGTH_OFFSET]
    ret

sybilant_darray_Stransient_Dget_B:
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    call sybilant_darray_Stransient_Dguard
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    mov rdi, r12
    mov rsi, r13
    call sybilant_darray_Stransient_Dget_Draw
    mov rdi, rax
    mov rsi, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov rsi, [rsi + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    call sybilant_darray_Stransient_Dvalue_Dboxed
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dget_B_Dunchecked:
    jmp sybilant_darray_Stransient_Dget_Draw

sybilant_darray_Stransient_Dset_B:
    push r12
    push r13
    push r14
    push r15
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    call sybilant_darray_Stransient_Dguard
    mov r15, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    mov rsi, r15
    mov rdi, r14
    call sybilant_Sinstance_q
    cmp rax, SYBILANT_TRUE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    mov rsi, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov rsi, [rsi + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov rdi, r14
    call sybilant_darray_Stransient_Dvalue_Draw
    mov rdx, rax
    mov rdi, r12
    mov rsi, r13
    call sybilant_darray_Stransient_Dset_Draw
    mov rax, r12
    pop r15
    pop r14
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dset_B_Dunchecked:
    jmp sybilant_darray_Stransient_Dset_Draw

;; Allocate a new transient and copy data, invalidating the old editor.
;; rdi=old, rsi=new length; returns new transient in rax.
sybilant_darray_Stransient_Dresize:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8
    mov r12, rdi
    mov r13, rsi
    mov r14, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov r15, [r12 + SYBILANT_ARRAY_EDITOR_OFFSET]
    mov rdi, [r14 + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov rsi, r15
    mov rdx, r13
    mov ecx, [r14 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    call sybilant_darray_Stransient_Dallocate
    mov [rsp], rax
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    cmp rax, r13
    cmova rax, r13
    mov rdi, [r14 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mul rdi
    mov rcx, rax
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    lea rsi, [r12 + SYBILANT_ARRAY_DATA_OFFSET]
    call sybilant_darray_Stransient_Dcopy
    mov qword [r12 + SYBILANT_ARRAY_EDITOR_OFFSET], SYBILANT_NIL
    mov rax, [rsp]
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dresize_B:
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    call sybilant_darray_Stransient_Dguard
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    mov rdi, r13
    call sybilant_darray_Stransient_Dlength_Dvalue
    mov rsi, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dresize
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dresize_B_Dunchecked:
    jmp sybilant_darray_Stransient_Dresize

;; Insert/delete allocate through the same resize path, then copy around the
;; affected element. Dynamic values are boxed and validated first.
sybilant_darray_Stransient_Dinsert_B:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov [rsp + 16], r13
    call sybilant_darray_Stransient_Dguard
    mov r15, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    cmp r13, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    ja sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rdi, r14
    mov rsi, r15
    call sybilant_Sinstance_q
    cmp rax, SYBILANT_TRUE
    jne sybilant_darray_Stransient_Dinvalid_Dargument
    mov rsi, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov rsi, [rsi + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov rdi, r14
    call sybilant_darray_Stransient_Dvalue_Draw
    mov r14, rax
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    inc rax
    mov rsi, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dresize
    mov [rsp], rax
    mov r15, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov eax, [r15 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov r15, rax
    mov rax, [rsp + 16]
    mul r15
    mov [rsp + 8], rax
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    add rdi, [rsp + 8]
    mov rdx, r14
    mov rsi, rdi
    cmp r15, 8
    je .store_qword
    cmp r15, 4
    je .store_dword
    cmp r15, 2
    je .store_word
    mov [rdi], dl
    jmp .insert_copy
.store_word:
    mov [rdi], dx
    jmp .insert_copy
.store_dword:
    mov [rdi], edx
    jmp .insert_copy
.store_qword:
    mov [rdi], rdx
.insert_copy:
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rax, [rsp + 16]
    mov rcx, rax
    mul r15
    mov rcx, rax
    lea rsi, [r12 + SYBILANT_ARRAY_DATA_OFFSET]
    add rsi, [rsp + 8]
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    add rdi, [rsp + 8]
    add rdi, r15
    call sybilant_darray_Stransient_Dcopy
    mov rax, [rsp]
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dinsert_B_Dunchecked:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24
    mov r12, rdi
    mov r13, rsi
    mov r14, rdx
    mov [rsp + 16], r13
    cmp r13, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    ja sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    inc rax
    mov rsi, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dresize
    mov [rsp], rax
    mov r15, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov eax, [r15 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov r15, rax
    mov rax, [rsp + 16]
    mul r15
    mov [rsp + 8], rax
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    add rdi, [rsp + 8]
    mov rdx, r14
    cmp r15, 8
    je .unchecked_insert_qword
    cmp r15, 4
    je .unchecked_insert_dword
    cmp r15, 2
    je .unchecked_insert_word
    mov [rdi], dl
    jmp .unchecked_insert_copy
.unchecked_insert_word:
    mov [rdi], dx
    jmp .unchecked_insert_copy
.unchecked_insert_dword:
    mov [rdi], edx
    jmp .unchecked_insert_copy
.unchecked_insert_qword:
    mov [rdi], rdx
.unchecked_insert_copy:
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    sub rax, [rsp + 16]
    mov rcx, rax
    mul r15
    mov rcx, rax
    lea rsi, [r12 + SYBILANT_ARRAY_DATA_OFFSET]
    add rsi, [rsp + 8]
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    add rdi, [rsp + 8]
    add rdi, r15
    call sybilant_darray_Stransient_Dcopy
    mov rax, [rsp]
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Ddelete_B:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16
    mov r12, rdi
    mov r13, rsi
    mov [rsp + 8], r13
    call sybilant_darray_Stransient_Dguard
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    cmp r13, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    dec rax
    mov rsi, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dresize
    mov [rsp], rax
    mov r14, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov r15, [r14 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov rax, r13
    mul r15
    mov r13, rax
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    dec rax
    sub rax, [rsp + 8]
    mul r15
    mov rcx, rax
    lea rsi, [r12 + SYBILANT_ARRAY_DATA_OFFSET]
    mov rax, [rsp + 8]
    mul r15
    add rsi, rax
    add rsi, r15
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    mov rax, [rsp + 8]
    mul r15
    add rdi, rax
    call sybilant_darray_Stransient_Dcopy
    mov rax, [rsp]
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Ddelete_B_Dunchecked:
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16
    mov r12, rdi
    mov r13, rsi
    mov [rsp + 8], r13
    cmp r13, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    jae sybilant_darray_Stransient_Dout_Dof_Dbounds
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    dec rax
    mov rsi, rax
    mov rdi, r12
    call sybilant_darray_Stransient_Dresize
    mov [rsp], rax
    mov r14, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov r15, [r14 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov rax, [rsp + 8]
    mul r15
    mov r13, rax
    mov rax, [r12 + SYBILANT_ARRAY_LENGTH_OFFSET]
    dec rax
    sub rax, [rsp + 8]
    mul r15
    mov rcx, rax
    lea rsi, [r12 + SYBILANT_ARRAY_DATA_OFFSET]
    add rsi, r13
    add rsi, r15
    mov rdi, [rsp]
    lea rdi, [rdi + SYBILANT_ARRAY_DATA_OFFSET]
    add rdi, r13
    call sybilant_darray_Stransient_Dcopy
    mov rax, [rsp]
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    ret

;; Replace a transient type descriptor with a newly allocated immutable array
;; descriptor, then clear the editor.
sybilant_darray_Stransient_Dpersistent_B:
    push r12
    push r13
    mov r12, rdi
    call sybilant_darray_Stransient_Dguard
    mov rdi, r12
    call sybilant_darray_Stransient_Dcheck_Deditor
    jmp sybilant_darray_Stransient_Dpersistent_Dcommon

sybilant_darray_Stransient_Dpersistent_B_Dunchecked:
    push r12
    push r13
    mov r12, rdi

sybilant_darray_Stransient_Dpersistent_Dcommon:
    mov r13, [r12 + SYBILANT_ARRAY_TYPE_OFFSET]
    mov edi, SYBILANT_ARRAY_TYPE_SIZE + SYBILANT_TAG_MASK
    call sybilant_Smalloc_Dunchecked
    add rax, SYBILANT_TAG_MASK
    and rax, -8
    mov qword [rax + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    mov qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ARRAY_TYPE_CONSTRUCTOR
    mov rdx, [r13 + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET]
    mov [rax + SYBILANT_ARRAY_TYPE_ELEMENT_TYPE_OFFSET], rdx
    mov edx, [r13 + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET]
    mov [rax + SYBILANT_ARRAY_TYPE_ELEMENT_STRIDE_OFFSET], edx
    mov dword [rax + SYBILANT_ARRAY_TYPE_LAYOUT_FLAGS_OFFSET], 0
    mov [r12 + SYBILANT_ARRAY_TYPE_OFFSET], rax
    mov qword [r12 + SYBILANT_ARRAY_EDITOR_OFFSET], SYBILANT_NIL
    mov rax, r12
    pop r13
    pop r12
    ret

sybilant_darray_Stransient_Dinvalid_Dargument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

sybilant_darray_Stransient_Dinvalid_Dstate:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

sybilant_darray_Stransient_Dout_Dof_Dbounds:
    mov edi, SYBILANT_ERROR_OUT_OF_BOUNDS
    jmp sybilant_Sexit_Dunchecked

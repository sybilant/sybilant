bits 64
default rel

%include "lib/constants.asm"

section .text
global sybilant_datom_S_e
global sybilant_datom_Scompare_Dand_Dset
global sybilant_datom_Scompare_Dand_Dset_Dunchecked
global sybilant_datom_Sderef
global sybilant_datom_Sderef_Dunchecked
global sybilant_datom_Snew
global sybilant_datom_Snew_Dunchecked
extern sybilant_Sexit_Dunchecked
extern sybilant_Sinstance_q
extern sybilant_Smalloc_Dunchecked

;; Return whether two distinct atoms are equal. Atoms are mutable reference
;; cells with identity semantics, so distinct atoms are never equal. Identical
;; atoms return true through the identity check in sybilant/=.
;; Arguments: rdi = left (atom); rsi = right (atom). Return type: boolean.
sybilant_datom_S_e:
    mov eax, SYBILANT_FALSE
    ret

;; Create an atom with an element type and a matching initial value.
;; Arguments: rdi = element type (type); rsi = initial value (value).
;; Return type: atom.
sybilant_datom_Snew:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    mov rdi, rsi
    mov rsi, [rsp]
    call sybilant_Sinstance_q

    cmp rax, SYBILANT_TRUE
    jne sybilant_datom_Sinvalid_argument

    mov rdi, [rsp]
    mov rsi, [rsp + 8]
    add rsp, 24
    jmp sybilant_datom_Snew_Dunchecked

;; Create an atom with a proven element type and matching initial value.
;; Arguments: rdi = element type (type); rsi = initial value (value).
;; Return type: atom.
sybilant_datom_Snew_Dunchecked:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    mov edi, SYBILANT_ATOM_TYPE_SIZE + SYBILANT_ATOM_SIZE + SYBILANT_TAG_MASK
    call sybilant_Smalloc_Dunchecked

    mov rdx, [rsp]
    mov rcx, [rsp + 8]
    add rsp, 24

    add rax, SYBILANT_TAG_MASK
    and rax, -8
    mov qword [rax + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    mov qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR
    mov [rax + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], rdx

    lea rdx, [rax + SYBILANT_ATOM_TYPE_SIZE]
    mov [rdx + SYBILANT_ATOM_TYPE_OFFSET], rax
    mov [rdx + SYBILANT_ATOM_VALUE_OFFSET], rcx
    mov rax, rdx
    ret

;; Atomically replace an atom's identical old value with a new value.
;; Arguments: rdi = atom (value); rsi = old value (matching value);
;;            rdx = new value (matching value). Return type: boolean.
sybilant_datom_Scompare_Dand_Dset:
    sub rsp, 40
    mov [rsp], rdi
    mov [rsp + 8], rsi
    mov [rsp + 16], rdx

    call sybilant_datom_Sguard
    mov rax, [rdi + SYBILANT_ATOM_TYPE_OFFSET]
    mov rax, [rax + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET]
    mov [rsp + 24], rax

    mov rdi, [rsp + 8]
    mov rsi, rax
    call sybilant_Sinstance_q
    cmp rax, SYBILANT_TRUE
    jne sybilant_datom_Sinvalid_argument

    mov rdi, [rsp + 16]
    mov rsi, [rsp + 24]
    call sybilant_Sinstance_q
    cmp rax, SYBILANT_TRUE
    jne sybilant_datom_Sinvalid_argument

    mov rdi, [rsp]
    mov rsi, [rsp + 8]
    mov rdx, [rsp + 16]
    add rsp, 40
    jmp sybilant_datom_Scompare_Dand_Dset_Dunchecked

;; Atomically replace a proven atom's identical old value with a new value.
;; Arguments: rdi = atom (atom); rsi = old value (matching value);
;;            rdx = new value (matching value). Return type: boolean.
sybilant_datom_Scompare_Dand_Dset_Dunchecked:
    mov rax, rsi
    lock cmpxchg [rdi + SYBILANT_ATOM_VALUE_OFFSET], rdx
    jne .false

    mov eax, SYBILANT_TRUE
    ret

.false:
    mov eax, SYBILANT_FALSE
    ret

;; Return an atom's current value with acquire ordering.
;; Arguments: rdi = atom (value). Return type: value.
sybilant_datom_Sderef:
    sub rsp, 8
    call sybilant_datom_Sguard
    add rsp, 8
    jmp sybilant_datom_Sderef_Dunchecked

;; Return a proven atom's current value with acquire ordering.
;; Arguments: rdi = atom (atom). Return type: value.
sybilant_datom_Sderef_Dunchecked:
    mov rax, [rdi + SYBILANT_ATOM_VALUE_OFFSET]
    ret

;; Validate that a value is an atom.
;; Arguments: rdi = value (value). Return type: none.
sybilant_datom_Sguard:
    cmp rdi, SYBILANT_NIL
    je sybilant_datom_Sinvalid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_datom_Sinvalid_argument

    mov rax, [rdi + SYBILANT_ATOM_TYPE_OFFSET]
    cmp rax, SYBILANT_NIL
    je sybilant_datom_Sinvalid_argument

    test rax, SYBILANT_TAG_MASK
    jnz sybilant_datom_Sinvalid_argument

    cmp qword [rax + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE
    jne sybilant_datom_Sinvalid_argument

    cmp qword [rax + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR
    jne sybilant_datom_Sinvalid_argument
    ret

sybilant_datom_Sinvalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

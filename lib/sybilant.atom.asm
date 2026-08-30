bits 64
default rel

extern sybilant_alloc
extern sybilant_type_p

%include "lib/sybilant.constants.asm"

    ; Atom cell layout.
    VALUE_OFFSET equ 8
    SIZE equ 16

section .rodata
align 16
global sybilant_atom_type
sybilant_atom_type:
    dq SYBILANT_TYPE_TYPE, SYBILANT_ATOM_TYPE

section .text
;; Create an atom containing rdi. rax = atom.
global sybilant_atom_new
sybilant_atom_new:
    push rdi
    mov edi, SIZE
    call sybilant_alloc
    pop rdi
    mov qword [rax], SYBILANT_ATOM_TYPE
    mov [rax + VALUE_OFFSET], rdi
    ret

;; Return whether rdi is an atom. rax = SYBILANT_TRUE or SYBILANT_FALSE.
global sybilant_atom_p
sybilant_atom_p:
    mov esi, SYBILANT_ATOM_TYPE
    jmp sybilant_type_p

;; Atomically read an atom. rdi = atom; rax = current value.
global sybilant_atom_deref
sybilant_atom_deref:
    mov rax, [rdi + VALUE_OFFSET]
    ret

;; Replace an atom's value when it equals the expected value.
;; rdi = atom, rsi = expected value, rdx = new value;
;; rax = SYBILANT_TRUE on success, SYBILANT_FALSE otherwise.
global sybilant_atom_compare_and_set
sybilant_atom_compare_and_set:
    mov rax, rsi
    lock cmpxchg [rdi + VALUE_OFFSET], rdx
    mov eax, SYBILANT_FALSE
    jne .done
    mov eax, SYBILANT_TRUE
.done:
    ret

;; Local Variables:
;; mode: nasm
;; End:

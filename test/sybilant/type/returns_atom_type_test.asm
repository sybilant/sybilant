bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_datom_Snew
extern sybilant_Stype

testcase:
    push r12

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_datom_Snew
    mov rdi, rax
    call sybilant_Stype
    mov r12, rax

    ASSERT_EQ qword [r12 + SYBILANT_HEAP_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE, "sybilant/type should return a type value for an atom"
    ASSERT_EQ qword [r12 + SYBILANT_HEAP_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR, "sybilant/type should return an atom type"
    ASSERT_EQ qword [r12 + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_BOOLEAN_TYPE, "sybilant/type should preserve the atom element type"

    mov rdi, r12
    call sybilant_Stype
    ASSERT_EQ rax, SYBILANT_TYPE_TYPE, "an atom type should be an instance of type"

    pop r12
    ret

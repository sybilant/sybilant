bits 64
default rel

%include "test/support.asm"

section .text
extern sybilant_datom_Snew
extern sybilant_datom_Snew_Dunchecked
extern sybilant_Smalloc

testcase:
    push r12
    push r13
    push r14

    mov edi, 1
    call sybilant_Smalloc

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_TRUE
    call sybilant_datom_Snew
    mov r12, rax

    mov rdx, SYBILANT_MALLOC_START + 1
    ASSERT_ABE r12, rdx, "atom/new should return an allocated object"
    mov rdx, r12
    and edx, SYBILANT_TAG_MASK
    ASSERT_EQ edx, 0, "atom/new should align its object"
    mov r14, [r12 + SYBILANT_ATOM_TYPE_OFFSET]
    mov rdx, r14
    and edx, SYBILANT_TAG_MASK
    ASSERT_EQ edx, 0, "atom/new should align the atom type"
    ASSERT_EQ qword [r14 + SYBILANT_ATOM_TYPE_TYPE_OFFSET], SYBILANT_TYPE_TYPE, "an atom type should itself be a type"
    ASSERT_EQ qword [r14 + SYBILANT_ATOM_TYPE_CONSTRUCTOR_OFFSET], SYBILANT_ATOM_TYPE_CONSTRUCTOR, "an atom type should identify the atom constructor"
    ASSERT_EQ qword [r14 + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_BOOLEAN_TYPE, "an atom type should contain its element type"
    ASSERT_EQ qword [r12 + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_TRUE, "atom/new should store the initial value"

    mov edi, SYBILANT_BOOLEAN_TYPE
    mov esi, SYBILANT_FALSE
    call sybilant_datom_Snew_Dunchecked
    mov r13, rax

    lea r14, [r12 + SYBILANT_ATOM_SIZE]
    ASSERT_ABE r13, r14, "atom/new should allocate distinct atoms"
    mov r14, [r13 + SYBILANT_ATOM_TYPE_OFFSET]
    ASSERT_EQ qword [r14 + SYBILANT_ATOM_TYPE_ELEMENT_TYPE_OFFSET], SYBILANT_BOOLEAN_TYPE, "unchecked atom/new should put the proven element type in the atom type"
    ASSERT_EQ qword [r13 + SYBILANT_ATOM_VALUE_OFFSET], SYBILANT_FALSE, "unchecked atom/new should store the proven initial value"

    pop r14
    pop r13
    pop r12
    ret

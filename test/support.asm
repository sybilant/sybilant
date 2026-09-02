%ifndef SYBILANT_TEST_SUPPORT_ASM
%define SYBILANT_TEST_SUPPORT_ASM

%include "lib/constants.asm"

;; Compare two valid CMP operands. The third argument is a quoted diagnostic
;; string. A failed assertion exits the test with status 1.
%macro ASSERT_EQ 3
%ifnstr %3
%error "ASSERT_EQ diagnostic must be a quoted string"
%endif

section .rodata
    %%failure_message:
    db %3, 10
    %%failure_message_length equ $ - %%failure_message

section .text
    cmp %1, %2
    je %%passed

    mov eax, SYS_WRITE
    mov edi, STDERR_FILENO
    lea rsi, [rel %%failure_message]
    mov edx, %%failure_message_length
    syscall

    mov eax, SYS_EXIT
    mov edi, 1
    syscall
    ud2

    %%passed:
%endmacro

;; Compare two valid CMP operands as unsigned values. The third argument is a
;; quoted diagnostic string. A failed assertion exits the test with status 1.
%macro ASSERT_ABE 3
%ifnstr %3
%error "ASSERT_ABE diagnostic must be a quoted string"
%endif

section .rodata
    %%failure_message:
    db %3, 10
    %%failure_message_length equ $ - %%failure_message

section .text
    cmp %1, %2
    jae %%passed

    mov eax, SYS_WRITE
    mov edi, STDERR_FILENO
    lea rsi, [rel %%failure_message]
    mov edx, %%failure_message_length
    syscall

    mov eax, SYS_EXIT
    mov edi, 1
    syscall
    ud2

    %%passed:
%endmacro

;; Call a function in a child process and assert that it exits with the given
;; status. The third argument is a quoted diagnostic string. Returning from
;; the child function fails the assertion.
%macro ASSERT_EXIT 3
%ifnstr %3
%error "ASSERT_EXIT diagnostic must be a quoted string"
%endif

section .bss
align 4
    %%wait_status:
    resd 1

section .text
    mov dword [rel %%wait_status], -1
    mov eax, SYS_FORK
    syscall
    test eax, eax
    js %%asserted
    jz %%child

    mov edi, eax
    lea rsi, [rel %%wait_status]
    xor edx, edx
    xor r10d, r10d
    mov eax, SYS_WAIT4
    syscall

    %%asserted:
    ASSERT_EQ dword [rel %%wait_status], %2 << 8, %3
    jmp %%done

    %%child:
    sub rsp, 8
    call %1
    add rsp, 8
    ud2

    %%done:
%endmacro

section .text
global sybilant_Smain

;; Run the test case and return a successful process status.
;; Arguments: none. Return type: integer.
sybilant_Smain:
    sub rsp, 8
    call testcase
    add rsp, 8

    xor eax, eax
    ret

%endif

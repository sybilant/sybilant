bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_type_p
extern sybilant_tls_base

%include "lib/sybilant.constants.asm"

section .rodata
align 16
global sybilant_main_thread
sybilant_main_thread:
    dq SYBILANT_THREAD_TYPE, 0

section .tdata progbits alloc write tls
align 8
global sybilant_current_thread
sybilant_current_thread:
    dq sybilant_main_thread

section .text
;; Install the initial thread's TLS base. Returns only on success.
global sybilant_thread_initialize
sybilant_thread_initialize:
    mov eax, SYS_ARCH_PRCTL
    mov edi, ARCH_SET_FS
    lea rsi, [sybilant_tls_base]
    syscall
    test rax, rax
    js .failed
    ret
.failed:
    mov edi, SYBILANT_EXIT_CORRUPT_DATA
    jmp sybilant_exit

;; Return the current thread in rax.
global sybilant_thread_current
sybilant_thread_current:
    mov rax, [rel sybilant_current_thread wrt ..gottpoff]
    mov rax, [fs:rax]
    ret

;; Local Variables:
;; mode: nasm
;; End:

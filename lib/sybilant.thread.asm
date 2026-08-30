bits 64
default rel

extern sybilant_alloc
extern sybilant_exit
extern sybilant_type_p

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
;; Return the current thread in rax.
global sybilant_thread_current
sybilant_thread_current:
    mov rax, [rel sybilant_current_thread wrt ..gottpoff]
    mov rax, [fs:rax]
    ret

;; Local Variables:
;; mode: nasm
;; End:

bits 64
default rel

extern main
extern sybilant_thread_initialize

section .text
global _start
_start:
    call sybilant_thread_initialize
    jmp main

;; Local Variables:
;; mode: nasm
;; End:

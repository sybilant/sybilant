        section .text
        extern exit
        global _start
_start:
        mov rdi, 0
        jmp exit

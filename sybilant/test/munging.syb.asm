        extern exit
        global _start
_start:
        mov rdi, 0
        jmp ._u2603
        ._u2603:
        jmp exit
        global _u2603
_u2603:
        jmp _start

        bits 64
        default rel
        section .text
        extern exit
        global _u2603
_u2603:
        mov rdi, 0
        ret
        global foo
foo:
        call _u2603
        ret
        global _start
_start:
        mov rdi, 1
        call foo
        jmp ._u2603
        ._u2603:
        jmp exit

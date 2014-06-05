        extern exit
        extern PI
        global something
something:
        db -6
        global foo
foo:
        mov rax, qword [1]
        mov bl, byte [rdi+.bar]
        jmp .bar
        .bar:
        add rax, 1
        global bar
bar:
        db 1
        db 2
        global _start
_start:
        mov rdi, 6
        add rdi, byte -6
        jmp exit

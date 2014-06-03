extern exit
extern PI
global foo
foo:
mov rax, qword [1]
jmp .bar
.bar:
add rax, 1
global bar
bar:
db 1
db 2
global _start
_start:
mov rdi, 0
jmp exit

extern exit
global _start
_start:
mov rdi, 0
jmp .foo_Dbar
.foo_Dbar:
jmp exit

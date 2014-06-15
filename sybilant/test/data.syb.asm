        bits 64
        default rel
        section .data
        global data
data:
        db 0
        db 0
        global foo
foo:
        db -30, -104, -125, 0
        section .text
        extern exit
        global _start
_start:
        mov dil, byte [data]
        mov rcx, 0
        cmp byte [rcx+foo], byte -30
        je .next
        mov dil, 1
        jmp exit
        .next:
        inc rcx
        cmp byte [rcx+foo], byte -104
        je .next2
        mov dil, 1
        jmp exit
        .next2:
        inc rcx
        cmp byte [rcx+foo], byte -125
        je .next3
        mov dil, 1
        jmp exit
        .next3:
        inc rcx
        cmp byte [rcx+foo], byte 0
        je .next4
        mov dil, 1
        .next4:
        jmp exit

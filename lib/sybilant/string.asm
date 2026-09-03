bits 64
default rel

%include "lib/constants.asm"

section .text
global sybilant_dstring_Sget
global sybilant_dstring_Sget_Dunchecked
global sybilant_dstring_Slength
global sybilant_dstring_Slength_Dunchecked
extern sybilant_Sbox_Dcodepoint
extern sybilant_Sexit_Dunchecked

;; Return a string's indexed codepoint as a dynamic value.
;; Arguments: rdi = string (value); rsi = index (uint64). Return type: value.
sybilant_dstring_Sget:
    sub rsp, 24
    mov [rsp], rdi
    mov [rsp + 8], rsi

    call sybilant_dstring_Sguard

    mov rdi, [rsp]
    mov rsi, [rsp + 8]
    call sybilant_dstring_Sget_Dunchecked

    add rsp, 24
    mov edi, eax
    jmp sybilant_Sbox_Dcodepoint

;; Return a proven string's indexed codepoint without boxing it.
;; Arguments: rdi = string (string); rsi = index (uint64).
;; Return type: codepoint.
sybilant_dstring_Sget_Dunchecked:
    push r12
    push r13
    push r14

    mov r12, rsi
    xor r13d, r13d

    mov rsi, [rdi + SYBILANT_STRING_BYTE_LENGTH_OFFSET]
    add rdi, SYBILANT_STRING_DATA_OFFSET
    add rsi, rdi
    jc sybilant_dstring_Sinvalid_state

.next_codepoint:
    cmp rdi, rsi
    jae .finished

    call sybilant_dstring_Sdecode
    cmp r13, r12
    jne .advance

    mov r14d, eax

.advance:
    inc r13
    jmp .next_codepoint

.finished:
    cmp r12, r13
    jae sybilant_dstring_Sout_of_bounds

    mov eax, r14d
    pop r14
    pop r13
    pop r12
    ret

;; Return a string's number of codepoints.
;; Arguments: rdi = string (value). Return type: uint64.
sybilant_dstring_Slength:
    sub rsp, 8
    call sybilant_dstring_Sguard
    add rsp, 8
    jmp sybilant_dstring_Slength_Dunchecked

;; Return a proven string's number of codepoints.
;; Arguments: rdi = string (string). Return type: uint64.
sybilant_dstring_Slength_Dunchecked:
    push r12

    mov rsi, [rdi + SYBILANT_STRING_BYTE_LENGTH_OFFSET]
    add rdi, SYBILANT_STRING_DATA_OFFSET
    add rsi, rdi
    jc sybilant_dstring_Sinvalid_state

    xor r12d, r12d

.next_codepoint:
    cmp rdi, rsi
    jae .return

    call sybilant_dstring_Sdecode
    inc r12
    jmp .next_codepoint

.return:
    mov rax, r12
    pop r12
    ret

;; Validate that a value is a string.
;; Arguments: rdi = value (value). Return type: none.
sybilant_dstring_Sguard:
    cmp rdi, SYBILANT_NIL
    je sybilant_dstring_Sinvalid_argument

    test rdi, SYBILANT_TAG_MASK
    jnz sybilant_dstring_Sinvalid_argument

    cmp qword [rdi + SYBILANT_STRING_TYPE_OFFSET], SYBILANT_STRING_TYPE
    jne sybilant_dstring_Sinvalid_argument
    ret

;; Decode one strictly encoded UTF-8 codepoint within a proven byte range.
;; Arguments: rdi = current byte; rsi = one past the final byte.
;; Return type: eax = codepoint; rdi = byte after the codepoint.
sybilant_dstring_Sdecode:
    movzx eax, byte [rdi]
    inc rdi

    cmp eax, 0x80
    jb .return

    cmp eax, 0xc2
    jb sybilant_dstring_Sinvalid_state

    cmp eax, 0xe0
    jb .two_bytes

    cmp eax, 0xf0
    jb .three_bytes

    cmp eax, 0xf5
    jb .four_bytes

    jmp sybilant_dstring_Sinvalid_state

.two_bytes:
    cmp rdi, rsi
    jae sybilant_dstring_Sinvalid_state

    movzx edx, byte [rdi]
    inc rdi

    mov ecx, edx
    and ecx, 0xc0
    cmp ecx, 0x80
    jne sybilant_dstring_Sinvalid_state

    and eax, 0x1f
    shl eax, 6
    and edx, 0x3f
    or eax, edx
    ret

.three_bytes:
    mov rcx, rsi
    sub rcx, rdi
    cmp rcx, 2
    jb sybilant_dstring_Sinvalid_state

    movzx edx, byte [rdi]
    movzx ecx, byte [rdi + 1]
    add rdi, 2

    mov r8d, edx
    and r8d, 0xc0
    cmp r8d, 0x80
    jne sybilant_dstring_Sinvalid_state

    mov r8d, ecx
    and r8d, 0xc0
    cmp r8d, 0x80
    jne sybilant_dstring_Sinvalid_state

    cmp eax, 0xe0
    jne .three_not_overlong

    cmp edx, 0xa0
    jb sybilant_dstring_Sinvalid_state

.three_not_overlong:
    cmp eax, 0xed
    jne .decode_three_bytes

    cmp edx, 0xa0
    jae sybilant_dstring_Sinvalid_state

.decode_three_bytes:
    and eax, 0x0f
    shl eax, 12
    and edx, 0x3f
    shl edx, 6
    or eax, edx
    and ecx, 0x3f
    or eax, ecx
    ret

.four_bytes:
    mov rcx, rsi
    sub rcx, rdi
    cmp rcx, 3
    jb sybilant_dstring_Sinvalid_state

    movzx edx, byte [rdi]
    movzx ecx, byte [rdi + 1]
    movzx r8d, byte [rdi + 2]
    add rdi, 3

    mov r9d, edx
    and r9d, 0xc0
    cmp r9d, 0x80
    jne sybilant_dstring_Sinvalid_state

    mov r9d, ecx
    and r9d, 0xc0
    cmp r9d, 0x80
    jne sybilant_dstring_Sinvalid_state

    mov r9d, r8d
    and r9d, 0xc0
    cmp r9d, 0x80
    jne sybilant_dstring_Sinvalid_state

    cmp eax, 0xf0
    jne .four_not_overlong

    cmp edx, 0x90
    jb sybilant_dstring_Sinvalid_state

.four_not_overlong:
    cmp eax, 0xf4
    jne .decode_four_bytes

    cmp edx, 0x90
    jae sybilant_dstring_Sinvalid_state

.decode_four_bytes:
    and eax, 0x07
    shl eax, 18
    and edx, 0x3f
    shl edx, 12
    or eax, edx
    and ecx, 0x3f
    shl ecx, 6
    or eax, ecx
    and r8d, 0x3f
    or eax, r8d

.return:
    ret

sybilant_dstring_Sinvalid_argument:
    mov edi, SYBILANT_ERROR_INVALID_ARGUMENT
    jmp sybilant_Sexit_Dunchecked

sybilant_dstring_Sinvalid_state:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

sybilant_dstring_Sout_of_bounds:
    mov edi, SYBILANT_ERROR_OUT_OF_BOUNDS
    jmp sybilant_Sexit_Dunchecked

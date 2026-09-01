%ifndef SYBILANT_CONSTANTS_ASM
%define SYBILANT_CONSTANTS_ASM

;; Linux x86-64 system call numbers.
    SYS_WRITE equ 1
    SYS_MMAP  equ 9
    SYS_FORK  equ 57
    SYS_EXIT  equ 60
    SYS_WAIT4 equ 61

    STDERR_FILENO equ 2

;; Linux memory protection and mapping flags.
    PROT_READ  equ 0x1
    PROT_WRITE equ 0x2

    MAP_PRIVATE         equ 0x2
    MAP_ANONYMOUS       equ 0x20
    MAP_FIXED_NOREPLACE equ 0x100000

    PAGE_SIZE equ 4096

;; Sybilant memory layout.
    SYBILANT_MALLOC_START equ 0x100000000

;; Sybilant process exit statuses.
    SYBILANT_ERROR_INVALID_ARGUMENT equ 1
    SYBILANT_ERROR_OUT_OF_MEMORY    equ 2

%endif

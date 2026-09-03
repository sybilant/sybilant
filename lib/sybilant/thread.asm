bits 64
default rel

%include "lib/constants.asm"

;; Thread-local runtime state
;;
;; `sybilant.thread/self` is the stable threading interface. It returns a
;; Sybilant thread value without exposing who allocated the surrounding TLS
;; block or thread control block (TCB). The private self slot uses standard ELF
;; TLS so the accessor can remain the same across two runtime configurations.
;;
;; In the current freestanding configuration, no dynamic loader initializes
;; TLS. The runtime reserves an instance of its `.tbss` template followed by a
;; minimal TCB, sets FS to that TCB, and initializes the self slot. A future
;; native thread operation will allocate the same layout for each thread and
;; give its TCB address to `clone` with `CLONE_SETTLS`.
;;
;; In a future hosted configuration, the dynamic loader will allocate the TLS
;; block and TCB and pthread will do the same for threads it creates. Hosted
;; startup must preserve the loader's FS base and initialize only Sybilant's
;; slot. A pthread that calls into Sybilant will need to attach a Sybilant
;; thread value when it first finds that slot empty. The language-level thread
;; abstraction remains unchanged in both configurations.
;;
;; The initial-exec `GOTTPOFF` access assumes Sybilant is part of the process's
;; initial load set rather than a library loaded later with `dlopen`.

global sybilant_dthread_Sinitialize_Dmain
global sybilant_dthread_Sself
;; NASM requires global binding for `GOTTPOFF`; hidden visibility keeps the
;; backing slot out of the public dynamic interface.
global sybilant_dthread_dtls_Sself:data hidden SYBILANT_THREAD_SELF_TLS_SIZE
extern sybilant_Sexit_Dunchecked

section .tbss
align 8
;; Eight-byte self slot in the ELF TLS template.
sybilant_dthread_dtls_Sself:
    resb SYBILANT_THREAD_SELF_TLS_SIZE

section .data
align 8
;; Main thread value. Native initialization replaces the zero thread ID.
sybilant_dthread_Smain:
    dq SYBILANT_THREAD_TYPE
    dq 0
    dq sybilant_dthread_Smain + SYBILANT_THREAD_SIZE
    dq SYBILANT_STRING_TYPE
    dq SYBILANT_NIL
    dq 4
    db "main"

align 8
;; Aggregate backing storage for the main thread's TLS instance and TCB:
;;
;;     [main thread TLS instance] [minimal TCB]
;;     ^                           ^
;;     base                        FS base
sybilant_dthread_Smain_Dtls:
    dq sybilant_dthread_Smain
sybilant_dthread_Smain_Dtcb:
    dq sybilant_dthread_Smain_Dtcb

section .text

;; Return the current thread through the same interface in either runtime
;; configuration. The TLS relocation selects this thread's instance relative
;; to its current FS base.
;; Arguments: none. Return type: thread.
sybilant_dthread_Sself:
    mov rax, [rel sybilant_dthread_dtls_Sself wrt ..gottpoff]
    mov rax, [fs:rax]
    ret

;; Initialize the freestanding executable's static TLS and main thread value.
;; A future hosted initializer must use the loader's existing FS base instead.
;; Arguments: none. Return type: none.
sybilant_dthread_Sinitialize_Dmain:
    mov eax, SYS_GETTID
    syscall
    mov [rel sybilant_dthread_Smain + SYBILANT_THREAD_ID_OFFSET], rax

;; Static TLS uses negative offsets from the thread pointer. The first TCB word
;; at FS:0 points back to the TCB, following the x86-64 thread-pointer ABI.
    lea rsi, [rel sybilant_dthread_Smain_Dtcb]
    mov eax, SYS_ARCH_PRCTL
    mov edi, ARCH_SET_FS
    syscall
    test rax, rax
    jnz .invalid_state
    ret

.invalid_state:
    mov edi, SYBILANT_ERROR_INVALID_STATE
    jmp sybilant_Sexit_Dunchecked

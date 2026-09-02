bits 64
default rel

%include "test/support.asm"

section .text
global sybilant_S_e_Dunchecked

testcase:
    ret

;; Reserve the removed entry's symbol so the test fails if the runtime exports it.
sybilant_S_e_Dunchecked:
    ud2

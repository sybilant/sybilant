### Copyright © 2024 Paul Stadig
###
### This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
### of the MPL was not distributed with this file, You can obtain one at
### http://mozilla.org/MPL/2.0/.
###
### This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla
### Public License, v. 2.0.
        .text
        .global _start
_start:
        movl $1, %eax           # 1 ("exit") -> EAX
        movl $0, %ebx           # 0 (with success) -> EBX
        int $0x80               # invoke kernel's syscall

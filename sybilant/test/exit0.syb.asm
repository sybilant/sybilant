### Copyright © 2024 Paul Stadig
###
### This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0.  If a copy
### of the MPL was not distributed with this file, You can obtain one at
### http://mozilla.org/MPL/2.0/.
###
### This Source Code Form is "Incompatible With Secondary Licenses", as defined by the Mozilla
### Public License, v. 2.0.
        .text
        .extern exit

        .text
        .global _start
_start:
        mov $0, %rdi            # 0 (with success) -> RDI
        jmp exit                # call exit

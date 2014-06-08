;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
{'%rax {:type reg64-type :name 'a :width 64 :group "gen" :nr 0}
 '%eax {:type reg32-type :name 'a :width 32 :group "gen" :nr 0}
 '%ax {:type reg16-type :name 'a :width 16 :group "gen" :nr 0}
 '%al {:type reg8-type :name 'a :width 8 :group "gen" :nr 0}

 '%rcx {:type reg64-type :name 'c :width 64 :group "gen" :nr 1}
 '%ecx {:type reg32-type :name 'c :width 32 :group "gen" :nr 1}
 '%cx {:type reg16-type :name 'c :width 16 :group "gen" :nr 1}
 '%cl {:type reg8-type :name 'c :width 8 :group "gen" :nr 1}

 '%rdx {:type reg64-type :name 'd :width 64 :group "gen" :nr 2}
 '%edx {:type reg32-type :name 'd :width 32 :group "gen" :nr 2}
 '%dx {:type reg16-type :name 'd :width 16 :group "gen" :nr 2}
 '%dl {:type reg8-type :name 'd :width 8 :group "gen" :nr 2}

 '%rbx {:type reg64-type :name 'b :width 64 :group "gen" :nr 3}
 '%ebx {:type reg32-type :name 'b :width 32 :group "gen" :nr 3}
 '%bx {:type reg16-type :name 'b :width 16 :group "gen" :nr 3}
 '%bl {:type reg8-type :name 'b :width 8 :group "gen" :nr 3}

 '%rsp {:type reg64-type :name 'sp :width 64 :group "gen" :nr 4}
 '%esp {:type reg32-type :name 'sp :width 32 :group "gen" :nr 4}
 '%sp {:type reg16-type :name 'sp :width 16 :group "gen" :nr 4}
 '%spl {:type reg8-type :name 'sp :width 8}

 '%rbp {:type reg64-type :name 'bp :width 64}
 '%ebp {:type reg32-type :name 'bp :width 32 :group "gen" :nr 5}
 '%bp {:type reg16-type :name 'bp :width 16 :group "gen" :nr 5}
 '%bpl {:type reg8-type :name 'bp :width 8}

 '%rsi {:type reg64-type :name 'si :width 64 :group "gen" :nr 6}
 '%esi {:type reg32-type :name 'si :width 32 :group "gen" :nr 6}
 '%si {:type reg16-type :name 'si :width 16 :group "gen" :nr 6}
 '%sil {:type reg8-type :name 'si :width 8}

 '%rdi {:type reg64-type :name 'di :width 64 :group "gen" :nr 7}
 '%edi {:type reg32-type :name 'di :width 32 :group "gen" :nr 7}
 '%di {:type reg16-type :name 'di :width 16 :group "gen" :nr 7}
 '%dil {:type reg8-type :name 'di :width 8}

 '%r8 {:type reg64-type :name 'r8 :width 64}
 '%r8d {:type reg32-type :name 'r8 :width 32}
 '%r8w {:type reg16-type :name 'r8 :width 16}
 '%r8b {:type reg8-type :name 'r8 :width 8}

 '%r9 {:type reg64-type :name 'r9 :width 64}
 '%r9d {:type reg32-type :name 'r9 :width 32}
 '%r9w {:type reg16-type :name 'r9 :width 16}
 '%r9b {:type reg8-type :name 'r9 :width 8}

 '%r10 {:type reg64-type :name 'r10 :width 64}
 '%r10d {:type reg32-type :name 'r10 :width 32}
 '%r10w {:type reg16-type :name 'r10 :width 16}
 '%r10b {:type reg8-type :name 'r10 :width 8}

 '%r11 {:type reg64-type :name 'r11 :width 64}
 '%r11d {:type reg32-type :name 'r11 :width 32}
 '%r11w {:type reg16-type :name 'r11 :width 16}
 '%r11b {:type reg8-type :name 'r11 :width 8}

 '%r12 {:type reg64-type :name 'r12 :width 64}
 '%r12d {:type reg32-type :name 'r12 :width 32}
 '%r12w {:type reg16-type :name 'r12 :width 16}
 '%r12b {:type reg8-type :name 'r12 :width 8}

 '%r13 {:type reg64-type :name 'r13 :width 64}
 '%r13d {:type reg32-type :name 'r13 :width 32}
 '%r13w {:type reg16-type :name 'r13 :width 16}
 '%r13b {:type reg8-type :name 'r13 :width 8}

 '%r14 {:type reg64-type :name 'r14 :width 64}
 '%r14d {:type reg32-type :name 'r14 :width 32}
 '%r14w {:type reg16-type :name 'r14 :width 16}
 '%r14b {:type reg8-type :name 'r14 :width 8}

 '%r15 {:type reg64-type :name 'r15 :width 64}
 '%r15d {:type reg32-type :name 'r15 :width 32}
 '%r15w {:type reg16-type :name 'r15 :width 16}
 '%r15b {:type reg8-type :name 'r15 :width 8}}

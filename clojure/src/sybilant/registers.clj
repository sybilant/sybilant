;;;; Copyright © Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
{'%al {:type register-type :width 8 :name 'a}
 '%ax {:type register-type :width 16 :name 'a}
 '%eax {:type register-type :width 32 :name 'a}
 '%rax {:type register-type :width 64 :name 'a}

 '%bl {:type register-type :width 8 :name 'b}
 '%bx {:type register-type :width 16 :name 'b}
 '%ebx {:type register-type :width 32 :name 'b}
 '%rbx {:type register-type :width 64 :name 'b}

 '%cl {:type register-type :width 8 :name 'c}
 '%cx {:type register-type :width 16 :name 'c}
 '%ecx {:type register-type :width 32 :name 'c}
 '%rcx {:type register-type :width 64 :name 'c}

 '%dl {:type register-type :width 8 :name 'd}
 '%dx {:type register-type :width 16 :name 'd}
 '%edx {:type register-type :width 32 :name 'd}
 '%rdx {:type register-type :width 64 :name 'd}

 '%sil {:type register-type :width 8 :name 'si}
 '%si {:type register-type :width 16 :name 'si}
 '%esi {:type register-type :width 32 :name 'si}
 '%rsi {:type register-type :width 64 :name 'si}

 '%dil {:type register-type :width 8 :name 'di}
 '%di {:type register-type :width 16 :name 'di}
 '%edi {:type register-type :width 32 :name 'di}
 '%rdi {:type register-type :width 64 :name 'di}

 '%spl {:type register-type :width 8 :name 'sp}
 '%sp {:type register-type :width 16 :name 'sp}
 '%esp {:type register-type :width 32 :name 'sp}
 '%rsp {:type register-type :width 64 :name 'sp}

 '%bpl {:type register-type :width 8 :name 'bp}
 '%bp {:type register-type :width 16 :name 'bp}
 '%ebp {:type register-type :width 32 :name 'bp}
 '%rbp {:type register-type :width 64 :name 'bp}

 '%r8b {:type register-type :width 8 :name 'r8}
 '%r8w {:type register-type :width 16 :name 'r8}
 '%r8d {:type register-type :width 32 :name 'r8}
 '%r8 {:type register-type :width 64 :name 'r8}

 '%r9b {:type register-type :width 8 :name 'r9}
 '%r9w {:type register-type :width 16 :name 'r9}
 '%r9d {:type register-type :width 32 :name 'r9}
 '%r9 {:type register-type :width 64 :name 'r9}

 '%r10b {:type register-type :width 8 :name 'r10}
 '%r10w {:type register-type :width 16 :name 'r10}
 '%r10d {:type register-type :width 32 :name 'r10}
 '%r10 {:type register-type :width 64 :name 'r10}

 '%r11b {:type register-type :width 8 :name 'r11}
 '%r11w {:type register-type :width 16 :name 'r11}
 '%r11d {:type register-type :width 32 :name 'r11}
 '%r11 {:type register-type :width 64 :name 'r11}

 '%r12b {:type register-type :width 8 :name 'r12}
 '%r12w {:type register-type :width 16 :name 'r12}
 '%r12d {:type register-type :width 32 :name 'r12}
 '%r12 {:type register-type :width 64 :name 'r12}

 '%r13b {:type register-type :width 8 :name 'r13}
 '%r13w {:type register-type :width 16 :name 'r13}
 '%r13d {:type register-type :width 32 :name 'r13}
 '%r13 {:type register-type :width 64 :name 'r13}

 '%r14b {:type register-type :width 8 :name 'r14}
 '%r14w {:type register-type :width 16 :name 'r14}
 '%r14d {:type register-type :width 32 :name 'r14}
 '%r14 {:type register-type :width 64 :name 'r14}

 '%r15b {:type register-type :width 8 :name 'r15}
 '%r15w {:type register-type :width 16 :name 'r15}
 '%r15d {:type register-type :width 32 :name 'r15}
 '%r15 {:type register-type :width 64 :name 'r15}}

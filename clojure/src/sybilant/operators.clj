;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
{%adc
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %adc}
 %add
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %add}
 %and
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %and}
 %bsf
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %bsf}
 %bsr
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %bsr}
 %bswap
 ^{:schemata [[:r32]
              [:r64]]}
 {:type :operator :form %bswap}
 %bt
 ^{:schemata [[:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:rm16 :uint8]
              [:rm32 :uint8]
              [:rm64 :uint8]]}
 {:type :operator :form %bt}
 %btc
 ^{:schemata [[:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:rm16 :uint8]
              [:rm32 :uint8]
              [:rm64 :uint8]]}
 {:type :operator :form %btc}
 %btr
 ^{:schemata [[:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:rm16 :uint8]
              [:rm32 :uint8]
              [:rm64 :uint8]]}
 {:type :operator :form %btr}
 %bts
 ^{:schemata [[:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:rm16 :uint8]
              [:rm32 :uint8]
              [:rm64 :uint8]]}
 {:type :operator :form %bts}
 %call
 ^{:schemata [[:rel32]
              [:rm64]]
   :branch? true}
 {:type :operator :form %call}
 %cbw
 ^{:schemata [[]]}
 {:type :operator :form %cbw}
 %clc
 ^{:schemata [[]]}
 {:type :operator :form %clc}
 %cmc
 ^{:schemata [[]]}
 {:type :operator :form %cmc}
 %cdq
 ^{:schemata [[]]}
 {:type :operator :form %cdq}
 %cdqe
 ^{:schemata [[]]}
 {:type :operator :form %cdqe}
 %cmova
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmova}
 %cmovae
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovae}
 %cmovb
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovb}
 %cmovbe
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovbe}
 %cmovc
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovc}
 %cmove
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmove}
 %cmovg
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovg}
 %cmovge
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovge}
 %cmovl
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovl}
 %cmovle
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovle}
 %cmovna
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovna}
 %cmovnae
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnae}
 %cmovnb
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnb}
 %cmovnbe
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnbe}
 %cmovnc
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnc}
 %cmovne
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovne}
 %cmovng
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovng}
 %cmovnge
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnge}
 %cmovnl
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnl}
 %cmovnle
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnle}
 %cmovno
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovno}
 %cmovnp
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnp}
 %cmovns
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovns}
 %cmovnz
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovnz}
 %cmovo
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovo}
 %cmovp
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovp}
 %cmovpe
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovpe}
 %cmovpo
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovpo}
 %cmovs
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovs}
 %cmovz
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmovz}
 %cmp
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %cmp}
 %cmpxchg
 ^{:schemata [[:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]]}
 {:type :operator :form %cmpxchg}
 %cqo
 ^{:schemata [[]]}
 {:type :operator :form %cqo}
 %crc32
 ^{:schemata [[:r32 :rm8]
              [:r32 :rm16]
              [:r32 :rm32]
              [:r64 :rm8]
              [:r64 :rm64]]}
 {:type :operator :form %crc32}
 %cwd
 ^{:schemata [[]]}
 {:type :operator :form %cwd}
 %cwde
 ^{:schemata [[]]}
 {:type :operator :form %cwde}
 %dec
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %dec}
 %div
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %div}
 %idiv
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %idiv}
 %imul
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]
              [:r16 :rm16 :int8]
              [:r32 :rm32 :int8]
              [:r64 :rm64 :int8]
              [:r16 :rm16 :int16]
              [:r32 :rm32 :int32]
              [:r64 :rm64 :int32]]}
 {:type :operator :form %imul}
 %inc
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %inc}
 %ja
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %ja}
 %jae
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jae}
 %jb
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jb}
 %jbe
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jbe}
 %jc
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jc}
 %je
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %je}
 %jecxz
 ^{:schemata [[:rel8]]
   :branch? true}
 {:type :operator :form %jecxz}
 %jg
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jg}
 %jge
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jge}
 %jl
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jl}
 %jle
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jle}
 %jmp
 ^{:schemata [[:rel8]
              [:rel32]
              [:rm64]]
   :branch? true}
 {:type :operator :form %jmp}
 %jna
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jna}
 %jnae
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnae}
 %jnb
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnb}
 %jnbe
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnbe}
 %jnc
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnc}
 %jne
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jne}
 %jng
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jng}
 %jnge
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnge}
 %jnl
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnl}
 %jnle
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnle}
 %jno
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jno}
 %jnp
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnp}
 %jns
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jns}
 %jnz
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jnz}
 %jo
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jo}
 %jp
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jp}
 %jpe
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jpe}
 %jpo
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jpo}
 %jrcxz
 ^{:schemata [[:rel8]]
   :branch? true}
 {:type :operator :form %jrcxz}
 %js
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %js}
 %jz
 ^{:schemata [[:rel8]
              [:rel32]]
   :branch? true}
 {:type :operator :form %jz}
 %loop
 ^{:schemata [[:rel8]]
   :branch? true}
 {:type :operator :form %loop}
 %loope
 ^{:schemata [[:rel8]]
   :branch? true}
 {:type :operator :form %loope}
 %loopne
 ^{:schemata [[:rel8]]
   :branch? true}
 {:type :operator :form %loopne}
 %mov
 ^{:schemata [[:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]
              [:r8 :imm8]
              [:r16 :imm16]
              [:r32 :imm32]
              [:r64 :imm64]
              [:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]]}
 {:type :operator :form %mov}
 %movbe
 ^{:schemata [[:r16 :m16]
              [:r32 :m32]
              [:r64 :m64]
              [:m16 :r16]
              [:m32 :r32]
              [:m64 :r64]]}
 {:type :operator :form %movbe}
 %movsx
 ^{:schemata [[:r16 :rm8]
              [:r32 :rm8]
              [:r64 :rm8]
              [:r32 :rm16]
              [:r64 :rm16]]}
 {:type :operator :form %movsx}
 %movsxd
 ^{:schemata [[:r64 :rm32]]}
 {:type :operator :form %movsxd}
 %movzx
 ^{:schemata [[:r16 :rm8]
              [:r32 :rm8]
              [:r64 :rm8]
              [:r32 :rm16]
              [:r64 :rm16]]}
 {:type :operator :form %movzx}
 %mul
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %mul}
 %neg
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %neg}
 %nop
 ^{:schemata [[]
              [:rm16]
              [:rm32]]}
 {:type :operator :form %nop}
 %not
 ^{:schemata [[:rm8]
              [:rm16]
              [:rm32]
              [:rm64]]}
 {:type :operator :form %not}
 %or
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %or}
 %pop
 ^{:schemata [[:rm16]
              [:rm64]
              [:r16]
              [:r64]]}
 {:type :operator :form %pop}
 %popcnt
 ^{:schemata [[:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %popcnt}
 %push
 ^{:schemata [[:rm16]
              [:rm64]
              [:r16]
              [:r64]
              [:imm8]
              [:imm16]
              [:imm32]]}
 {:type :operator :form %push}
 %rcl
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %rcl}
 %rcr
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %rcr}
 %ret
 ^{:schemata [[]
              [:imm16]]}
 {:type :operator :form %ret}
 %rol
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %rol}
 %ror
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %ror}
 %sal
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %sal}
 %sar
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %sar}
 %shl
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %shl}
 %shr
 ^{:schemata [[:rm8 :cl]
              [:rm8 :imm8]
              [:rm16 :cl]
              [:rm16 :imm8]
              [:rm32 :cl]
              [:rm32 :imm8]
              [:rm64 :cl]
              [:rm64 :imm8]]}
 {:type :operator :form %shr}
 %sbb
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %sbb}
 %seta
 ^{:schemata [[:rm8]]}
 {:type :operator :form %seta}
 %setae
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setae}
 %setb
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setb}
 %setbe
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setbe}
 %setc
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setc}
 %sete
 ^{:schemata [[:rm8]]}
 {:type :operator :form %sete}
 %setg
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setg}
 %setge
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setge}
 %setl
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setl}
 %setle
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setle}
 %setna
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setna}
 %setnae
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnae}
 %setnb
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnb}
 %setnbe
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnbe}
 %setnc
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnc}
 %setne
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setne}
 %setng
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setng}
 %setnge
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnge}
 %setnl
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnl}
 %setnle
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnle}
 %setno
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setno}
 %setnp
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnp}
 %setns
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setns}
 %setnz
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setnz}
 %seto
 ^{:schemata [[:rm8]]}
 {:type :operator :form %seto}
 %setp
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setp}
 %setpe
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setpe}
 %setpo
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setpo}
 %sets
 ^{:schemata [[:rm8]]}
 {:type :operator :form %sets}
 %setz
 ^{:schemata [[:rm8]]}
 {:type :operator :form %setz}
 %shld
 ^{:schemata [[:rm16 :r16 :imm8]
              [:rm16 :r16 :cl]
              [:rm32 :r32 :imm8]
              [:rm32 :r32 :cl]
              [:rm64 :r64 :imm8]
              [:rm64 :r64 :cl]]}
 {:type :operator :form %shld}
 %shrd
 ^{:schemata [[:rm16 :r16 :imm8]
              [:rm16 :r16 :cl]
              [:rm32 :r32 :imm8]
              [:rm32 :r32 :cl]
              [:rm64 :r64 :imm8]
              [:rm64 :r64 :cl]]}
 {:type :operator :form %shrd}
 %stc
 ^{:schemata [[]]}
 {:type :operator :form %stc}
 %sub
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %sub}
 %test
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]]}
 {:type :operator :form %test}
 %xadd
 ^{:schemata [[:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]]}
 {:type :operator :form %xadd}
 %xchg
 ^{:schemata [[:rm8 :r8]
              [:r8 :rm8]
              [:rm16 :r16]
              [:r16 :rm16]
              [:rm32 :r32]
              [:r32 :rm32]
              [:rm64 :r64]
              [:r64 :rm64]]}
 {:type :operator :form %xchg}
 %xor
 ^{:schemata [[:rm8 :imm8]
              [:rm16 :imm16]
              [:rm32 :imm32]
              [:rm64 :int32]
              [:rm16 :int8]
              [:rm32 :int8]
              [:rm64 :int8]
              [:rm8 :r8]
              [:rm16 :r16]
              [:rm32 :r32]
              [:rm64 :r64]
              [:r8 :rm8]
              [:r16 :rm16]
              [:r32 :rm32]
              [:r64 :rm64]]}
 {:type :operator :form %xor}}

#import "style.typ": *

#show: design-document.with(
  title: [Sybilant],
  subtitle: [Design and implementation],
)

#outline(title: [Contents], depth: 3, indent: auto)
#pagebreak()

= Introduction

Lambda calculus and a Turing machine describe computation equally well. Each
model is universal, and neither exceeds the other in power. Yet the two rarely
meet in one language. A language usually adopts one view and treats the other as
foreign territory.

A top-down language starts from lambda calculus and descends toward the machine.
It rests on a layer that hides the hardware behind more palatable abstractions.
The layer buys portability, but blunts performance and creates a barrier the
language's own abstractions cannot reach past.

A bottom-up language starts from the machine and builds upward. It exposes the
hardware directly, yet it tangles the programmer in mutation, loops, and manual
memory management. High-level structure becomes hard to express and harder to
trust.

"Lambda: The Ultimate GOTO" @lambda-goto narrowed the gap between these two
views. It shows a function call is really a jump. A function and a machine
instruction both express the same more fundamental act, which draws lambda
calculus and the Turing machine toward a single account of computation.

Sybilant follows that convergence to its conclusion and unifies the two forms of
computation into one continuum. Machine-level code and high-level code inhabit
the same language giving programmers the freedom to write elegant, dense programs
or precise, performant programs, as needed.

#bibliography("references.bib", title: [References])

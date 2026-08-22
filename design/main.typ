#import "style.typ": *

#show: design-document.with(
  title: [Sybilant],
  subtitle: [Design and implementation],
)

#outline(title: [Contents], depth: 3, indent: auto)
#pagebreak()

= Introduction

Language design tends to run in one of two directions. A top-down design starts
from mathematical abstractions and works toward a concrete machine. A bottom-up
design starts from the machine and works toward high-level code. Most languages
favor one ideal to the detriment of the other.

A top-down language usually bottoms out at a compatibility or abstraction layer.
Below that layer the machine, with its efficiency and its performance, is out of
reach. A bottom-up language usually favors mutability, `for` loops, and manual
memory management. Each of these matters at some level, but none combines cleanly
with immutable, functional programming.

Sybilant runs in both directions. It reaches from the lambda calculus down to the
machine and keeps the machine's performance within reach the whole way. The
Scheme tradition already showed that high-level abstraction can keep the machine
close: a jump is equivalent to a function
call.#footnote[#cite(<lambda-goto>, form: "prose") treats a procedure call and a
`GOTO` as the same mechanism: a call is a `GOTO` that also passes arguments.]
Typed assembly language showed the converse, that a type discipline can survive
all the way down to registers and
instructions.#footnote[#cite(<tal>, form: "prose") keep a type discipline intact
down to registers and instructions, where a compatibility layer would otherwise
erase it.]

A second divide separates static from dynamic typing, and again a language usually
picks one. Static typing gives the programmer performance and strong guarantees
in return for the effort of satisfying the type checker. Dynamic typing spares
that effort but costs memory fetches and runtime guards, and it always will,
despite real and substantial progress.

Static typing has its own cost. A static type system cannot express some correct
programs, and in the end everything erases to numbers and to the instructions
that manipulate them. Within a single codebase, some code needs
performance and strong guarantees, and some code is not worth the effort of
proving.

Sybilant treats static and dynamic checking as points on one continuum rather
than as rival disciplines. It checks statically what it can and defers the rest
to runtime, so the programmer can prove what matters and leave the rest to
runtime checks.#footnote[#cite(<sage>, form: "prose") verify specifications
statically where they can and insert runtime checks where they cannot.]

These are just a couple examples where Sybilant blends ideas that language
design usually assumes must stay separate. None of those ideas is
novel. Sybilant draws on the work that came before it: the lambda calculus and
the Scheme reports for the top-down reach, typed assembly language for the
bottom-up discipline, and hybrid checking for the meeting of static and dynamic
types. Its contribution is the synthesis.

#bibliography("references.bib", title: [Bibliography], full: true, style: "chicago-author-date")

# Project status

Sybilant is under active design. `DESIGN.pdf` states the current intended design.
This file tracks the project's current focus, direction, open questions, and
pending design work. It is a working snapshot, not a historical record, and does
not define language behavior.

## Current focus

Fleshing out the body of the design document from an initial design brain dump.
The dump is organized below into provisional decisions and open questions. None
of it is in `main.typ` yet; it needs confirmation first. The document should read
as approachable and semi-formal for a working programmer with a few years of
experience, with a later chapter or appendix carrying the formal rules (grammar,
typing, coercion).

## Design direction (from the initial brain dump)

Provisional. Captured from a first pass; not yet confirmed or written into the
design. Items marked "open" need a decision before drafting.

### Guiding principles

- Static and dynamic are not separate worlds; Sybilant blends them along a
  continuum. Differences show up as representation choices and coercions inserted
  where needed, not as a hard boundary. (The numeric model is a worked example:
  promotion is per-operation, and static interval information just removes dynamic
  work.)
- Prefer static checking. Any type expected at run time should be expressible and
  checkable at compile time where possible, falling back to runtime checks
  otherwise. But do not avoid a feature just because it cannot be statically
  checked.
- Nothing purely erases. Every construct has both a compile-time effect and a
  run-time effect, so definitions stay importable and inspectable at run time.
- This extends to separately compiled modules: anything the compiler computes
  goes into the module, so another module can load and use it.
- Immutability by default, as low as the assembly level (see arrays and vectors).
- Allow assembly, but do not encourage it (see syntax).
- Small, composable core with a rich, batteries-included library. Keep the
  primitives few and build breadth in the library (transducers over sequences,
  protocols over dispatch, macros over syntax). Scheme's simplicity is a model,
  but its lack of batteries is a weakness to avoid.
- Checked by request, not by default, at every level. The high-level language is
  dynamically typed by default and statically checked when annotated, mirroring
  `%deftext`'s unchecked-default and checked-on-request. One gradient, two levels.

### REPL semantic (incremental top-level compilation)

- The compilation unit is a single top-level form, not a file.
- Even when compiling ahead of time into a binary module, a file is read like a
  REPL transcript: top-level forms are evaluated one at a time, so each form's
  effect takes place and the next form can observe it.
- This is foundational and shapes much else: it explains why nothing erases (a
  form's effect must be observable by later forms), constrains forward references
  (a later form is not yet visible), and drives the redefinition questions in the
  special forms section. Likely a DDR candidate.

### Macros

- Sybilant will have macros.
- Macros expand even at the assembly level, inside `%deftext` blocks.
- Follows from the REPL semantic and the Lisp lineage.
- Macros should be hygienic. Clojure's approach works; Scheme's pattern-matching
  and template approach (`syntax-rules`) is appealing.
- Possibly hygiene and templating are not a separate feature but an application of
  the general pattern-matching feature (see pattern matching).

### Syntax and surface conventions

- Prefix assembly-level symbols with `%` so they cannot shadow instructions
  (for example, block definitions use `%deftext`).
- Allow assembly, but do not encourage it. Making the assembly surface
  deliberately unattractive is one lever to steer people toward the higher-level
  language.
- Kebab-case, as a Lisp.
- Predicates that return true/false (or truthy/falsey) end with `?`.
- Names can be namespaced and can include Unicode characters. **Open:** the
  namespaced-symbol syntax, dot-separated (`com.example.Foo`) or Clojure-like with
  a slash separating namespace from name (`com.example/Foo`).
- **Open:** trailing `!`. Either side-effecting functions (aligns with effect
  tracking below) or the Ruby sense that the function may throw. Pick one; leaning
  side-effecting, with "may throw" carried by the type/effect system instead.
- Name munging: not a problem for Sybilant's own separate compilation (munge
  freely, or use the custom object format). For import/export and FFI, munging has
  to be toggled on or off, explicitly or implicitly (possibly another role for
  `%defextern`).

### Integers and the integer type model

- At the machine level everything is integers, so assembly instructions need
  typing rules and integers need a solid type model.
- Three kinds, with these names (confirmed): `uint` (**unsigned**, plain), `sint`
  (**signed**, two's complement), `int` (**signless**: the overlapping range
  shared by `uint` and `sint` of the same width; a way to hold an integer without
  committing to a representation). Widths append to the name, for example
  `uint64`, `sint64`, `int64`.
- An integer type has a **kind**, a **width**, and a **min** and **max**. The
  min and max must lie within the kind-and-width range. (This min/max interval is
  effectively refinement typing on integers.)
- Literals: bases 2, 10, 16 with `0b` and `0x` prefixes; sign optional, positive
  if absent. A non-negative literal's type is `int`, the smallest width that
  contains the value, and min = max = the value.
- Negative literals are the exception: a negative literal is typed as `sint` at
  the smallest width whose range contains the value.
- Coercion (`type1` coerces to `type2`) when either: `type1` is `int` and its
  [min, max] is contained in `type2`'s; or same kind, `type1` width <= `type2`
  width, and `type1` range contained in `type2` range.
- Width promotion: sign-extend for `sint`; zero-extend for `uint` and `int`.
- **Overflow promotion is a property of the operation, not of static vs. dynamic
  code.** This follows Clojure's checked/unchecked math: the programmer chooses
  the operation (promoting, wrapping, or throwing). Expect both promoting and
  non-promoting operations:
  - Non-promoting operations stay fixed-width (wrap or trap). They need a concrete
    width, for example `sint64`, so a value not already at that width is coerced
    (checked) to it first.
  - Promoting operations preserve mathematical value and are
    representation-polymorphic: they accept boxed or unboxed operands and yield a
    boxed **big integer** when the result exceeds native width. This is why
    promotion is not a static/dynamic separation; a promoting op handles an
    unboxed `sint64` and a boxed big integer alike.
  - Static interval information sharpens promoting ops rather than excluding them:
    if the `[min, max]` intervals prove the result fits native width, the compiler
    emits the unboxed path and skips the overflow-check-and-box; otherwise it
    emits the check. Same operation, static info removing dynamic work.
- Corollary: there is no separate "static integer world," but a family of
  representations. Here "boxed" means dynamically typed and self-describing, not
  necessarily heap-allocated:
  - **unboxed machine integer**: a raw machine word of a given kind and width
    (`uint`/`sint`/`int` at 8/16/32/64), statically typed only, no runtime tag.
  - **fixnum**: a boxed *immediate* (62-bit, tagged), living in a register with no
    memory dereference needed to read its tag. The efficient general-purpose
    dynamic integer; it makes dynamic-mode math faster than fetching and storing
    tagged values in memory.
  - **boxed fixed-width integer**: a dynamically typed value that carries a
    specific kind and width (`uint`/`sint` at 8/16/32/64) and wraps or throws at
    that width. Needed when the precision must stay fixed but the value is used in
    a dynamic context (also matters for FFI, where exact widths are required). This
    is distinct from a fixnum: a boxed `uint8` is not a fixnum even when its value
    would fit, because its type and overflow behavior differ.
  - **big integer**: a boxed *heap* value, arbitrary precision.
  - Operations, not a type-world boundary, decide which is produced.
- Reifying an unboxed machine integer into dynamic code boxes it. Which boxed form
  depends on the operation and precision needed, not on fitting the fixnum range:
  - Promoting context: go straight to a **big integer**.
  - Non-promoting dynamic context: box to the matching **fixed-width integer** (for
    example a boxed 64-bit), because a 62-bit fixnum has the wrong precision and an
    arbitrary-precision big integer would not wrap or throw at the fixed width.
- Fixnum promotion goes straight to a big integer. A non-promoting fixnum operation
  wraps or throws at 62 bits.
- Representation implication (open): a boxed 8/16/32-bit fixed-width integer may
  pack into the 56-bit extended tag (immediate, no heap); a boxed 64-bit integer
  cannot (64 bits do not fit an immediate) and needs the heap. Confirm against the
  tag layout.
- Cost to weigh: this family multiplies the boxed representations, so dynamic
  operations must dispatch across fixnum, the boxed fixed-width integers (each kind
  and width), and big integer. Justified by fixed precision in dynamic code and by
  FFI, and consistent with "support everything," but keep the fixnum the fast path
  and the boxed fixed-width integers the less common one.

### Assembly modularity and target architectures

- Sybilant should support multiple assembly architectures; the assembly layer
  should be modular so adding an instruction set is as simple as possible. The
  real design work is the ISA abstraction, not any one target.
- **Open (scope):** start with x86-64, arm64, or both. Doing both early forces
  architecture-neutral decisions at ~2x the instruction-typing surface. Middle
  path: design the abstraction against both, fully specify one as a worked
  instance, and keep the abstraction from becoming secretly x86-shaped.

### Trust modes for code blocks

Assembly blocks are defined with `%deftext`. Three modes:

- **checked:** the compiler verifies the block body against its declared type.
- **trusted:** the block declares a type; the compiler trusts it, checks nothing,
  and allows anything in the body.
- **unchecked:** the block declares no type; the compiler checks nothing.

### Special forms (def-forms)

Beyond `%deftext` (assembly code blocks; see trust modes), the current set:

- **`%defdata`**: a label and a literal value. Takes a type or infers it from
  the data. Produces a chunk of data dropped into the assembled file verbatim and
  loaded into memory as-is; the label evaluates to the data's address.
  - **Open:** how to specify data, especially large amounts (perhaps by
    importing), and whether to have special syntax for combining integer literals
    into one chunk.
  - **Open:** what happens when data references other labels. Code and data must
    be relocatable. Two dual options: build a symbol table / relocations that the
    linker patches; or disallow symbol references in data and require an
    initializer that copies a label's address into the data at startup (an
    initializer is a hand-rolled relocation).
- **`%defconst`**: a label and a literal value inlined into code. Unlike a pure
  constant, it also produces a runtime value so other modules can import it; it
  does not simply erase (see guiding principles).
- **`%defextern`** (kept, for FFI): references an external definition; may take a
  type, takes no body. Its distinct role is foreign symbols that may use a
  different calling convention or ABI, which a bodyless def-form has nowhere to
  record.
  - Division of labor: bodyless `%deftext` and `%defdata` (no body or data) are
    declarations for separately compiled Sybilant code or forward references,
    linked or defined later; `%defextern` is for FFI.
  - In checked blocks, a jump target must be a `%deftext` (or a local `%label`)
    and data operations are allowed only on a `%defdata`. Likely enforce the
    jump-target rule even in unchecked code.
  - Forward references at the assembly level may need indirection, or a size
    declaration so memory can be reserved.
- A declaration's type is its interface, which link-time verification checks
  (connects to the typed object format and the TAL/TML thread).

Declaration types and redefinition (needs exploration):

- A forward declaration likely must carry a type. Then its definition must satisfy
  that type, because other code may have compiled against it.
- **Open:** what happens if the type must change. Can a definition be undefined,
  or is it immutable once declared? Options under consideration:
  - Compile-against-current: once the system bootstraps indirection, code compiles
    against wherever a `%deftext` currently points; a redefinition leaves the old
    definition in place, and new code compiles against the new one. Changing a
    definition then requires recompiling code that references it.
  - Reference-cell indirection (Clojure var sense): code compiles against a
    reference type that is always dereferenced at run time. The type still cannot
    change, but old code need not be recompiled to see a new definition.
- Both keep the type fixed; they differ on whether existing code must be
  recompiled. Tied to the REPL semantic.
- Key insight that dissolves the overhead worry: if a var's type is immutable
  (a redefinition may swap the value but never the type), then reference-cell
  indirection needs **no runtime type check**. The callsite just dereferences the
  var and calls; the "type has not changed" invariant is enforced at definition
  time, not per call. So the real choice is direct-linking (fastest call, but
  redefinition recompiles callers) vs. var indirection (one pointer load, picks up
  new definitions free) without a static-typing penalty either way. Allowing the
  type itself to change is what would force a runtime check or recompilation.
- Invoking a `%deftext` is not through a var. The same tradeoff applies via a code
  pointer: a direct jump (fast, recompile to redefine) or a jump through a
  redefinable code-pointer slot (like a GOT/PLT entry). Could unify vars and text
  blocks under one indirection-slot mechanism. Still open.

### Static and dynamic checking (a continuum, not a boundary)

- The type system is compiler-checkable, but because Sybilant also checks
  dynamically, all types must be representable in memory at runtime.
- Where a statically typed value meets code that expects a checked value (or the
  reverse), the compiler reifies the value's type and inserts a guard that checks
  it at runtime. These coercions appear only where the representations or the
  known information differ; they are points along the continuum, not a wall.
- Coercions are concretely box/unbox operations. Going from static to dynamic, the
  compiler inserts boxing. Going from dynamic to static, it inserts unboxing, which
  may throw at run time if the value does not fit the static type.
- The split is not clean even within one function: a statically typed function may
  take a `sint64` and return a big integer. Argument and result representations
  are independent, so "static" and "dynamic" do not partition functions.
- Sage-style hybrid checking. The integer intervals and promoting-operation
  optimization above are the first worked instances: static information removes
  runtime work, and the runtime check remains only where the static side cannot
  prove it away.

### Nominal and structural typing (unification problem)

- Tension: the high-level language is nominally typed, while the low-level TAL is
  structurally typed. The blending principle says nominal types should be usable
  in assembly too, which means preserving them through separate compilation (no
  erasure).
- Proposed direction: a **nominal type is a structural type plus a brand** (a
  stable identity). The structural component is the layout that TAL and machine
  operations use; the brand is the nominal identity the high level distinguishes
  by. A purely structural type is simply a brand-less type.
  - Operations that care only about layout work structurally; type identity checks
    compare brands, so two nominal types with the same layout stay distinct.
  - No erasure: the brand travels with the type in the object format, so a
    `%deftext` can name a nominal type and the linker can verify brand identity
    across modules.
  - Brands need globally stable, unique identifiers (namespaced names, possibly
    content hashes); ties to the module and namespace system.
- Likely DDR-worthy once the direction firms up.

### Type system scope and soundness

- Hard constraint: keep the type system sound. Avoid a "Frankenstein" of bolted
  together features that lose soundness.
- **Exploratory idea:** integrate Rust-style borrow checking into TAL. A possible
  novel contribution. Weigh it against the soundness constraint before committing;
  it is not yet a decision.
- Motivation: TAL gives memory safety up to a point (the layout of records and
  tuples), but memory safety is more than that. Borrow checking could cover the
  rest, such as temporal safety and controlled aliasing and mutation.
- Tension to resolve: borrow checking is a static discipline, but Sybilant has
  dynamically typed values crossing the reification/guard boundary. What happens
  to a borrow when a value crosses into dynamic code and back is where soundness
  is won or lost, and likely where any novel contribution lies.
- Prior art to calibrate novelty: substructural and region reasoning already
  exist at the machine-typed level (Alias Types, L3, the capability calculus,
  and Cyclone's regions). The new angle is Rust-style borrow checking combined
  with TAL and the static/dynamic boundary.

### Arrays, vectors, and immutability

- Terminology (confirmed): a **vector** is the high-level, immutable, persistent,
  bounds-safe sequence (RRB or Clojure-style) and the default; an **array** is
  the low-level, contiguous, possibly mutable primitive that vectors and buffers
  build on.
- Lean into immutability broadly, as low as the assembly level, in keeping with
  the functional side of the vision.
- Most arrays should be immutable. Updating an immutable array is copy-on-write.
- High-level immutable sequences (vectors) use persistent structures: Clojure
  style persistent vectors, or relaxed radix balanced (RRB) trees.
- Mutable arrays still exist where necessary at the lowest levels.
- Large data (images, big buffers) makes copy-on-write too expensive, so it needs
  in-place mutation. This is a natural fit for borrow-checked exclusive access:
  unique ownership permits safe mutation without copying. Ties the array story to
  the borrow-checking thread.
- **Open:** how immutability is expressed and enforced at the assembly level.
- **Open:** how copy-on-write sharing is tracked (reference counting vs.
  ownership/borrows).
- **Open:** the boundary between low-level mutable arrays and high-level
  persistent vectors.

Bounds checking:

- Want bounds checking on array access: static where possible, dynamic certainly.
- **Open:** whether to adopt dependent typing to express static bounds checks, or
  to rely on the refinement-style integer intervals (min/max) already planned. An
  index whose max is provably below the array length is safe statically; where it
  is not provable, a runtime guard checks it. This is the same hybrid mechanism
  as elsewhere and may avoid full dependent typing. The reach of the static side
  depends on whether the array's length is tracked in its type.

### Effect tracking (pure vs. impure)

- Idea: track side effects in the type system so the compiler knows which
  functions are pure. Exact use not yet pinned down.
- Assessment (worth doing, but keep it coarse): a lightweight purity attribute
  (pure vs. may-effect), inferred where possible, pays for itself. It enables
  optimization (reordering, memoization, dead-code removal) and, importantly,
  sound in-place update: a function that uniquely owns a value and is otherwise
  pure can mutate it in place and still present a pure, immutable interface. That
  ties effects to the immutability, copy-on-write, and borrow-checking threads.
- Push-back / scope: avoid committing to a full effect system (effect
  polymorphism over higher-order functions, algebraic effects) up front; that is
  where the complexity explodes. Start coarse and grow only if needed.
- Interactions: I/O is the canonical effect (see the I/O gap). Dynamic or
  unchecked code has unknown effects and is assumed impure. Reference
  neighborhood: Koka, the Haskell IO monad.
- **Open:** whether effects are part of the reified function type (riding along
  with types, so they do not erase) or static-only reasoning (which would erase,
  an exception to "nothing erases"). Since types are already reified at run time,
  making effects part of the type keeps the principle intact.

### nil, null, and initialization

- Big open question. Open to an option/maybe type instead of a pervasive null.
- **Open:** does an option/maybe type work with dynamic typing? Investigate why it
  would or would not.
- Uninitialized values are the usual reason to want null. Alternative: use linear
  types or borrow checking (Rust-style) to guarantee initialization statically,
  and fall back to a runtime check where static proof is not available (consistent
  with the prefer-static principle).

### Value representation (implementation-leaning)

- 64-bit targets. 8-byte alignment leaves the low three bits of a word free to
  work with.
- The low two bits are the type selector: `00` = fixnum, `11` = pointer,
  `01` = extended tag, `10` = reserved.
- The third low bit is the fixnum's own least significant bit (parity), so
  fixnums are 62-bit.
- Extended tag uses the low byte to tag values that pack into 56 bits: candidates
  include ratios, characters, short tuples, keywords, decimals, maybe complex
  numbers.

### Numbers beyond integers

- Support binary floating point, but the **default** float type is **decimal**
  floating point.
- Err toward supporting everything, so Sybilant puts nothing out of reach,
  especially for FFI compatibility. Support both **decimal32** (packed via the
  extended tag, for efficiency and to save space and memory access) and
  **decimal64** (as a static type and a boxed type). IEEE also allows custom
  exponent/mantissa widths.
- Decimal literals default to **decimal64**; a decimal32 comes from a cast or a
  literal tag.
- No automatic promotion between decimal types: the IEEE standard saturates to
  infinity (the same as binary floating point). A decimal32 can be coerced to
  decimal64 explicitly, but nothing happens automatically.
- The integer tower still tops out in a boxed **big integer** above the unboxed
  integers (see the integer section). Binary floating point has no big form; it
  saturates to IEEE infinity.
- **Ratio** type in the number tower. Likely two forms: an immediate packed ratio
  (a numerator/denominator split within the extended tag, for example 28 bits
  each, or 36/24) and a big-integer-based ratio for larger values.

### Characters and strings

- Characters correspond to Unicode code points.
- **Open:** string encoding. UTF-32 simplifies things but bloats memory, and
  combining characters mean it still doesn't give true random access by grapheme;
  UTF-8 is likely the better default.

### High-level language (Clojure and Scheme guided)

- Guides: Clojure (data, concurrency, expressiveness) and Scheme (simplicity). The
  tension between "keep it simple" and "jam everything in" is acknowledged; aim for
  a small, composable core even while the feature set is broad.
- **Functions and closures:** named function definitions and function literals; a
  function literal evaluates to a closure that captures its lexical environment.
- **Arguments:** variadic and keyword arguments. Keyword arguments follow Clojure:
  a function that accepts either a map or alternating keys and values.
- **Application:** apply to some positional values plus a trailing collection (the
  Clojure/Scheme `apply`); likewise supply keyword arguments from a map.
- **Multiple arities** (Clojure-style), related to variadics. Confirm.
- **Destructuring** in bindings and parameters (Clojure-style), related to pattern
  matching.
- **Data types:** rich built-ins, maps, sets, vectors, tables (see literal data
  types).
- **Polymorphism:** static polymorphism (parametric/generics) and runtime
  polymorphism. Runtime dispatch is probably type-based, but a more generic
  dispatch (Clojure multimethods) would be nice. Implies a protocol/interface
  mechanism for how types provide behavior.
- **Pattern matching:** wanted; integration open. Likely underpins destructuring,
  sum-type case analysis, and possibly macro templates (Scheme `syntax-rules`).
- **Sequence library:** the full functional set (`map`, `filter`, `reduce`, ...).
- **Transducers:** a core part of the runtime library (Clojure transducers). Also
  the answer to laziness (see evaluation strategy below).
- **Transients:** local, unobservable mutation for performance (Clojure
  transients); ties to uniqueness, borrow checking, effects, and copy-on-write.
- **Tail calls:** optimized. Provide `loop`/`recur`, where `recur` both lets the
  compiler participate in the optimization and is checked, so a non-tail call in a
  position that expects a tail call is an error rather than silent stack growth.
- **Equality and hashing:** structural equality and hashing, needed for hash maps.
  A protocol, extensible to new types.
- **Typing surface:** dynamic by default, static when requested (parallel to
  `%deftext`'s unchecked-default, checked-on-request; see guiding principles).
  **Open:** annotation syntax and how much inference.
- **Evaluation strategy:** eager, not lazy. Transducers give the composability
  people reach to laziness for (lazy to construct a pipeline, eager to process the
  data), which also sidesteps the laziness/no-GC/effects interactions.
- **Reader, homoiconicity, eval:** yes to all. `eval` is tricky: with no
  interpreter, `eval` means compile-then-execute at run time (as Clojure does), so
  the compiler is part of the runtime. For `%deftext` this means compiling the
  assembly block and executing it. Implication to weigh: the runtime carries the
  compiler (affects bootstrapping and image size).

### Literal data types and data-oriented programming

- Carry Clojure's expressive data literals and data-oriented style: write out data
  directly to interpret or process, rather than writing code that constructs it.
- Support literal maps and sets (and the existing vectors). Lean toward Clojure
  syntax, but open to alternatives.
- Want a two-dimensional table literal format, usable both for table-like syntax
  (for example a `let`) and for expressing two-dimensional data.
- **Open:** the concrete literal syntax for maps, sets, and the table format.

### Error handling

- At the assembly level, an error just means terminating.
- Anything more interesting (exceptions, conditions, result types) gets
  bootstrapped on top at higher levels. This defines the base case for the
  throwing operations already assumed (unbox, checked math, bounds, guards).
- **Open:** the higher-level error-handling mechanism, which also decides the `!`
  naming convention. Leaning toward a condition system (appealing but less
  familiar), with exceptions as the well-understood fallback. Needs research and
  exploration.

### Concurrency

- Lean on Clojure's model for the most part.
- Reference types with known, expected semantics (the Clojure reference-type
  family). Connects to the redefinition reference-cell idea.

### Blending assembly and functional programming

- Allow a `%deftext` block to define code invocable via function application.
- Allow function application inside a `%deftext` block; the compiler expands it
  into the instructions to invoke the function per its calling convention,
  handling register saving and stack adjustment so surrounding instructions'
  values survive.
- **Open:** should `%deftext` allow a custom calling convention? Should there be
  an active calling convention set by the environment?

### Memory management

- Reclamation is **tiered**, and tier 0 is "don't worry about it": use a bump
  allocator for `malloc` and let the process free everything at once on exit,
  like an arena. This banks on many programs being short-lived. Let the virtual
  memory manager page out unused data in the meantime.
- Later tiers add real reclamation: a lazy collector might unmap all-garbage
  pages or do something fancier (for example, the Mesh collector).
- Design thread, not settled. Note the constraint: paging out does not reclaim
  memory (address space and backing store grow until something unmaps), and
  deciding a page is all-garbage needs liveness information, so the higher tiers
  still face the hard part of GC. Tier 0 sidesteps this by not reclaiming at all.

### Separate compilation and linking

- Support separate compilation and linking with a custom object format that
  carries type information alongside assembly, perhaps in a custom ELF section.
- Compile to a binary format that a custom linker later loads and type-checks
  before executing. (TAL and TML are directly relevant.)

## Not yet addressed

Areas the brain dump has not covered. Together these amount to one theme: the
high-level, top-down half of the language is still a sketch, while the bottom-up
half is specified in depth. The intro promises both directions, so this is the
main thing to fill in.

- **The type language.** How types are written: function types, record types, the
  refinement integers, union types, a top/dynamic type, and whether there is
  subtyping. Central and only implied so far.
- **Closure and function specifics.** Direction is set (see the high-level
  language section), but open details remain: how closures allocate under the
  memory model, and multiple return values.
- **Sum types.** Records and tuples appear in passing (layout, borrow checking,
  tags), and maps/sets/vectors have literal direction, but there is no sum-type
  design. Sum types (option, result) are needed by the type system itself and by
  the nil/option decision.
- **Module and namespace system.** Names can be namespaced and modules compile
  separately, but import, export, visibility, and namespace resolution are
  unspecified.
- **I/O and networking.** No model yet for interacting with the outside world:
  syscalls, FFI to libc, or a built-in I/O layer. The canonical effect, so it
  ties to effect tracking, and to `%defextern`/FFI.
- **Bootstrapping / implementation language.** The redefinition notes assume a
  bootstrap story ("once the system can bootstrap indirection"); the compiler's
  own implementation path is unstated.

High-level concepts still to flesh out (answering "what else"), roughly by
priority. Several from the first pass are now resolved in direction (typing
surface, tail calls, equality/hashing, laziness, reader/eval, rationals) and moved
into their sections.

- **The type language and its surface syntax.** How types are written: function
  types, record types, the refinement integers, union types, a top/dynamic type,
  subtyping, and how annotations appear (see also the type-language gap above).
- **Protocols/interfaces and user-defined types.** The substrate for runtime
  polymorphism and for declaring records/structs (a `defrecord`/`deftype`/protocol
  analog) and how they interoperate with the static type system. Structural
  equality/hashing is the first protocol.
- **Binding, scope, and control flow.** `let`/`letrec`, recursion, shadowing;
  conditionals (`if`/`cond`/`case`); truthiness (likely nil and false are falsey).
- **Pattern matching.** How it integrates with destructuring, sum-type case
  analysis, and possibly macro templates.
- **Concurrency specifics.** Which reference types to include (atoms, refs/STM,
  agents, channels) under the Clojure model.
- **Error/condition system.** The higher-level mechanism above the assembly-level
  terminate (also under error handling and the `!` convention).
- **Reflection and metadata.** Reflection: yes (reified types make it natural).
  Metadata: appealing but, as in Clojure, mostly useful for syntactic purposes;
  scope to decide.

## Tensions to resolve

- **Tier-0 memory vs. long-running programs.** Tier 0 (bump allocate, free on
  exit) assumes short-lived programs, but networking and servers are long-lived
  and allocate continuously (persistent vectors, boxed big integers, closures,
  copy-on-write). If long-running programs are a target, tier 0 is insufficient
  and real reclamation becomes urgent. Needs a stance on whether servers are in
  scope.
- **Immutability by default vs. mutation-heavy I/O buffers.** Manageable through
  borrow-checked unique ownership, but I/O buffers are exactly the large mutable
  case and should be designed together with the array/vector story.
- **REPL semantic vs. ahead-of-time separate compilation.** The REPL semantic
  evaluates top-level forms in order, so each form's effect is observable by the
  next. Separate compilation emits a module loaded later, out of that order, and
  link-time verified. If a form has a compile-time effect that a later form
  depends on, the compiled module must carry and replay that effect in load order.
  "Put everything the compiler computes into the module" gestures at this but does
  not resolve it. (The effect-erasure question is now tracked under effect
  tracking.)

## Open design questions

Decisions needed before or during drafting:

1. Fix the exact inventory of promoting vs. non-promoting integer operations
   (Clojure-style checked/unchecked). (The fixnum-vs-64-bit representation boundary
   and the decimal model are now resolved above.)
2. String encoding: UTF-8 vs. UTF-32.
3. Calling-convention model for `%deftext`: custom per block, environment-set, or
   both.
4. Target architecture scope: x86-64, arm64, or both first.
5. Memory reclamation beyond tier 0: how higher tiers determine liveness if pages
   are to be unmapped.
6. Borrow checking in TAL: whether to pursue it, and how to keep the combined
   system sound, especially across the static/dynamic boundary (exploratory).
7. Bounds checking on arrays: dependent typing vs. refinement-interval integers
   for the static portion.
8. Array immutability mechanics: expressing immutability at the assembly level,
   tracking copy-on-write sharing, the low-level-mutable vs. persistent-vector
   boundary, and safe in-place mutation of large buffers.
9. Data definition (`%defdata`): specifying large data (import vs. inline
   syntax), and handling label references in data (relocations/symbol table vs.
   startup initializers) while keeping code and data relocatable.
10. Redefinition under the REPL semantic: whether a typed declaration is immutable
    once declared or can be undefined; and if redefinable, compile-against-current
    (recompile referencing code) vs. reference-cell indirection (no recompile).
    The type stays fixed either way.
11. nil/null: adopt an option/maybe type (and confirm it works with dynamic
    typing), and how to guarantee initialization (linear/borrow types statically,
    runtime check otherwise).
12. Higher-level error handling built on the assembly-level terminate base:
    exceptions, conditions, or result types. Also settles the `!` convention.
13. Namespaced-symbol syntax: dot-separated vs. Clojure-like slash.
14. Literal syntax for maps, sets, and the two-dimensional table format.
15. Nominal/structural unification: confirm the brand-over-structural approach and
    how brands are made stable and unique across separate compilation.

## Design tasks

- Confirm the organized brain dump above, then move confirmed, observable
  decisions into `main.typ` and keep implementation notes clearly marked.
- Decide the document structure. Proposed dependency-ordered spine: (1) a short
  concrete example program for orientation; (2) values and representation
  (integers with refinement intervals, the tagged word, the number tower,
  characters and strings); (3) `%deftext` and the three trust modes; (4) blending
  assembly and function application, with calling conventions; (5) the type
  system and the static/dynamic boundary; (6) memory; (7) separate compilation
  and linking; (8) an appendix with the formal grammar, typing, and coercion
  rules.
- Triage DDR candidates (create only with approval). Strongest candidates:
  the REPL semantic (incremental top-level compilation); the static/dynamic hybrid
  model (checked/trusted/unchecked plus reification/guards); the nominal/structural
  unification (brand over structural type); the tiered memory strategy; the typed
  object format with link-time verification; and, if pursued, borrow checking in
  TAL. The integer model, tagged representation, and decimal representation are
  real decisions but can be presented directly in the document unless promoted.

Remove questions and tasks when the project resolves or completes them rather
than retaining them as history. Incorporate adopted results into the design
document. Use DDRs only under the policy in `AGENTS.md`.

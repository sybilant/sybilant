# Project status

## Current focus

None.

## Open tasks

4. Reclaim allocation mappings -- depends on: 2, 3
   Allow a future garbage collector to release physical memory and
   virtual memory manager metadata without retreating the frontier or
   making an allocation address reusable.

5. Validate sparse allocation costs -- depends on: 2, 4
   Measure address-space consumption and virtual memory manager metadata
   for representative long-running workloads, then adjust the mapping
   and reclamation policy if needed.

## Completed

### Main thread identity

`sybilant.thread/self` is a zero-argument function that returns the current
thread value from a hidden eight-byte variable in `.tbss`. Before calling
`sybilant/main`, `_start` uses statically allocated backing storage for a
thread-local storage block followed by a minimal thread control block, stores
its thread pointer in FS, and initializes the variable to a thread value
containing its unboxed Linux thread ID and immutable string name `main`. The
accessor keeps the FS-relative storage representation out of the public runtime
interface. A future dynamic loader can preserve that interface while taking
over thread-pointer initialization and extending the private per-thread state
with dynamic binding data.

Integrate into: manual.

### String value equality

`sybilant/=` compares two strings by byte length and then by bytes. Strict
UTF-8 has one encoding for each codepoint sequence, so proven strings are equal
exactly when their bytes match. The comparison trusts the encoding and doesn't
revalidate it. Equality doesn't normalize strings, so canonically equivalent
composed and decomposed sequences remain unequal.

Integrate into: manual.

### Immutable strings

Immutable strings have a built-in type, a null editor, a byte length, and
UTF-8 data. They don't cache their codepoint count or an indexing structure.
`sybilant.string/length` validates and counts the encoded codepoints in linear
time. String indexing scans from the start and checks the codepoint index
against the encoded data. Guarded operations validate the string type, while
unchecked operations skip only that type guard. Both retain bounds checks and
strict UTF-8 validation. Invalid encoding reports
`SYBILANT_ERROR_INVALID_STATE`. Dynamic gets return boxed codepoints, while
unchecked gets return decoded codepoints in `eax`.

Integrate into: manual.

### Boxed codepoint values

The `codepoint` type represents Unicode code points from U+0000 through
U+10FFFF as 21-bit values. Dynamic codepoints use a distinct extended
immediate tag and have no separate packed representation. Boxing masks the
payload to 21 bits. Checked unboxing rejects other tags, unchecked unboxing
extracts the payload directly, and `sybilant/type` recognizes boxed
codepoints.

Integrate into: manual.

### Packed array layout metadata

Array type descriptors store the element stride as a 32-bit byte count and
reserve the remaining 32 bits of the aligned layout slot for future flags.
Guarded array operations reject nonzero reserved flags. Unchecked gets read
only the stride field, so reserved metadata doesn't affect element addressing.

Integrate into: manual.

### Array element strides

Parameterized array types cache their element stride in bytes. Unchecked gets
use that stride to select 1-, 2-, 4-, or 8-byte indexing and loads without
inspecting the element type. Array type equality includes the stride, and
array value equality compares only the register width defined by that stride.
Unsupported strides report `SYBILANT_ERROR_INVALID_STATE` until wider value
return conventions exist.

Integrate into: manual.

### Array bounds and native-width returns

Guarded and unchecked array gets report `SYBILANT_ERROR_OUT_OF_BOUNDS` for an
index outside the array. Unchecked 1- and 2-byte gets write only `al` and `ax`,
respectively, preserving the rest of `rax`. Four-byte gets use `eax` directly.

Integrate into: manual.

### Immutable typed arrays

Immutable `Array<T>` values reference first-class element types and contain a
null editor, an unboxed length, and packed element data. Guarded length and get
operations validate the array type. Dynamic get operations box integer values,
while unchecked gets return unboxed 1-, 2-, 4-, or 8-byte elements. Both get
paths check index bounds. Immutable arrays and their types compare
structurally through `sybilant/=`.

Integrate into: manual.

### Typed atomic reference cells

Atom values point to first-class `Atom<T>` heap type descriptors and contain
only their current values. Atom types compare structurally, and an atom type
can use another atom type as its element type. Guarded allocation and
compare-and-set operations enforce the element type. Unchecked entries trust
static proof. Compare-and-set uses identity and an atomic update. Dereference
acquires values published through compare-and-set. Atoms compare by identity,
so `sybilant/=` treats distinct atoms as unequal even when their current
values match.

Integrate into: manual.

### Slash-delimited runtime procedure names

Every runtime procedure name separates its module namespace from its operation
with `/`. All module namespaces start with `sybilant`. Dots identify nested
modules, as in `sybilant.atom/new` for `lib/sybilant/atom.asm`. Munged symbols
encode `/` as `_S`, `.` as `_d`, and `-` as `_D`. Nonprocedure runtime state
keeps its existing names.

Integrate into: manual.

### Unchecked integer unboxing

Every integer unboxing function has an `-unchecked` counterpart that
extracts and extends the payload without validating its tag, type
header, alignment, or pointer value. Checked unboxing validates its
input and then uses the same unchecked extraction path.

Integrate into: manual.

### Integer types and boxing

Unsigned, signed, and natural integers have 8-, 16-, 32-, and 64-bit
types. The narrower types box their raw payloads into distinct extended
immediates. The 64-bit types box into aligned heap objects with a type
header and payload. Unboxing validates the dynamic type, zero-extends
unsigned and natural values, and sign-extends signed values. Distinct
boxed 64-bit integers of the same type compare by payload through
`sybilant/=`.

Integrate into: manual.

### Value equality

`sybilant/=` checks identity first: identical values, immediate or heap, are
equal without further validation, even when the heap type has no equality
support or an invalid header. Otherwise, an immediate on either side settles
the comparison as unequal without inspecting the other, heap, side. For two
heap values, it validates their types, compares them recursively with
`sybilant/=`, and dispatches to a type-specific equality implementation.
Because that recursive check proves the operand types, dispatch calls the
unchecked equality entries directly. Parameterized type descriptors share a
heap type header whose first slot names the type and whose second slot holds a
constructor tag. Dispatch routes strings on their type tag to
`sybilant.string/=-unchecked` and reads that constructor tag to route array
values to `sybilant.array/=-unchecked`. Integer equality stays inline, and
type-descriptor equality is `sybilant/type=` in the root module.

Each type-specific equality is a guarded entry that validates its arguments
before calling an unchecked counterpart: `sybilant.string/=` validates two
strings, `sybilant.array/=` validates two arrays and rejects unequal element
types or strides, and `sybilant.atom/=` validates two atoms. The unchecked
array comparison reads the shared stride once and runs a loop for that width.
Equality defaults to identity, so distinct values of a heap type without a
defined structural equality compare unequal rather than reporting an error.
Atoms compare by identity, so `sybilant/=` needs no atom dispatch and lets
them fall through to that identity default. `sybilant/=` is the single general
entry point for value equality and has no unchecked variant.
`sybilant/instance?` uses it to compare type values.

Integrate into: manual.

### Guarded runtime entry points

Public runtime symbols guard values that arrive from dynamic code. Matching
`-unchecked` symbols skip type guards when the compiler proves the argument
and return types. Semantic precondition and operational failure checks remain
in both paths.

Integrate into: manual.

### Tagged immediate value introspection

The low three bits distinguish aligned pointers, false, true, and extended
immediates. Type values use the type extended tag. `sybilant/type` delegates
type computation to its unchecked entry and validates only the resulting type
or nil value. It recognizes booleans, type values, integers, and aligned
pointer values, with nil as a special case. `sybilant/instance?` compares a
value's type with a validated type argument, and `sybilant/boolean?`
specializes that predicate for booleans. Invalid object headers exit with
`SYBILANT_ERROR_INVALID_STATE`, and invalid type arguments exit with
`SYBILANT_ERROR_INVALID_ARGUMENT`.

Integrate into: manual.

### Allocator state initialization

Before calling `sybilant/main`, `_start` sets `sybilant-malloc-start`
and `sybilant-malloc-maximum` to `SYBILANT_MALLOC_START`.

Integrate into: manual.

### Root runtime entry and exit

`_start` calls `sybilant/main`, then passes its return status to the
Linux exit system call through `sybilant/exit`.

Integrate into: manual.

### Frontier allocation

`sybilant/malloc` returns storage from a byte-granular frontier that
only moves upward. It maps newly crossed pages at fixed addresses
without replacing existing mappings. A zero byte count exits with
`SYBILANT_ERROR_INVALID_ARGUMENT`, and allocation failure exits with
`SYBILANT_ERROR_OUT_OF_MEMORY`.

Integrate into: manual.

### Runtime symbol munging

Munged runtime names contain only the letters `A` through `Z` and `a`
through `z`, digits, and underscores. Those letters and digits remain
unchanged. The compiler uses these short, case-sensitive escapes for
common punctuation:

| Character | Escape | Mnemonic |
| --- | --- | --- |
| `!` | `_B` | bang |
| `#` | `_h` | hash |
| `$` | `_R` | dollar |
| `%` | `_P` | percent |
| `&` | `_a` | ampersand |
| `*` | `_s` | star |
| `'` | `_Q` | quote |
| `<` | `_l` | less |
| `.` | `_d` | dot |
| `>` | `_g` | greater |
| `/` | `_S` | slash |
| `?` | `_q` | question |
| `=` | `_e` | equals |
| `+` | `_p` | plus |
| `\|` | `_V` | vertical bar |
| `:` | `_c` | colon |
| `-` | `_D` | dash |
| `_` | `__` | underscore |

The compiler encodes every other Unicode scalar value in the Basic
Multilingual Plane as `_uXXXX` and every other scalar value as
`_UXXXXXX`. The hexadecimal digits are uppercase and zero-padded.
Surrogate code points aren't valid scalar values. Symbol munging does
not perform Unicode normalization.

Integrate into: manual.

<!--
List completed-but-not-integrated design work here, with a one-line
summary and where it belongs:

```
- Task title: the result. Integrate into: DDR | manual.
```

Move unusual or novel design decisions into a Design Decision Record
first. Remove completed work from here after its design lands in the
manual, and in a Design Decision Record when the decision warrants one.
-->

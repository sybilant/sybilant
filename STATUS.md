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

### Slash-delimited runtime procedure names

Every runtime procedure uses the `sybilant/` namespace. Munged symbols encode
the namespace separator as `_S`, while hyphens within procedure names remain
`_D`. Nonprocedure runtime state keeps its existing names.

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

`sybilant/=` compares immediate values directly. For two heap values, it
validates their types and compares them recursively with `sybilant/=` before
using a fixed type-specific equality dispatch. Heap types without equality
support report `SYBILANT_ERROR_INVALID_STATE`. Value equality has one runtime
entry and implementation. `sybilant/instance?` uses it to compare type values.

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

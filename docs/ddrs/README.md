# Design decision records

## Naming

Each settled decision gets its own Design Decision Record (DDR), named
`NNNN-title.md` with a zero-padded, four-digit, sequential number:
`0001-title.md`, `0002-title.md`, and so on, following the structure in
`0000-template.md`. DDRs aren't included in the manual PDF. They're a
standalone record, referenced from `docs/sybilant-manual.typ` by prose.

## Style

A DDR should focus on conceptual decisions, not concrete names, syntax,
or implementation details, except where necessary. Concrete details like
these can appear as examples or to show feasibility, without binding the
implementation to them. When explaining concepts, lead with positive and
negative examples, then the more technical and formal details. Write for
an experienced programmer -- skip explaining fundamentals.

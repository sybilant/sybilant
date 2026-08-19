# Design decision records

DDRs preserve the rationale for selected major Sybilant language and runtime
decisions. They also record alternatives and consequences. The design document
states each adopted decision. The project does not use DDRs for tooling or
project process.

DDRs are exceptional records, not a log of every design choice. Reserve them for
major language or runtime decisions and major departures from an established
design. Create one only when the project owner explicitly requests it or approves
a proposal to create it.

## Naming

DDR files use `NNNN-short-title.md`, where `NNNN` is the next unused four-digit
number. `0000-template.md` is a template and does not describe a decision.

## Status

- `Proposed` means the project is still considering the options.
- `Accepted` means the project has adopted the decision into the design.
- `Rejected` means the project considered but did not adopt the proposal.
- `Superseded` means a later DDR replaces the decision.

An accepted DDR is a historical record. Correct only small errors that do not
alter its decision or rationale. With project-owner approval, use a new DDR to
change the decision and mark the old record as superseded.

## Process

1. Confirm that the project owner requested or approved the DDR.
2. Copy `0000-template.md` to the next numbered filename.
3. Describe the problem, requirements, context, and constraints.
4. State the criteria for evaluating every option.
5. Describe the advantages and disadvantages of the chosen option.
6. Describe the advantages and disadvantages of each rejected option.
7. Explain why the chosen option best satisfies the criteria.
8. Record consequences and identify related or superseded decisions.
9. When accepting a DDR, update the design document with the adopted result.

The design document may cite a record as `DDR-NNNN`.

# Sybilant

Sybilant is a programming language. This repository holds its design and
implementation notes, design decision records, and a reference corpus of related
languages and systems.

The design document is semiformal rather than an implementation-independent
standard. It combines prose, diagrams, mathematical notation, typing judgments,
and implementation details for the expected single implementation.

## Project status

The project designs and implements Sybilant iteratively. `DESIGN.pdf` states the
current intended language even when the implementation is incomplete or lags
behind it. An iteration may deliberately begin with a smaller design and revise
it later.

[STATUS.md](STATUS.md) tracks current design work, open questions, and pending
tasks.

With project-owner approval, the project records selected major language and
runtime choices under `ddrs/`. A DDR compares the considered options and
preserves the rationale for the adopted design. The project does not use DDRs for
tooling or project process. When an accepted decision changes, a new accepted DDR
supersedes the old record, and the design document states the replacement
decision.

## Toolchain

The build uses [Typst](https://typst.app/) for typesetting and supports
source-controlled diagrams through [Graphviz](https://graphviz.org/). Typst
provides PDF layout and native mathematical notation without requiring a large
TeX installation. [Vale](https://vale.sh/) checks maintained prose against the
Google developer documentation style guide and project-specific rules.
[devenv](https://devenv.sh/) pins the tools and fonts.

## Build

Enter the development environment and build the PDF:

```sh
devenv shell
build-design
```

The command writes `DESIGN.pdf` at the repository root. In an active environment,
run `bin/build-design` for the same build. `devenv test` also builds the document
and checks that the output is nonempty. Run `lint-prose` to check all maintained
prose. Entering the environment installs the same check as a Git pre-commit hook.

## Repository layout

- `design/main.typ` contains the current design and serves as the document entry
  point.
- `design/style.typ` contains shared page and component styling.
- `design/diagrams/*.dot` contains editable Graphviz diagram sources.
- `design/references.bib` contains bibliography metadata.
- `ddrs/` contains numbered design decision records and their template.
- `STATUS.md` tracks open design questions and pending work.
- `references/` contains the local inspiration corpus.
- `bin/build-design` renders any diagrams and compiles `DESIGN.pdf`.
- `AGENTS.md` gives editing guidance to coding agents.

The build generates `design/generated/` and `DESIGN.pdf`. Edit their sources
instead.

## Authoring

Write the design document as a description of the current intended language,
not as a progress report. Ask the project owner about unresolved language or
runtime choices, and track open questions and pending work in `STATUS.md`. Create
a DDR only when the project owner explicitly requests one or approves a proposal
to create it. When the project accepts a DDR, update the design document with the
selected result. If an accepted decision changes, use an accepted superseding DDR
rather than rewriting the original record.

Add a diagram by placing a `.dot` file in `design/diagrams/` and referring to
the corresponding `design/generated/<name>.svg` from Typst. Add citation data to
`design/references.bib`; source documents remain in `references/`.

# Agent guide

## Project purpose

This repository records the design of Sybilant, a programming language for which
the project expects one implementation. The document may describe implementation
choices in detail, but the project does not intend it as a formal,
implementation-independent standard.

## Canonical files

- Edit document content in `design/main.typ`.
- Edit reusable presentation in `design/style.typ`.
- Edit diagram sources in `design/diagrams/*.dot`.
- Edit citation metadata in `design/references.bib`.
- Store approved design decision records under `ddrs/` using
  `ddrs/0000-template.md`.
- Track open design questions and pending work in `STATUS.md`.
- Treat `references/` as source material; do not modify it unless asked.
- Do not hand-edit `design/generated/` or `DESIGN.pdf`.

## Documentation rules

- Write `design/main.typ` as the current intended design. Track project status,
  drafting notes, open questions, and unresolved alternatives in `STATUS.md`.
- Do not invent a Sybilant design decision to fill a gap. Ask the user when the
  task requires a decision.
- Distinguish observable language behavior from implementation notes.
- Introduce metavariables and judgments before using them.
- Keep rule names unique and stable. Keep declarative judgments separate from
  algorithms that decide them unless the equivalence is intentional.
- Use examples to expose behavior, especially boundary and error cases.
- Cite an influence when borrowing a mechanism or presentation, but restate the
  Sybilant-specific decision explicitly.
- Preserve the established Typst visual language unless the user requests a
  redesign.

## DDR policy

- Reserve DDRs for major language or runtime decisions and major departures from
  the existing design. Do not use a DDR for every design decision.
- Create a DDR only when the user explicitly asks for one or approves a proposal
  to create it. If a decision appears to warrant a DDR, explain why and ask for
  approval before creating it.
- Do not create DDRs for tooling or project process.
- Use an approved DDR to preserve the description, criteria, options, rationale,
  and consequences. Present the adopted result directly in the design document.
- Do not rewrite an accepted DDR to change its decision. With user approval,
  add a new DDR and mark the earlier record as superseded.

## Workflow

1. Read the relevant design section, applicable `STATUS.md` entries and existing
   DDRs, and nearby reference material.
2. Ask the user when the task requires a design decision; do not create a DDR
   without explicit request or prior approval.
3. Make the smallest coherent design and implementation change.
4. Run `devenv shell bin/build-design` from the repository root.
5. Confirm that `DESIGN.pdf` exists and inspect build diagnostics.
6. Do not co-author commits.
7. Do not co-author pull requests.

Run `devenv test` after changes to the build environment or build script.

## Generated files

The build script compiles every `design/diagrams/*.dot` file to
`design/generated/*.svg`, then compiles `design/main.typ` to `DESIGN.pdf`.
Git ignores generated files. Every source change must leave the document
rebuildable from a clean checkout.

## Writing style

These guidelines apply to design prose, Markdown, and comments.
Use the Google developer documentation style guide as the base where this section
and `.vale.ini` do not override it.

Vale enforces mechanical rules for maintained prose. Run `lint-prose` after
changing prose and before committing.

For all prose and comments:

- Use active voice and active verbs.
- State points plainly without hedging.
- Be concise without making the writing choppy or omitting needed context.

For prose:

- Write in an inviting and natural tone without slang or forced informality.
- Use contractions only when they read naturally and preserve technical
  precision.
- Keep sentences to 40 words or fewer.
- Use structure to convey meaning.
- Avoid unnecessary structure.
- State the main point of a paragraph up front, then develop it.
- Use diagrams sparingly to summarize, show relationships, or provide examples.
- Use sentence capitalization for headings.

For comments:

- Prefer active voice, but allow passive voice.
- Comments may use sentence fragments.
- Be precise.
- Explain why code takes its form, not what the code does.
- Warn against apparently harmless changes to odd-looking code that could cause
  severe failures.
- Use comments sparingly; prefer clear names and straightforward code.

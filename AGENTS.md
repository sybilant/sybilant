# Agent guide

## Repository layout

- `README.md` -- project overview.
- `STATUS.md` -- current focus and open tasks, per the workflow below.
- `docs/sybilant-manual.typ` -- source for the design and implementation
  manual.
- `docs/sybilant-manual.pdf` -- the built manual. `bin/build-manual`
  regenerates it, and a pre-commit hook keeps it in sync automatically.
- `docs/ddrs/` -- Design Decision Records, one per settled decision,
  named `NNNN-title.md`.
- `bin/` -- project scripts: `build-manual` and `watch-manual`.
- `devenv.nix`, `devenv.yaml`, `devenv.lock` -- the development
  environment: tooling, packages, and the pre-commit hooks.
- `.config/vale/` -- prose linting configuration and vendored style
  rules.

## Workflow

Sybilant's design and implementation grow together, starting from a
minimum viable compiler and runtime and building up incrementally.
Organize the work into tasks and present them in an order that solves
the most important problems first.

Track the current focus and open tasks in STATUS.md. Give each task a
short descriptive title and a number, and order tasks so the one most
others depend on comes first. Insert newly discovered tasks at their
appropriate priority.

Mark a task complete in STATUS.md when it's settled. Remove it from
STATUS.md once its design work lands in the manual, or in a Design
Decision Record (DDR) if the decision is unusual, novel, or one you were
genuinely torn on and want a record of, so you don't keep revisiting it.
Most decisions don't need a DDR. See `STATUS.md` for the format its
entries follow.

## Design decision records

Each DDR follows the structure in `docs/ddrs/0000-template.md`. See
`docs/ddrs/README.md` for naming and scope.

## Prose style

Vale lints prose in Markdown and Typst files against the Google
developer style guide, configured in `.config/vale/vale.ini`. Most
checks fail at error level, which blocks a commit. Check a file before
you stage it:

```
vale --config .config/vale/vale.ini <file>
```

## Building the manual

Run `bin/build-manual` inside `devenv shell` to regenerate
`docs/sybilant-manual.pdf`, or `bin/watch-manual` to rebuild it
automatically on every change and keep it open. Both are also available
as `devenv shell` commands, `build-manual` and `watch-manual`.

## Implementation

No implementation exists yet. Once it starts, this section should cover
the language, project layout, and how to build and test it.

## Source control

- Don't stage files without approval.
- Don't commit without approval.
- Don't push without approval.
- Don't create a Github PR without approval.

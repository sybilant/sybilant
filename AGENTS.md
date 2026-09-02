# Agent guide

## Repository map

- `README.md`: project overview.
- `STATUS.md`: current focus, open tasks, and settled work awaiting
  documentation.
- `docs/sybilant-manual.typ`: design and implementation manual source.
- `docs/sybilant-manual.pdf`: generated manual. A pre-commit hook keeps
  it synchronized.
- `docs/ddrs/`: design decision records, named `NNNN-title.md`.
- `Makefile`: incremental build rules for runtime tests.
- `bin/`: project scripts.
- `devenv.nix`, `devenv.yaml`, and `devenv.lock`: development tools,
  packages, and pre-commit hooks.
- `.config/vale/`: prose lint configuration and vendored rules.

## Workflow

Develop Sybilant's design and implementation together, incrementally
from a minimal compiler and runtime. Split work into tasks and order
prerequisites first.

For every code change:

1. Add tests for the intended behavior. Add only the production stubs
   required to assemble, link, and run them.
2. Run the tests and confirm that the new tests fail for the expected
   reason.
3. Stop and ask the user to approve the test-suite changes. Don't
   implement the behavior before approval.
4. After approval, implement the behavior and rerun the tests.

Prefer adding tests. Modify existing tests only for bug fixes or
refactoring.

Track work in `STATUS.md`. Give each open task a number, short title,
dependencies, and intended outcome. Keep tasks in priority order, with
prerequisites first.

Move settled tasks to the completed section. Remove them after their
design enters the manual or, for an unusual, novel, or difficult
decision worth preserving, a design decision record. Most decisions
don't need one. Check before suggesting one. Follow `STATUS.md` for
entry format.

## Documentation

Follow `docs/ddrs/0000-template.md` for record structure and
`docs/ddrs/README.md` for naming and scope.

Vale checks Markdown and Typst against the configured Google developer
style rules. Check prose before staging it:

```
vale --config .config/vale/vale.ini <file>
```

Inside `devenv shell`, run `bin/build-manual` to regenerate
`docs/sybilant-manual.pdf`, or `bin/watch-manual` to rebuild it after
changes and keep it open. The commands are also available as
`build-manual` and `watch-manual`.

## Runtime implementation

The bootstrap runtime uses Netwide Assembler (NASM) syntax and targets
x86-64 Linux. It links without libc and makes Linux system calls
directly.

- `lib/constants.asm`: constants shared by runtime modules.
- `lib/sybilant.asm`: root runtime functions and the process entry
  point.
- `lib/**/*.asm`: runtime modules.
- `test/<module>/**/*_test.asm`: independent test scenarios for
  `lib/<module>.asm`, organized according to runtime function or
  representation.
- `test/support.asm`: minimal test assertions and the shared
  `sybilant/main` function.
- `Makefile`: assembles and links test executables under `build/test/`.
- `bin/format-assembly`: formats assembly with Emacs `nasm-mode`.
- `bin/test`: builds and runs all tests.

Each runtime test file defines one self-contained `testcase` function.
The function can contain multiple assertions. The shared
`sybilant/main` calls `testcase` and returns status 0 when the function
returns normally. The test runner fails any test executable that runs
for more than five seconds.

Keep each test file focused on one behavior. Combine closely related input
variants when their assertion diagnostics identify the failing case. Put
runtime symbol-shape tests under the module's `abi/` directory.

The name of a runtime module is also its namespace. Functions in
`lib/example.asm` use names such as `sybilant_Sexample_Dsomething`.
Functions in the root `lib/sybilant.asm` module use names such as
`sybilant_Ssomething`.

Indent assembly with four spaces, never tabs. Use left-aligned `;;`
for block comments. Use `;` for inline comments and align them with the
surrounding instructions. Inside `devenv shell`, run
`bin/format-assembly` to format assembly and `bin/test` to test the
runtime. Run `make clean` to delete build artifacts. Pre-commit hooks
format staged assembly automatically.

## Source control

The user batches changes and iterates before committing. Leave changes
unstaged and uncommitted until the user explicitly asks to stage or
commit them. Approval of changes or completion of a task isn't source
control authorization. Don't push or create a GitHub pull request
without explicit approval.

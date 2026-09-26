# Tests

The executable test suite is under `tests/testthat/` and runs as part of the standard full-build audit.

## Running tests

```bash
make test-affected BASE=<commit-before-the-change>
make test
make test-inventory > test_inventory.csv
```

`test-affected` is a developer aid. It combines changed paths, function definitions, transitive source-file calls, test references, and repository-level path rules. It is intentionally conservative and can select more tests than strictly necessary. It does not replace `make test` or the final integrated build.

`test-inventory` derives the current test catalog from executable test files; the repository does not maintain a hand-written per-test catalog that could drift from the suite.

## Test organization

Tests are grouped by scientific or infrastructure responsibility rather than mirroring every implementation file. New tests should normally live with the existing context that owns the behavior being changed.

The full build also runs non-`testthat` validation stages for source syntax, metadata/file requirements, rendered text, report values, cross-references, output structure, and processed-data replication. Those checks should stay outside `testthat` when they require a complete rendered/build state rather than a small fixture.

## Testing principles

Prefer tests that protect:

- observable behavior and returned schemas;
- statistical/methodological invariants;
- fail-closed behavior for invalid scientific states;
- registry relationships and finite-design coverage;
- reproducible identities, counts, and accounting constraints where those are substantive requirements; and
- reviewer-facing output behavior when it is an intentional release requirement.

Avoid assertions that depend only on:

- exact source-code strings or helper placement;
- private implementation structure;
- prose wording, heading order, or editorial choices;
- LaTeX fragments or styling details that do not define a release requirement; or
- a duplicate hand-written inventory of information already represented by an executable registry.

A source-text assertion is appropriate only when the literal text is itself the interface being protected, such as a required shell command, rendered release string, or file-format contract that cannot be exercised more directly.

## Adding or changing tests

When changing code:

1. inspect the existing tests for the affected scientific/module boundary;
2. update behavioral and methodological coverage in the same patch;
3. reuse small fixtures and shared test helpers instead of duplicating setup;
4. test both the successful path and the important fail-closed invariant;
5. run `make test-affected` while iterating when useful; and
6. run `make test` before delivery, followed by the integrated build for changes that affect rendering, target composition, metadata, or generated outputs.

Do not preserve a compatibility wrapper, duplicate helper, or obsolete interface solely because a test calls it. If the production interface has been superseded, migrate the test to the active implementation and remove the dead compatibility layer when safe.

## Full-build integration

The standard audit runs the complete suite after static syntax/metadata checks and before the main target build. A passing unit suite therefore validates code-level behavior but does not by itself establish that the full research/rendered outputs are valid. See [`../docs/BUILD.md`](../docs/BUILD.md) for the complete validation sequence.

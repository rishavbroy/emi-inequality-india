# Testing workflow

The full public-build audit continues to run the complete `testthat` suite. The
commands below are developer aids for finding and exercising likely downstream
tests before that final run.

```bash
make test-affected BASE=<commit-before-the-change>
make test
make test-inventory > test_inventory.csv
```

`test-affected` is intentionally conservative. It combines changed paths,
function definitions, a transitive source-file call scan, test references, and
repository-level path rules. It can over-select tests, and it does not replace
`make test` or the public-build audit.

`test-inventory` generates the current test catalog directly from the test
files. The repository does not maintain a hand-written per-test catalog that
could drift away from the executable suite.

When changing code:

1. Inspect and update the affected tests in the same patch as the code.
2. Prefer observable behavior, schemas, and methodological invariants over
   exact source strings or internal helper structure.
3. Keep exact LaTeX, figure-style, prose, and archive-layout assertions only
   when they represent an intentional reviewer-facing release contract.
4. Run affected tests while iterating, the full suite before delivery, and the
   public-build audit for the final integrated check.

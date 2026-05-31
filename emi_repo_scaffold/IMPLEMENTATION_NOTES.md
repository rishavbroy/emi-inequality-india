# Implementation notes

This bundle implements the repo structure and scaffolding discussed in the planning conversation.

## How to use

1. Copy or overlay this scaffold into the local project root.
2. Run the migration script in dry-run mode:

```bash
python scripts/migrate_repo_structure.py --root .
```

3. Inspect the proposed moves/deletions.
4. Execute only after reviewing:

```bash
python scripts/migrate_repo_structure.py --root . --execute
```

5. Extract legacy Rmd chunks:

```bash
python scripts/extract_legacy_rmd_chunks.py 580-Draft-ECON-580.Rmd --out archive/legacy-rmd-chunks
```

## What is implemented now

- Full target folder structure.
- Neutral `.gitignore`.
- Makefile with `renv`, `targets`, report, samples, and tests commands.
- Draft/final/diagnostics YAML configs.
- `_targets.R` pipeline skeleton.
- R file layout with function boundaries matching the agreed refactor.
- Metadata CSV templates and active-file manifest.
- Writing/coding sample specs based on current sample excerpts.
- Diagnostic/polished QMD skeletons.
- Testthat scaffolding.

## What still requires semantic migration

The R files contain function boundaries, some extracted logic, and many TODO placeholders. A coding agent should migrate the corresponding code from `archive/legacy-rmd-chunks/` into these functions while preserving comments and authorial voice.

The pipeline is intentionally package-like but not a full R package. `DESCRIPTION`, `NAMESPACE`, and `man/` are deferred.

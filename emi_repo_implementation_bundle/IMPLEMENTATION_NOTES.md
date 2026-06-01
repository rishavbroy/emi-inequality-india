# EMI repo implementation bundle, v3

This bundle applies the final migration patches requested after inspecting v2.

## What changed in v3

- `IMPLEMENTATION_NOTES.md` is moved to `docs/admin/IMPLEMENTATION_NOTES.md` during migration, so it does not remain as an unexpected top-level item.
- The old broad `("623*", "archive/623-version")` glob was replaced with narrow `623*.Rmd` and `623*.pdf` globs, so generated 623 caches and `.nb.html` render byproducts are removed by the delete patterns rather than archived.
- `Papers/` is merged directly into lowercase `papers/` instead of becoming `papers/Papers/`; the empty source directory is removed after its contents are moved.
- `application-samples/specs/coding-full.yml` now points to `application-samples/cover-notes/coding/full.qmd`, not the 47-page cover note.

## Recommended command order

Overlay/copy this scaffold into your local repo. Then run:

```bash
python scripts/migrate_repo_structure.py --root .
python scripts/migrate_repo_structure.py --root . --execute
python scripts/extract_legacy_rmd_chunks.py archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd --out archive/legacy-rmd-chunks
Rscript scripts/init_renv.R
```

The first command is a dry run. Review it before executing.

## Remaining semantic work

The migration script performs structural reorganization; it does not guarantee that the refactored analysis pipeline is semantically complete. The `R/` files are structured to receive code from the legacy Rmd, but a coding agent still needs to migrate the real chunk logic into the right functions, preserve your comments/authorial voice, and then run tests/targets against your local data.

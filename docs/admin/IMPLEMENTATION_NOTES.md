# EMI repo implementation bundle v4

This bundle implements the repository structure agreed in the planning conversation.
It includes a conservative migration script, project scaffolding, target/config/test scaffolds,
application-sample specs, and diagnostic/report notebooks.

## What changed in v4

- `IMPLEMENTATION_NOTES.md` is moved into `docs/admin/IMPLEMENTATION_NOTES.md` during migration.
- Prior implementation bundles such as `emi_repo_implementation_bundle/` and `emi_repo_implementation_bundle*.zip` are moved to `archive/implementation-bundles/`.
- Hidden top-level byproduct files `.dkm`, `.dta`, `.ind`, `.missRecode`, and `.nsf` are removed during migration.
- The real bibliography `English Education Economic Returns.bib` is moved to `paper/references.bib`; the scaffold no longer ships a placeholder `paper/references.bib` that could block the move.
- Conflict handling now compares file hashes. If the source and destination are identical, the source is removed; if they differ, the source is moved to a `__conflict_#` filename.
- `Papers/` contents are merged directly into lowercase `papers/`, then the empty `Papers/` directory is removed.
- Legacy 623 caches/render byproducts are removed by delete patterns rather than archived.

## Recommended order

Run the migration in dry-run mode first:

```bash
python scripts/migrate_repo_structure.py --root .
```

After reviewing the planned moves/removals:

```bash
python scripts/migrate_repo_structure.py --root . --execute
python scripts/extract_legacy_rmd_chunks.py archive/legacy-paper-drafts/580-Draft-ECON-580.Rmd --out archive/legacy-rmd-chunks
Rscript scripts/init_renv.R
```

## Important limitation

The bundle creates a rigorous project scaffold and safe structural migration, but it is not yet a full semantic refactor of every line of `580-Draft-ECON-580.Rmd`. The next pass should migrate the archived Rmd chunks into the corresponding functions under `R/` while preserving Rishav's comments and authorial voice.

## Top-level cleanliness

The migration script prints a top-level cleanliness report at the end. In a simulation against the top-level items from `file_list.txt` plus the edge cases raised in review, it returned:

```text
No unexpected top-level items detected.
```

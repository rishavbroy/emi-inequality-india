# Build and validation

This document is the human-facing execution reference. Scientific configuration, optional build families, and destructive reconstruction are separate choices.

## Research configurations

- [`config/final.yml`](../config/final.yml) is the release configuration used by the paper and final checks. It runs the full average-marginal-effect calculation and strict district-panel validation.
- [`config/fast.yml`](../config/fast.yml) is for code iteration. It retains the registered estimands and variable definitions while reducing expensive release-only checks.

There is no separate diagnostic configuration. Extended diagnostics and benchmarks are optional target families layered onto the selected scientific configuration.

## Main commands

| Task | Command |
|---|---|
| Restore the R library | `make restore` |
| Prepare automatically retrievable inputs | `make prepare-data` |
| Run unit tests | `make test` |
| Run the final target graph without samples/poster | `make pipeline` |
| Run the faster development target graph | `make pipeline-fast` |
| Render and validate the paper | `make paper` |
| Render application samples | `make samples` |
| Render the optional conference poster | `make poster` |
| Run the processed-data replication | `make replicate-processed` |
| Verify processed results against the full store | `make verify-processed-replication` |
| Run the ordinary complete build | `make` or `make all` |
| Remove generated renders/optional outputs | `make clean` |
| Also destroy both `{targets}` stores | `make clean-all` |

`make` and `make all` delegate to `scripts/run_full_build.sh` with its ordinary defaults.

## Full-build sequence

The standard build executes these stages in order:

1. check source whitespace;
2. restore the project library from `renv.lock`;
3. optionally remove generated state for a clean-slate run;
4. prepare automatically retrievable inputs;
5. check stale/live `{targets}` process metadata;
6. run shell, Python, R, QMD, manifest, and renv synchronization checks;
7. run the complete `testthat` suite unless explicitly skipped;
8. build/validate the lineage geometry;
9. run requested extended diagnostics;
10. run the selected main target graph, including samples/poster when requested;
11. run fast or final publication checks;
12. verify the processed-data replication in final mode;
13. run requested benchmarks;
14. fail on persisted target warnings;
15. write the output manifest; and
16. write the review archive unless disabled.

The ordinary run preserves the existing target store. `{targets}` checks dependencies and reruns work whose recorded inputs/commands are no longer current.

## Full-build options

The standard shell entry point is:

```bash
scripts/run_full_build.sh
```

Useful options are:

- `--no-samples` — omit application-sample rendering and checks;
- `--with-poster` — render and require the conference poster;
- `--with-extended-diagnostics` — run the extended scientific diagnostic family;
- `--with-benchmarks` — run optional benchmarks;
- `--fast` — use `config/fast.yml` and fast publication checks;
- `--from-clean-slate` — remove generated outputs and both target stores before reconstruction;
- `--no-archive` — do not write `review.zip`; and
- `--skip-tests` — skip unit tests for an explicitly scoped run.

`--from-clean-slate` is an exceptional reconstruction check. A fresh clone already has no target store, and ordinary development should allow `{targets}` to decide what is out of date.

The main build requests up to four consumption-domain workers by default. Override with `EMI_CONSUMPTION_DOMAIN_CORES`; the R code clamps the request to detected physical cores and uses serial execution on Windows.

## Optional target families

The maintained environment switches are:

- `EMI_RUN_EXTENDED_DIAGNOSTICS`;
- `EMI_RUN_BENCHMARKS`;
- `EMI_RENDER_APPLICATION_SAMPLES`; and
- `EMI_RENDER_POSTER`.

The full-build script is the intended human interface to these switches. For direct maintenance/debugging, the Make targets `extended-diagnostics`, `benchmarking`, `samples`, and `poster` select the corresponding families.

Extended diagnostic files intended for durable review should terminate in a `diag_ext_` file target so prefix-selected runs actually traverse the computation that produces them.

## Target subsets and reruns

Useful targeted commands include:

```bash
make public-diagnostics
make extended-diagnostics
make benchmarking
make rerun-extended-diagnostics
make rerun-benchmarks
```

For direct target selection, use the maintained `scripts/run_targets_checked.R` interface rather than creating a second ad hoc execution script.

## Build markers and maintenance reports

Successful strict builds write short-lived markers such as `.pipeline-final-ok` and `.public-final-ok`; downstream archive/release checks use these to distinguish a verified render from arbitrary files left in the working tree.

Final checks also write maintenance reports under `outputs/build/`:

- `source_health.csv` is an advisory static inventory for possible orphan/duplicate production functions and direct aliases;
- `schema_less_csvs.csv` is a hard output-structure check; and
- `duplicate_generated_csvs.csv` is advisory because distinct analyses can legitimately serialize identical tables.

`outputs/build/output_manifest.csv` is written after requested target families and renders finish. It indexes generated filesystem outputs; it is not a replacement for the scientific design registries.

## Application samples

`make samples` reads [`../application-samples/samples.yml`](../application-samples/samples.yml) and generates named/anonymous writing and coding variants from the current paper/code. Sample selection, numbering preservation, page-count policy, and publication behavior are documented in [`../application-samples/README.md`](../application-samples/README.md).

Application-sample page targets are currently advisory while typography is still being finalized: mismatches emit a visible `WARNING:` and do not fail the build.

## GitHub Pages

`.github/workflows/pages.yml` publishes the tracked current paper and named application-sample PDFs after pushes to `main`. The workflow copies committed PDFs; it does not restore raw data or rerun the empirical analysis in CI. Anonymous application samples are excluded because the repository itself identifies the author.

## Processed-data replication

`make replicate-processed` executes [`../_targets_processed.R`](../_targets_processed.R) in the separate `_targets_processed/` store using tracked district-level inputs and metadata. `make verify-processed-replication` compares the five shared result objects with the full `_targets/` store. See [`../REPLICATION.md`](../REPLICATION.md) for exact scope, exclusions, and data prerequisites.

## Troubleshooting

- **Live `{targets}` process:** the full build stops instead of killing it. Inspect the printed PID and terminate only an abandoned run.
- **Package restore failure:** install the package's system requirements, then rerun `make restore`.
- **Missing registered input:** use the exact path reported by source preflight and consult `data/metadata/file_manifest.csv` plus `DATA_AVAILABILITY.md`.
- **Need a failed-run snapshot:** use `scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip`.
- **Need a true reconstruction check:** use `scripts/run_full_build.sh --from-clean-slate`.

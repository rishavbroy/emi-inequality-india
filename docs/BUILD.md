# Build and validation

This document is the execution reference. The scientific configuration, optional steps, and reconstruction from scratch can be selected independently.

## Research configurations

- [`config/final.yml`](../config/final.yml) is the release configuration used by the paper and final checks. It runs the full average-marginal-effect calculation and strict district-panel validation.
- [`config/fast.yml`](../config/fast.yml) is for code iteration. It retains the registered estimands and variable definitions while reducing expensive release-only checks.

Extended validation checks and benchmarks are optional steps within the selected scientific configuration.

## Main commands

| Task | Command |
|---|---|
| Restore the R library | `make restore` |
| Prepare automatically retrievable inputs | `make prepare-data` |
| Run unit tests | `make test` |
| Run the final analysis without samples/poster | `make pipeline` |
| Run the faster development analysis | `make pipeline-fast` |
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
3. optionally remove generated state to test reconstruction from scratch;
4. prepare automatically retrievable inputs;
5. check stale/live `{targets}` process metadata;
6. run shell, Python, R, QMD, manifest, and renv synchronization checks;
7. run the complete `testthat` suite unless explicitly skipped;
8. build/validate the lineage geometry;
9. run requested extended validation checks;
10. run the selected main analysis, including samples/poster when requested;
11. run fast or final publication checks;
12. verify the processed replication in final mode;
13. run requested benchmarks;
14. fail on persisted target warnings;
15. write the list of generated outputs; and
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
- `--with-extended-diagnostics` — run the extended validation checks;
- `--with-benchmarks` — run optional benchmarks;
- `--fast` — use `config/fast.yml` and fast publication checks;
- `--from-clean-slate` — remove generated outputs and both target stores before rebuilding;
- `--no-archive` — do not write `review.zip`; and
- `--skip-tests` — skip unit tests for an explicitly scoped run.

`--from-clean-slate` verifies reconstruction after generated state has been deleted. A fresh clone already has no target store, and ordinary development can let `{targets}` decide what is out of date.

The main build requests up to four consumption-domain workers by default. Override with `EMI_CONSUMPTION_DOMAIN_CORES`; the R code clamps the request to detected physical cores and uses serial execution on Windows.

## Optional build steps

The maintained environment switches are:

- `EMI_RUN_EXTENDED_DIAGNOSTICS`;
- `EMI_RUN_BENCHMARKS`;
- `EMI_RENDER_APPLICATION_SAMPLES`; and
- `EMI_RENDER_POSTER`.

The full-build script is the recommended command-line interface to these switches. For maintenance and debugging, the Make targets `extended-diagnostics`, `benchmarking`, `samples`, and `poster` select the corresponding steps.

Extended validation files kept for review should terminate in a `diag_ext_` file target so prefix-selected runs execute the computation that produces them.

## Target subsets and reruns

Useful targeted commands include:

```bash
make public-diagnostics
make extended-diagnostics
make benchmarking
make rerun-extended-diagnostics
make rerun-benchmarks
```

For direct target selection, use the maintained `scripts/run_targets_checked.R` interface.

## Build markers and maintenance reports

Successful strict builds write short-lived markers such as `.pipeline-final-ok` and `.public-final-ok`; archive and release checks use these markers to record that rendering and validation completed.

Final checks also write maintenance reports under `outputs/build/`:

- `source_health.csv` lists possible unused or duplicate production functions and direct aliases for maintainer review;
- `schema_less_csvs.csv` is a hard output-structure check; and
- `duplicate_generated_csvs.csv` is advisory because distinct analyses can legitimately serialize identical tables.

`outputs/build/output_manifest.csv` is written after the requested analyses and renders finish. It lists generated filesystem outputs; scientific definitions remain in the corresponding registries.

## Application samples

`make samples` reads [`../application-samples/samples.yml`](../application-samples/samples.yml) and generates named/anonymous writing and coding variants from the current paper/code. Sample selection, numbering preservation, page-count policy, and publication behavior are documented in [`../application-samples/README.md`](../application-samples/README.md).

Application-sample page targets currently emit a visible `WARNING:` when the rendered length differs from the target. Page-count enforcement can become strict after the typography is final.

## GitHub Pages

`.github/workflows/pages.yml` publishes the tracked current paper and named application-sample PDFs after pushes to `main`. The Pages job copies committed PDFs and does not rerun the empirical analysis. Historical named code-sample filenames are staged as aliases of the current code sample so previously distributed Pages URLs remain valid without duplicating generated PDFs in the repository. The staged custom 404 page sends browser requests for other missing paths back to the project landing page. Anonymous application samples are excluded because the repository itself identifies the author.

## Processed-data replication

`make replicate-processed` executes [`../_targets_processed.R`](../_targets_processed.R) in the separate `_targets_processed/` store using tracked district-level inputs and metadata. `make verify-processed-replication` compares the five shared results with the full `_targets/` store. See [`../REPLICATION.md`](../REPLICATION.md) for exact scope, exclusions, and data prerequisites.

## Troubleshooting

- **Live `{targets}` process:** the full build stops and prints the PID. Terminate the process only if it is an abandoned run.
- **Package restore failure:** install the package's system requirements, then rerun `make restore`.
- **Missing registered input:** use the exact path reported by input validation and consult `data/metadata/file_manifest.csv` plus `DATA_AVAILABILITY.md`.
- **Need a failed-run snapshot:** use `scripts/make_review_archive.sh --without-samples --allow-incomplete --output review.zip`.
- **Need a true reconstruction check:** use `scripts/run_full_build.sh --from-clean-slate`.

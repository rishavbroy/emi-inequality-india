# Building and checking the project

The repository has two computation configurations and a small set of optional build families. These are separate choices.

## Configurations

- `config/final.yml` is the research configuration used for the paper and release checks. It runs the full average-marginal-effect calculation and applies the strict district-panel checks.
- `config/fast.yml` is for code iteration. It keeps the same registered estimands and variable definitions but skips the expensive full AME calculation and relaxes strict panel checks that are useful only for release validation.

There is no separate diagnostic configuration. Extended checks and benchmarks are selected by build options while retaining the chosen research configuration.

## Common commands

| Task | Command |
|---|---|
| Run unit tests | `make test` |
| Run the final target set without samples or the poster | `make pipeline` |
| Run the faster development target set | `make pipeline-fast` |
| Render and validate the papers | `make paper` |
| Render application samples | `make samples` |

`make samples` reads `application-samples/samples.yml`. Writing samples are selected by ordinary Quarto section IDs from `paper/paper.qmd`; coding samples use marker-delimited R excerpts. Named and anonymous variants are generated from the same selections. Writing excerpts run their section selector at Quarto's `post-quarto` stage. The full-paper render keeps `paper.tex`, then a no-PDF XeLaTeX pass writes `paper-reference-labels.aux`; before the selector removes omitted targets, it resolves Quarto's deferred LaTeX `\ref` against that full-paper label file and appends a compact `(full paper)` suffix. Direct references to selected section IDs remain local. Excerpt renders also keep their generated `.tex` files under `application-samples/.work/` so final layout checks inspect the post-filter document rather than the unfiltered QMD.
| Render the conference poster | `make poster` |
| Re-render all QMD families | `make qmd-renders` |
| Run the ordinary full build | `make` or `make all` |
| Remove generated renders and optional outputs | `make clean` |
| Also destroy the `{targets}` store | `make clean-all` |

`make` and `make all` call `bash scripts/run_full_build.sh` with its defaults.


## GitHub Pages publication

`.github/workflows/pages.yml` publishes the tracked current paper and named application-sample PDFs to GitHub Pages after pushes to `main`. It copies already-rendered files; it does not install R, restore raw data, or rerun the research build in CI. This keeps publication separate from empirical reconstruction while ensuring the browser-hosted PDFs update whenever a successful local build is committed. Anonymous application samples are excluded from Pages because the repository URL itself identifies the author.

## Full-build options

The ordinary command is:

```bash
bash scripts/run_full_build.sh
```

It uses `config/final.yml`, includes application samples, omits the conference poster, preserves the `{targets}` store, and writes `review.zip` on success or failure.

Useful options are:

- `--no-samples`: omit application-sample rendering and checks.
- `--with-poster`: construct poster-only figure dependencies, render the conference poster, and require its outputs.
- `--with-extended-diagnostics`: run extended research checks.
- `--with-benchmarks`: run benchmarks.
- `--fast`: use `config/fast.yml`.
- `--from-clean-slate`: remove generated outputs and destroy `_targets/` before rebuilding.
- `--no-archive`: do not create `review.zip`.
- `--skip-tests`: skip unit tests.

The default preserves cached target values because `{targets}` already reruns only work that is out of date. `--from-clean-slate` is therefore an exceptional reconstruction check, not the ordinary development mode. A fresh clone already starts without a target store.

Final checks also write advisory maintenance reports under `outputs/build/`. `source_health.csv` lists top-level production functions and flags definitions with no symbolic or registered metadata reference for manual review; it does not fail the build because dynamic R dispatch can defeat static reachability analysis. `schema_less_csvs.csv` remains a hard output-structure check, while `duplicate_generated_csvs.csv` is advisory because independent analyses may legitimately serialize identical results.

## Optional target families

The maintained build switches `EMI_RUN_EXTENDED_DIAGNOSTICS`, `EMI_RUN_BENCHMARKS`, `EMI_RENDER_APPLICATION_SAMPLES`, and `EMI_RENDER_POSTER` determine which optional target definitions `_targets.R` includes. They do not select a different YAML configuration.

The full-build script is the intended human interface to those switches. `_targets.R` does not print a message every time an omitted family is encountered; the script prints one build-profile summary instead.

## Processed-data analysis replication

`make replicate-processed` runs `_targets_processed.R` in its own `_targets_processed/` store using only the tracked district panel, district welfare estimates, and tracked metadata. It reruns the paper-facing district consumption/IV, schooling-conversion, alternative-distance first-stage, and first-stage-absorption analyses. It intentionally does not rerun the individual-level NSS selection model or source-level reconstruction. `make verify-processed-replication` updates that graph and compares the reported empirical components of the five shared result objects with the full `_targets/` store; a final full build performs this check automatically.

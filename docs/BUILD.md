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
| Render the conference poster | `make poster` |
| Run extended checks, benchmarks, and analysis reports | `make analysis` |
| Re-render all QMD families | `make qmd-renders` |
| Run the ordinary full build | `make` or `make all` |
| Remove generated renders and optional outputs | `make clean` |
| Also destroy the `{targets}` store | `make clean-all` |

`make` and `make all` call `bash scripts/run_full_build.sh` with its defaults.

## Full-build options

The ordinary command is:

```bash
bash scripts/run_full_build.sh
```

It uses `config/final.yml`, includes application samples, omits analysis reports and the conference poster, preserves the `{targets}` store, and writes `review.zip` on success or failure.

Useful options are:

- `--no-samples`: omit application-sample rendering and checks.
- `--with-analysis`: render analysis reports and run the extended results and benchmarks they read.
- `--with-poster`: render and require the conference poster.
- `--with-extended-diagnostics`: run extended research checks without rendering analysis reports.
- `--with-benchmarks`: run benchmarks without rendering analysis reports.
- `--fast`: use `config/fast.yml`.
- `--from-clean-slate`: remove generated outputs and destroy `_targets/` before rebuilding.
- `--no-archive`: do not create `review.zip`.
- `--skip-tests`: skip unit tests.

The default preserves cached target values because `{targets}` already reruns only work that is out of date. `--from-clean-slate` is therefore an exceptional reconstruction check, not the ordinary development mode. A fresh clone already starts without a target store.

## Optional target families

The environment switches `EMI_RUN_EXTENDED_DIAGNOSTICS`, `EMI_RUN_BENCHMARKS`, `EMI_RENDER_ANALYSIS_NOTES`, `EMI_RENDER_APPLICATION_SAMPLES`, and `EMI_RENDER_POSTER` determine which optional target definitions `_targets.R` includes. They do not select a different YAML configuration.

The full-build script is the intended human interface to those switches. `_targets.R` does not print a message every time an omitted family is encountered; the script prints one build-profile summary instead.

## Processed-data analysis replication

`make replicate-processed` runs `_targets_processed.R` in its own `_targets_processed/` store using only the tracked district panel, district welfare estimates, and tracked metadata. It reruns the paper-facing district consumption/IV, schooling-conversion, alternative-distance first-stage, and first-stage-absorption analyses. It intentionally does not rerun the individual-level NSS selection model or source-level reconstruction.

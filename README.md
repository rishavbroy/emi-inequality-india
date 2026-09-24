# Unequal Access to Equalizing Opportunities

This repository contains the code, metadata, processed outputs, and build instructions for my paper, "Unequal Access to Equalizing Opportunities: Variation in Linguistic Conditions, Access to English-Medium Instruction, and Local Economic Outcomes in India."

For build and replication commands, see [`docs/BUILD.md`](docs/BUILD.md) and [`REPLICATION.md`](REPLICATION.md).
A raw-data-less district analysis replication is available with `make replicate-processed`; `make verify-processed-replication` checks its shared empirical targets against the full-source target store. Full reconstruction still requires the source files documented in `REPLICATION.md`.

## Key files

- **[`paper/paper-new.pdf`](paper/paper-new.pdf): Current paper.** Its tables, figures, and reported quantities are generated from the repository.
- [`paper/paper-new.qmd`](paper/paper-new.qmd): Current paper source.
- [`paper/paper.qmd`](paper/paper.qmd) and [`paper/paper.pdf`](paper/paper.pdf): Earlier paper retained for comparison and application-sample source excerpts.
- [`REPLICATION.md`](REPLICATION.md): Replication guide.
- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md): Source-by-source data availability and redistribution notes.

- [`docs/plan/roadmap.md`](docs/plan/roadmap.md): Current empirical roadmap and source-first priorities.
- [`application-samples/output/RishavRoy_WritingSample.pdf`](application-samples/output/RishavRoy_WritingSample.pdf): Reviewer-facing writing sample generated from [marked excerpts](application-samples/specs/writing-10pg.yml) in the [paper](paper/paper.qmd). Cover note updates are still pending.
- [`application-samples/output/RishavRoy_CodingSample.pdf`](application-samples/output/RishavRoy_CodingSample.pdf):  Reviewer-facing coding sample generated from [marked excerpts](application-samples/specs/coding-full.yml) in the [code](R). Cover note updates are still pending.

## Research question

How are inherited linguistic conditions, access to English-medium instruction (EMI), and the later local economic setting distributed across Indian districts, and how do these conditions relate to inequality?

## Empirical design and current status

The analytical geography is Census-2001 districts. The pipeline combines NSS education surveys, historical NSS consumer-expenditure rounds, modern HCES 2022-23/2023-24, Census language and socioeconomic tables, reviewed district lineage, and price indices. District-level EMI exposure is instrumented with population-weighted linguistic distance from Hindi, with alternative constructions, historical balance checks, weak-IV-robust inference, monotonicity diagnostics, and extensive source-specific mechanism diagnostics. Consumption outcomes are price-adjusted and estimated under their survey designs before harmonization to the analytical geography.

The paper text can lag the active pipeline, so empirical claims should be checked against generated tables/diagnostics and the current roadmap rather than older prose. [`docs/plan/roadmap.md`](docs/plan/roadmap.md) records the remaining source-first work.

Current build status:

- report values are generated through [`R/output/build_report_values.R`](R/output/build_report_values.R) and checked before final rendering;
- accepted historical refactor warnings are frozen in the refactor-proof tag rather than enforced in active builds;
- lengthy diagnostics and benchmarks are optional, not part of ordinary public builds;
- several optional diagnostic/benchmarking artifacts are still investigative rather than final empirical claims, especially district-matching diagnostics, fuzzy-matching benchmarks, AME benchmarks, and experimental spatial-IV attempts.

## For faculty reviewers and hiring/admissions committees

I believe the [current paper](paper/paper-new.pdf) displays multiple traits that are essential to high-quality economics research: a commitment to institutional knowledge and literature reviews, a deep understanding of econometrics, and meticulous empirical judgement.

This repository, on the other hand, is intended to signal more than just one PDF. Its current state contains the following:

- **Completed refactor proof.** The legacy-to-Quarto migration and parity machinery has been frozen under `archive/refactoring/` and the `archive/refactoring-complete` branch/tag workflow. Active paper and research modules are now edited directly.
- **Targets-based research build.** [`_targets.R`](_targets.R) organizes raw-data readers, district tracking, measure construction, IV/probit models, figures, tables, diagnostics, report rendering, and application samples.
- **Verified final outputs.** [`scripts/run_full_build.sh`](scripts/run_full_build.sh) checks source whitespace without modifying the working tree, runs tests and final checks, optionally runs extended research checks and benchmarks, records machine-readable build status, and packages `review.zip`.
- **Review archives without raw data.** [`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) packages source, rendered outputs, and selected research checks into `review.zip` while omitting local raw data and caches.
- **Application-sample automation.** Writing and coding samples are generated from the same source and target build used for the paper rather than hand-maintained as separate PDFs.

## Repository map

- [`R/`](R/): function-based research code.
- [`R/diagnostics/`](R/diagnostics/): core and extended research checks.
- [`R/benchmarking/`](R/benchmarking/): opt-in benchmarking/tuning target helpers.
- [`config/`](config/): `fast.yml` for iteration and `final.yml` for paper/release calculations.
- [`_targets.R`](_targets.R): dependency graph for data cleaning, modeling, diagnostics, benchmarks, figures, tables, report rendering, and application samples.
- [`paper/`](paper/): Quarto source and rendered paper output.
- [`application-samples/`](application-samples/): cover notes, sample specifications, and generated writing/coding samples.
- `outputs/build/`: short-lived build metadata and warnings, regenerated by full builds and intentionally untracked.
- `outputs/diagnostics/public/`: short-lived core research checks, regenerated by builds and intentionally untracked.
- [`outputs/diagnostics/extended/`](outputs/diagnostics/extended/): opt-in diagnostic artifacts preserved across ordinary public builds.
- [`outputs/benchmarking/`](outputs/benchmarking/): opt-in method/timing/tuning benchmark artifacts.
- [`data/metadata/`](data/metadata/): tracked source catalog, manifests, checksums, current crosswalk, and district-lineage adjudication ledgers; see [`data/metadata/README.md`](data/metadata/README.md).
- [`docs/DISTRICT_LINEAGE.md`](docs/DISTRICT_LINEAGE.md): durable data and methodology handoff for the Census 2001 district panel.
- [`docs/EDUCATION_SELECTION.md`](docs/EDUCATION_SELECTION.md): enrollment selection and average marginal effects.
- [`docs/SPATIAL_ANALYSIS.md`](docs/SPATIAL_ANALYSIS.md): spatial weights, Moran tests, and the boundary around experimental spatial IV work.
- [`data/processed/`](data/processed/): processed analysis exports and reproducible processed geography used downstream.
- `data/raw/`, `data/raw_future/`, `data/interim/`: local-only, gitignored data folders.
- [`docs/`](docs/): methodological notes and project planning documents.
- [`archive/`](archive/): legacy drafts, rendered artifacts, source samples, and extracted legacy Rmd chunks.

## Quickstart

```bash
make restore   # restore the R library recorded in renv.lock
make test      # unit tests
make pipeline  # final research targets, without samples or poster
make paper     # render and validate the papers
make samples   # application samples
make all       # ordinary full build; equivalent to scripts/run_full_build.sh
```

The full command reference and the distinction between `config/fast.yml`, `config/final.yml`, and optional build families are in [`docs/BUILD.md`](docs/BUILD.md).

Raw data are not tracked. The full build first runs `make prepare-data`, which downloads any missing Census tables listed in the Census acquisition manifests. Sources that cannot be redistributed or downloaded automatically must be placed at the paths in [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv); see [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md).

For the usual no-samples review run used during development:

```bash
caffeinate -dimsu bash scripts/run_full_build.sh \
  --no-samples \
  --with-extended-diagnostics \
  --with-benchmarks \
  2>&1 | tee full_output.txt
```

The ordinary script invocation includes application samples but does not render the conference poster. Add `--with-poster` when that output is wanted. Use `--from-clean-slate` only when you specifically want to destroy the `{targets}` store and generated outputs before reconstruction.

## Behavior without raw data

The pipeline should fail gracefully when required raw data are absent: it reads [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv), checks the listed paths, and reports the exact missing files before attempting to call readers like `read_sav()`, `read_excel()`, or `sf::st_read()`. A cryptic raw-reader path error should be treated as a bug.

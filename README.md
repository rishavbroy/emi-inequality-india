# Escaping Inequality in India: English-Medium Instruction and Local Development

This repository contains the code and derived data needed to replicate my paper, "Escaping Inequality in India: The Role of English-Medium Instruction," as well as the application samples built from it.


The legacy-to-Quarto refactor is complete. Active paper, analysis, and code changes now happen in the current source tree rather than by regenerating public documents from the archived R Markdown draft. The completed refactor proof is preserved through the `archive/refactoring-complete` tag/branch workflow described in [`archive/refactoring/README.md`](archive/refactoring/README.md).

Use the ["Commands for running and auditing"](#commands-for-running-and-auditing) listed below to:

1. Generate empirical outputs using [`{targets}`](_targets.R).
2. Render the current paper, application samples, and public artifacts from their active sources.
3. Run tests and current public-output checks.
4. Optionally run extended diagnostics, benchmarks, and analysis notebooks for methodological review.

## Key files

- **[`paper/report.pdf`](paper/report.pdf): Current rendered paper**. All of its results, tables, and figures are generated in this codebase.
- [`paper/report.qmd`](paper/report.qmd): Source of paper. This is now an ordinary active Quarto source file edited directly.
- [`REPLICATION.md`](REPLICATION.md): Replication guide.
- [`DATA_AVAILABILITY.md`](DATA_AVAILABILITY.md): Source-by-source data availability and redistribution notes.

- [`docs/plan/roadmap.md`](docs/plan/roadmap.md): My plan going forward, after this refactoring is done.
- [`application-samples/output/RishavRoy_WritingSample.pdf`](application-samples/output/RishavRoy_WritingSample.pdf): Reviewer-facing writing sample generated from [marked excerpts](application-samples/specs/writing-10pg.yml) in the [paper](paper/report.qmd). Cover note under active review.
- [`application-samples/output/RishavRoy_CodingSample.pdf`](application-samples/output/RishavRoy_CodingSample.pdf):  Reviewer-facing coding sample generated from [marked excerpts](application-samples/specs/coding-full.yml) in the [code](R). Cover note under active review.

## Research question

Does increasing baseline district-level exposure to English-medium instruction (EMI) in 2007-08 affect local consumption growth over the next decade?

## Empirical design and current status

This project combines 2007-08 and 2017-18 National Sample Survey microdata, 2001 Census mother tongue data, 2020 district boundary data, and district change trackers to construct a district pseudo-panel. The current 2SLS design instruments district-level EMI exposure with a population-weighted measure of linguistic distance from Hindi. Pending the changes I discuss in [`docs/plan/roadmap.md`](docs/plan/roadmap.md), all current estimates should be treated as **provisional**.

Current build status:

- report values are generated through [`R/output/build_report_values.R`](R/output/build_report_values.R) and audited before final rendering;
- accepted historical refactor warnings are frozen in the refactor-proof tag rather than enforced in active builds;
- lengthy diagnostics and benchmarks are optional, not part of ordinary public builds;
- several optional diagnostic/benchmarking artifacts are still investigative rather than final empirical claims, especially district-matching diagnostics, fuzzy-matching benchmarks, AME benchmarks, and experimental spatial-IV attempts.

## For faculty reviewers and hiring/admissions committees

I believe the [current draft](paper/report.pdf) displays multiple traits that are essential to high-quality economics research: a commitment to institutional knowledge and literature reviews, a deep understanding of econometrics, and meticulous empirical judgement.

This repository, on the other hand, is intended to signal more than just one PDF. Its current state contains the following:

- **Completed refactor proof.** The legacy-to-Quarto migration and parity machinery has been frozen under `archive/refactoring/` and the `archive/refactoring-complete` branch/tag workflow. Active paper and analysis sources are now edited directly.
- **Targets-based research pipeline.** [`_targets.R`](_targets.R) organizes raw-data readers, district tracking, measure construction, IV/probit models, figures, tables, diagnostics, report rendering, and application samples.
- **Audited public artifacts.** [`scripts/run_public_build_audit.sh`](scripts/run_public_build_audit.sh) normalizes source whitespace, runs tests, executes final public checks, optionally runs extended diagnostics/benchmarks, and packages `review.zip`.
- **Review archives without raw data.** [`scripts/make_review_archive.sh`](scripts/make_review_archive.sh) packages source, public outputs, and diagnostics into `review.zip` while omitting local raw data and caches.
- **Explicit diagnostics policy.** Public/build diagnostics are short-lived; extended diagnostics and benchmarking outputs are preserved unless intentionally cleaned.
- **Application-sample automation.** Writing and coding samples are generated from the same source/pipeline used for the paper rather than hand-maintained as separate PDFs.

## Repository map

- [`R/`](R/): function-based research pipeline.
- [`R/diagnostics/`](R/diagnostics/): public and extended diagnostic code.
- [`R/benchmarking/`](R/benchmarking/): opt-in benchmarking/tuning target helpers.
- [`config/`](config/): draft/final/diagnostic run settings.
- [`_targets.R`](_targets.R): dependency graph for data cleaning, modeling, diagnostics, benchmarks, figures, tables, report rendering, and application samples.
- [`paper/`](paper/): Quarto source and rendered paper output.
- [`application-samples/`](application-samples/): cover notes, sample specifications, and generated writing/coding samples.
- [`analysis/diagnostics/`](analysis/diagnostics/): lightweight diagnostic notebooks and rendered diagnostic reports.
- [`analysis/benchmarking/`](analysis/benchmarking/): opt-in benchmarking reports generated by benchmark targets.
- `outputs/diagnostics/build/`: short-lived target metadata and warnings, regenerated by audits and intentionally untracked.
- `outputs/diagnostics/public/`: short-lived public-build diagnostics, regenerated by audits and intentionally untracked.
- [`outputs/diagnostics/extended/`](outputs/diagnostics/extended/): opt-in diagnostic artifacts preserved across ordinary public builds.
- [`outputs/benchmarking/`](outputs/benchmarking/): opt-in method/timing/tuning benchmark artifacts.
- [`data/metadata/`](data/metadata/): tracked metadata, manifests, manual district corrections, checksums, and variable dictionary.
- [`data/processed/`](data/processed/): tracked processed district tracker and district panel.
- `data/raw/`, `data/raw_future/`, `data/interim/`: local-only, gitignored data folders.
- [`docs/`](docs/): methodological notes and project planning documents.
- [`archive/`](archive/): legacy drafts, rendered artifacts, source samples, and extracted legacy Rmd chunks.

## Quickstart using Makefile

```bash
make init-renv   # first local setup; creates/updates renv.lock
make test        # unit tests and input/output contracts; should pass without raw data
make pipeline-draft
make report
make samples
make check-public-draft
```

Raw data are not tracked. Place raw files according to [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv). See [`REPLICATION.md`](REPLICATION.md) for the current replication data contract and expected behavior on a fresh clone without local-only raw data.

The two public processed data products are:

- [`data/processed/district_tracker_2001_2007_2017_2020.csv`](data/processed/district_tracker_2001_2007_2017_2020.csv)
- [`data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`](data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv)

## Commands for running and auditing

Use a small number of commands repeatedly rather than trying to remember every [`Makefile`](Makefile) target.

| Use case | Command | Explanation |
|---|---|---|
| Unit-test smoke check | `make test` | Fast contract tests; should pass without local raw data. |
| Fast public audit, no samples | `make public-build-audit` | Runs the canonical audit without application samples and writes a no-samples `review.zip` on success. |
| Full reviewer-facing audit | `make public-build-audit-full` | Runs cached `{targets}` public render targets for the report, docs, and application samples, audits outputs, and writes a full `review.zip`. |
| Cache-preserving debug audit | `make public-build-audit-full-incremental-review` | Preserves generated renders and the `{targets}` cache, and writes a debug archive if the run fails. |
| Extended diagnostics only | `make extended-diagnostics` | Runs opt-in `diag_ext_*` targets, respecting the targets cache. |
| Benchmarks only | `make benchmarking` | Runs opt-in `bench_*` targets, respecting the targets cache. |
| Full audit plus diagnostics/benchmarks | `make public-build-audit-full-with-benchmarks` | Runs the full public audit, then opt-in extended diagnostics and benchmarks. Use when reviewing methodological/debug outputs, not for every edit. |

Cleaning commands are intentionally separate: `make clean-public-diagnostics`, `make clean-extended-diagnostics`, and `make clean-benchmarking` clear the corresponding output families only when you explicitly want to refresh them.

## Script variants for reviewers, LLM users, and debugging

The shell script is the canonical configurable audit. It is more flexible than the [`Makefile`](Makefile) aliases:

```bash
# macOS/Linux/Git Bash: full reviewer bundle, archive on failure, save the log.
bash scripts/run_public_build_audit.sh --with-samples --archive-on-error 2>&1 | tee full_output.txt

# Faster iterative debug run: keep caches/renders, omit application samples, archive on failure.
bash scripts/run_public_build_audit.sh --without-samples --incremental --archive-on-error 2>&1 | tee full_output.txt

# Full diagnostic/benchmarking run: expensive, but useful before methodological review.
bash scripts/run_public_build_audit.sh --with-samples --incremental --archive-on-error --with-extended-diagnostics --with-benchmarks 2>&1 | tee full_output_with_diagnostics_benchmarks.txt
```

On Windows, use WSL or Git Bash for the same commands. From PowerShell, the equivalent logging pattern is:

```powershell
bash scripts/run_public_build_audit.sh --with-samples --archive-on-error 2>&1 | Tee-Object -FilePath full_output.txt
```

From `cmd.exe`, use:

```bat
bash scripts\run_public_build_audit.sh --with-samples --archive-on-error > full_output.txt 2>&1
```

### Cheaper workflow for LLMs

If you want help changing the code but do not want to spend hundreds of dollars on a coding agent, run one of the audit commands above yourself and upload both files to the chatbot's web interface:

1. `review.zip`
2. the corresponding log file, usually `full_output.txt` or `full_output_with_diagnostics_benchmarks.txt`

`--archive-on-error` is important: successful runs create a final `review.zip`, while failed runs create an incomplete debug archive that still contains the source tree, generated diagnostics, target metadata, and enough context for an LLM to propose a patch. The no-samples/incremental variants are cheaper for iteration; the full `--with-samples` run is the better reviewer-facing proof build.


### Review archive contract

Build `review.zip` through [`scripts/run_public_build_audit.sh`](scripts/run_public_build_audit.sh) or after a final public check succeeds. The packaging script stages the current working tree, omits raw data and local caches, and normally refuses to run without the `.public-final-ok` stamp produced by a final public check. When called by the audit script with `--archive-on-error`, it can create an `--allow-incomplete` debug archive after a failed run; that archive is for diagnosis, not for reviewer submission.

For fast iteration, run `bash scripts/run_public_build_audit.sh --without-samples --archive-on-error`. This mode omits [`application-samples/output/`](application-samples/output/) from `review.zip`, so it cannot accidentally package stale sample PDFs. Before a full submission or application bundle, run `bash scripts/run_public_build_audit.sh --with-samples --archive-on-error`; that mode renders application samples and requires them in `review.zip`.

## Behavior without raw data

The pipeline should fail gracefully when required raw data are absent: it reads [`data/metadata/file_manifest.csv`](data/metadata/file_manifest.csv), checks the listed paths, and reports the exact missing files before attempting to call readers like `read_sav()`, `read_excel()`, or `sf::st_read()`. A cryptic raw-reader path error should be treated as a bug.

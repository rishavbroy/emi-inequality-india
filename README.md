# Escaping Inequality in India: English-Medium Instruction and Local Development

This repository contains the research pipeline for my independent paper, **"Escaping Inequality in India: The Role of English-Medium Instruction."**

## For pre-doc hiring committees / faculty reviewers

- Main paper source: [paper/report.qmd](paper/report.qmd)
- Rendered draft paper PDF: [paper/report.pdf](paper/report.pdf) (provisional; final checks do not yet pass)
- Writing sample PDF: [application-samples/output/RishavRoy_WritingSample.pdf](application-samples/output/RishavRoy_WritingSample.pdf) (generated from the current sample pipeline)
- Coding sample PDF: [application-samples/output/RishavRoy_CodingSample.pdf](application-samples/output/RishavRoy_CodingSample.pdf) (generated from the current sample pipeline)
- District-matching note: [docs/district-matching.qmd](docs/district-matching.qmd) and [docs/district-matching.html](docs/district-matching.html)
- Replication guide: [REPLICATION.md](REPLICATION.md)
- Data availability: [DATA_AVAILABILITY.md](DATA_AVAILABILITY.md)
- Research roadmap: [docs/plan/roadmap.md](docs/plan/roadmap.md)

## Research question

Does increasing baseline district-level exposure to English-medium instruction (EMI) in 2007–08 affect local consumption growth over the next decade?

## Empirical design

The current pipeline combines National Sample Survey microdata, 2001 Census mother-tongue data, district-boundary data, and district-change trackers to construct a district pseudo-panel. The current 2SLS design instruments district-level EMI exposure with a population-weighted measure of linguistic distance from Hindi.

The project is under active revision. The main methodological priorities are district harmonization, geography controls, state fixed effects or state-demeaned IV specifications, spatial diagnostics, and a first-difference 2SLS redesign.

## Repository map

- `R/`: function-based research pipeline.
- `config/`: draft/final/diagnostic run settings.
- `_targets.R`: dependency graph for data cleaning, modeling, diagnostics, figures, tables, report rendering, and application samples.
- `paper/`: Quarto source and rendered paper output.
- `data/metadata/`: tracked metadata, source manifests, manual district corrections, and variable dictionary.
- `data/processed/`: tracked processed district tracker and district panel.
- `data/raw/`, `data/raw_future/`, `data/interim/`: local-only, gitignored data folders.
- `docs/`: polished methodological notes.
- `docs/plan/roadmap.md`: converted project planning roadmap.
- `analysis/diagnostics/`: diagnostic notebooks whose outputs are generated from the pipeline.
- `application-samples/`: cover notes, sample specifications, and generated writing/coding samples for pre-doc applications.
- `archive/`: legacy drafts, source samples, and old rendered files.

## Quickstart

```bash
make init-renv   # first local setup; creates/updates renv.lock
make test        # smoke tests and input/output contracts
make pipeline-draft
make report
make samples
make check-public-draft
```

Raw data are not tracked. Place raw files according to the canonical paths in `data/metadata/file_manifest.csv`. If your local raw folders still use the older long names, run `Rscript scripts/rename_raw_data_from_manifest.R` first for a dry-run rename plan; use `--execute` only after reviewing that plan. See `REPLICATION.md` for the current replication data contract and expected behavior on a fresh clone without local-only raw data.

`make check-public-draft` verifies that the current draft renders without scaffold or fallback prose. `make check-public-final` is stricter: it runs the final config and fails on unresolved report cross-references or placeholder-valued legacy inline quantities that still need final data/model outputs.

The two public processed data products are:

- `data/processed/district_tracker_2001_2007_2017_2020.csv`
- `data/processed/district_panel_emi_consumption_2001_2007_2017_2020.csv`

## Missing-data behavior

The pipeline should fail gracefully when required raw data are absent: it reads `data/metadata/file_manifest.csv`, checks the listed paths, and reports the exact missing files before attempting to call readers like `read_sav()`, `read_excel()`, or `sf::st_read()`. A cryptic raw-reader path error should be treated as a bug.

## Current limitations

The current estimates should be treated as provisional. Active issues include district harmonization, geographic controls, state fixed effects/state-demeaned IVs, spatial autocorrelation, migration, inflation/local price changes, and the transition from a simple consumption-percent-change response variable to log consumption differences.

See `docs/district-matching.qmd` for the district-harmonization plan.

# Architecture

This repository builds the EMI and inequality paper, diagnostics, application samples, and replication artifacts. Current source files are authoritative; historical refactor evidence remains under `archive/refactoring/` and is not active build machinery.

## Project structure

- `_targets.R` — thin orchestration and target-group selection.
- `R/io/` — raw-data readers and path handling.
- `R/districts/` — district identities, lineage, crosswalks, and panel construction contracts.
- `R/measures/` — analysis measures and survey-weighted aggregation.
- `R/iv/` and `R/selection/` — estimation logic; `R/iv/specification_registry.R` is the shared contract for extended IV specifications and diagnostic applicability.
- `R/diagnostics/` — public and extended diagnostics.
- `R/benchmarking/` — optional benchmarks.
- `R/output/` — figures, tables, shared table contracts, report values, and render helpers.
- `paper/`, `docs/`, `analysis/` — current prose and rendering sources.
- `tests/testthat/` — behavioral and output-contract tests.
- `archive/refactoring/` — historical proof only.

## Dependency layers

1. Readers normalize raw data and tracked metadata.
2. District lineage maps both NSS waves to Census-2001 districts.
3. Measures and pooled household records construct the three panel variants.
4. The public alias selects the reviewed primary panel.
5. Estimation and diagnostics consume the public alias or an explicitly named comparison panel.
6. Output modules generate tables, figures, report values, and rendered documents.

Reusable logic belongs in `R/`; `_targets.R`, scripts, and Make targets should coordinate rather than duplicate it. Public QMDs should contain prose and small rendering calls only.

## District-panel roles

- `district_panel_conservative`: 408 deterministic districts.
- `district_panel_primary`: 573 reviewed districts and the public production specification.
- `district_panel_full_reviewed`: 587-district fractional-allocation sensitivity specification.
- `district_panel`: public alias to `district_panel_primary`.
- `district_panel_legacy`: inherited harmonization panel used only in historical comparison diagnostics.

The lineage object uses matching names: `conservative_source_crosswalk`, `primary_source_crosswalk`, and `full_reviewed_source_crosswalk`.

## Production and legacy separation

`core_pipeline_targets` contains only production dependencies. Legacy boundaries, the inherited harmonization crosswalk, and district tracker inputs live in `legacy_geography_targets`; the inherited legacy panel and archived review ledger live in `legacy_comparison_targets`. The geography group is available to extended diagnostics and benchmarks, while the legacy panel is constructed only for extended historical comparisons.

The production lineage does not load `data/metadata/district_legacy_mapping_reviews.csv`, does not evaluate migration gates, and does not depend on the inherited panel.

## Output contracts

`R/output/table_contract.R` is the single source of public table captions and notes. It is sourced both by the targets table writer and by standalone Quarto helpers. Generated CSVs retain machine-readable schemas; public rendering applies presentation labels without changing stored data.

All public models, tables, maps, diagnostics, processed data, paper outputs, poster outputs, and application samples depend on the generic production targets rather than an implementation-specific comparison target.

## Strict final mode

`config/final.yml` enables strict district-panel and analysis-panel validation for production panels. Strict validation stops on error-severity panel issues, including duplicated production panel units, but retains warning-severity source-key reuse for documented split/merge allocations. Final builds also fail on incomplete analysis rows, placeholder model output, missing report values, unresolved cross-references, or missing required artifacts. The inherited legacy panel is exempt from production uniqueness gates only in its optional historical-comparison target because inherited duplicate split/merge rows are themselves an object of that review.

## Target groups

- `core_pipeline_targets` — production data, models, outputs, and documents.
- `legacy_geography_targets` — inherited geometry and harmonization inputs shared by optional diagnostics and benchmarks.
- `legacy_comparison_targets` — the inherited legacy panel, archived historical reviews, and crosswalk comparisons used only by extended diagnostics.
- `extended_diagnostic_targets` — three-panel, lineage, missingness, spatial, and matching diagnostics.
- `benchmark_targets` — optional benchmarks.
- `analysis_note_targets` and `application_sample_targets` — optional rendered derivatives.

Legacy geography is appended once when either extended diagnostics or benchmarks are enabled.

## Build philosophy

`{targets}` is the build source of truth. Durable computation should be represented by functions and explicit targets. Avoid untracked side effects, compatibility aliases, source-text tests, and parallel implementations of the same contract. Tests should protect behavioral and methodological invariants.

Update this document when target groups, panel roles, public-output contracts, directory ownership, or strict validation rules change.


## Public map regions

The public region map uses the Reserve Bank of India six-region classification,
not the former five-way ad hoc grouping. The categories are `Northern`,
`North Eastern`, `Central`, `Eastern`, `Western`, and `Southern`.
`panel_state_region_crosswalk()` is the single state-to-region contract used by
panel construction and map preparation. Historical state names are resolved
through the project's canonical state aliases before classification. The small
union territories omitted from some RBI state lists follow the corresponding
RBI/Zonal-Council geography used by the project: Delhi, Chandigarh, and Jammu
and Kashmir are Northern; Andaman and Nicobar Islands is Eastern; and
Puducherry and Lakshadweep are Southern.

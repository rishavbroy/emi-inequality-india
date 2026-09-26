# Architecture

This document explains how the active repository is organized. Domain-specific scientific choices are documented in the corresponding methodological files under `docs/`.

## Design principles

The code follows four main rules:

- define each scientific construct or finite set of specifications once and reuse it;
- normalize input schemas before measure construction or estimation;
- separate data construction, estimation, validation, and presentation; and
- keep reusable computation in `R/` functions while `{targets}` and scripts declare dependencies and execution order.

Intermediate files are retained when they are useful research summaries, publication inputs, or review records. Final builds stop on invalid inputs, failed invariants, or inconsistent results. Tests emphasize observable behavior, schemas, and methodological invariants.

Historical refactor evidence remains under [`archive/refactoring/`](../archive/refactoring/) and is not active build machinery.

## Repository layers

| Layer | Main locations | Responsibility |
|---|---|---|
| Configuration and metadata | `config/`, `data/metadata/` | Scientific configuration, file registries, construct/design metadata, reviewed crosswalks |
| Input adapters | `R/io/`, `R/clean/` | Read raw/tracked inputs and normalize source-specific schemas |
| Measure construction | `R/measures/`, `R/prices/`, `R/controls/` | Construct analysis variables, survey aggregates, prices, and baseline controls |
| Geography and lineage | `R/districts/` | District identities, lineage evidence, crosswalks, and geographic harmonization |
| Estimation and inference | `R/selection/`, `R/iv/` | Statistical models, registered specifications, weak-identification inference |
| Validation and benchmarking | `R/diagnostics/`, `R/benchmarking/` | Scientific checks, robustness analyses, optional performance comparisons |
| Presentation | `R/output/`, `R/application_samples/` | Tables, figures, report values, manuscript/sample rendering helpers |
| Execution | `_targets.R`, `_targets_processed.R`, `R/pipeline/`, `scripts/`, `Makefile` | Declare dependencies and execution order |
| Publication | `paper/`, `application-samples/`, `posters/` | Documents that consume generated results |

Source directories are loaded with `{targets}`' native `tar_source()` support. The main entry points rely on that mechanism.

## Analysis entry points

[`_targets.R`](../_targets.R) is the primary entry point. It loads reusable modules and target definitions for:

- the core paper/release analysis;
- optional extended diagnostics;
- optional benchmarks;
- application-sample rendering; and
- optional poster rendering.

The environment switches `EMI_RUN_EXTENDED_DIAGNOSTICS`, `EMI_RUN_BENCHMARKS`, `EMI_RENDER_APPLICATION_SAMPLES`, and `EMI_RENDER_POSTER` control whether those optional families are included. Scientific configuration remains in `config/final.yml` or `config/fast.yml`.

[`_targets_processed.R`](../_targets_processed.R) is a smaller entry point for district-level replication from tracked processed inputs. It uses its own `_targets_processed/` store and covers the analyses supported by those inputs.

Target definitions under `R/pipeline/` declare dependencies. Reusable domain computation belongs in independently testable `R/` functions. Moving declarations between files should preserve target names and commands unless the underlying scientific dependency changes.

## Shared definitions

The repository uses tracked tables for scientific choices that form a finite, reviewable set. Important definitions include:

- `data/metadata/file_manifest.csv` for required local files;
- `data/metadata/variable_dictionary.csv` for shared construct semantics;
- `data/metadata/census_2001_control_registry.csv` for 2001 Census control definitions;
- `data/metadata/iv_candidate_designs.csv` plus the IV registries for allowed candidate specifications;
- consumption survey/outcome registries for welfare concepts and endpoint use;
- district-lineage review metadata for geographic identity decisions; and
- `application-samples/samples.yml` for application-sample selections.

`R/iv/analysis_design_registry.R` combines the specialized analysis registries into a common table of designs. It references the construct and specification definitions already used by estimation.

When adding a finite robustness family, define the scientific dimensions and allowed combinations once and have estimation, tables, and checks reuse that definition.

## Geography roles

The repository distinguishes three concepts that should not be conflated:

1. **Source geography** — the native geography of a survey, Census table, administrative file, or boundary release.
2. **Reference analysis geography** — 2001 Census districts used by the principal district analyses.
3. **Alternative harmonized geography** — conservative, full reviewed, historical, or other explicitly named specifications used for robustness or validation.

`district_panel_primary` is the reviewed main district panel and `district_panel` is the alias used elsewhere in the project. Alternative panels retain explicit names so analyses can identify which geography they use.

District identity evidence and the rules used to transform observations across vintages are documented separately in [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md) and [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md).

## Main and optional analyses

A target belongs in the main build when a paper or release result depends on it. Additional validation analyses remain available for scientific review. Benchmarks compare implementation choices or performance.

Whether an analysis is part of the main build is independent of whether its raw inputs can be redistributed.

If an additional validation result becomes a manuscript input, include its existing dependency in the paper build. A check can remain available for validation even after its table or figure leaves the paper.

## Output conventions

- Cached target values are the default home for intermediate calculations.
- Machine-readable CSVs retain analytical schemas and identifiers.
- Presentation labels and formatting are applied downstream in `R/output/`.
- A persisted validation file should be a scientific summary, a document input, or an independently useful review record.
- Multi-file writers should return their emitted paths to `{targets}` as file targets so incremental builds track the files themselves.

`R/output/table_contract.R` centralizes paper table captions and notes used by target writers and QMD rendering helpers. After the selected steps finish, the final audit writes `outputs/build/output_manifest.csv`, which lists generated files. Scientific definitions remain in their corresponding metadata and registries.

Generated publication PDFs are tracked intentionally. Raw data, dependency libraries, target stores, and local caches are not publication outputs.

## Final and optional execution modes

`config/final.yml` is the release configuration. `config/fast.yml` is for iteration and may reduce expensive validation work, but it should not redefine the registered estimands or variable semantics.

These optional steps can be enabled independently:

- extended validation adds robustness checks;
- benchmarks add performance/implementation comparisons;
- application samples render selected paper/code excerpts; and
- the conference poster adds poster-only rendering dependencies.

See [`BUILD.md`](BUILD.md) for the command and flag reference.

## Error and status conventions

Required inputs, invalid schemas, failed accounting identities, and inconsistent final results stop the selected build. The build does not substitute a different estimator or specification.

Optional analyses may return explicit status and reason rows when a design does not apply. Those rows distinguish inapplicable designs from computational failures and are excluded from estimate summaries.

Static maintenance reports such as `outputs/build/source_health.csv` are advisory because R's dynamic dispatch can obscure whether every function is reachable. Scientific and output-structure checks remain build failures.

## Where new work belongs

Use the narrowest existing layer that owns the responsibility:

| Change | Preferred location |
|---|---|
| New raw format or input reader | `R/io/` |
| Source-specific cleaning/normalization | `R/clean/` or the relevant input adapter |
| New constructed variable or survey aggregate | `R/measures/` |
| Price/deflator construction | `R/prices/` |
| Baseline-control construction | `R/controls/` |
| District identity/linkage rule | `R/districts/` |
| New IV specification/inference method | `R/iv/` |
| Selection-model logic | `R/selection/` |
| Scientific diagnostic | `R/diagnostics/` |
| Performance comparison | `R/benchmarking/` |
| Figure/table/report-value formatting | `R/output/` |
| Application-sample assembly | `R/application_samples/` |
| Target declaration only | `R/pipeline/` |
| Build/maintenance entry point | `scripts/` or `Makefile` |
| Methodological rationale | the corresponding domain document under `docs/` |

Before adding a helper, search the shared modules and registries for an existing definition. New configuration flags or specification dimensions should extend an existing registry when that registry already owns the choice.

## Documentation ownership

Documentation follows the same separation of concerns:

- [`../README.md`](../README.md) orients first-time readers;
- [`../REPLICATION.md`](../REPLICATION.md) describes supported replication paths and required environment/data setup;
- [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md) records access and redistribution status;
- [`BUILD.md`](BUILD.md) documents commands and build options;
- this file documents structural ownership;
- domain documents explain scientific constructions and inferential boundaries; and
- [`plan/roadmap.md`](plan/roadmap.md) should contain unfinished work rather than implementation history.

The pre-rewrite architecture/status narrative is retained under [`../archive/refactoring/docs/architecture-before-documentation-rewrite.md`](../archive/refactoring/docs/architecture-before-documentation-rewrite.md) for historical reference only.

## Related documentation

- [`BUILD.md`](BUILD.md) — execution modes and build sequence.
- [`../REPLICATION.md`](../REPLICATION.md) — replication paths and prerequisites.
- [`../data/metadata/README.md`](../data/metadata/README.md) — metadata file roles and editing rules.
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md) — district identity evidence and adjudication.
- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md) — transformation across geographic definitions.
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md) — IV design, weak-identification, and robustness methodology.
- [`EDUCATION_SELECTION.md`](EDUCATION_SELECTION.md) — selection-model methodology.
- [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md), [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md), and [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md) — consumption construction, price adjustment, and the consumption analysis used in the paper.
- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md) and [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md) — inherited linguistic conditions and EMI measurement.
- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md), [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md), and [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md) — historical validation layers.

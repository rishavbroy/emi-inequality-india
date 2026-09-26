# Architecture

This document describes where responsibilities belong in the active repository. It is a structural guide, not a record of research chronology or a catalog of empirical results. Domain-specific scientific choices belong in the corresponding methodological documents under `docs/`.

## Design principles

The active code follows a small set of structural rules:

- define each scientific construct or finite design family once and reuse that definition downstream;
- normalize source-specific schemas at the input boundary before estimation code sees them;
- keep data construction, estimation, diagnostics, and presentation in separate modules;
- use registries for finite analysis families instead of parallel hand-written specification branches;
- keep orchestration in `{targets}` factories and scripts while reusable computation remains in `R/` functions;
- persist intermediate files only when they are independently useful scientific summaries, publication inputs, or forensic review records;
- let final/release mode fail on invalid scientific states instead of silently changing the estimator or design; and
- test observable behavior, schemas, and methodological invariants rather than source layout or implementation text unless layout itself is a release requirement.

Historical refactor evidence remains under [`archive/refactoring/`](../archive/refactoring/) and is not active build machinery.

## Repository layers

| Layer | Main locations | Responsibility |
|---|---|---|
| Configuration and metadata | `config/`, `data/metadata/` | Scientific configuration, file registries, construct/design metadata, reviewed crosswalks |
| Input adapters | `R/io/`, `R/clean/` | Read raw/tracked inputs and normalize source-specific schemas |
| Measure construction | `R/measures/`, `R/prices/`, `R/controls/` | Construct analysis variables, survey aggregates, prices, and baseline controls |
| Geography and lineage | `R/districts/` | District identities, lineage evidence, crosswalks, and geographic harmonization |
| Estimation and inference | `R/selection/`, `R/iv/` | Statistical models, registered specifications, weak-identification inference |
| Diagnostics | `R/diagnostics/`, `R/benchmarking/` | Scientific validation, robustness checks, optional performance comparisons |
| Presentation | `R/output/`, `R/application_samples/` | Tables, figures, report values, manuscript/sample rendering helpers |
| Composition | `_targets.R`, `_targets_processed.R`, `R/pipeline/`, `scripts/`, `Makefile` | Declare and execute dependency structure; do not duplicate scientific logic |
| Publication | `paper/`, `application-samples/`, `posters/` | Reader-facing documents that consume generated results |

Source directories are loaded with `{targets}`' native `tar_source()` support. The composition roots should not maintain a second source-file discovery mechanism.

## Target composition

[`_targets.R`](../_targets.R) is the primary composition root. It sources reusable modules and target-family factories, then assembles:

- the core paper/release analysis;
- optional extended diagnostics;
- optional benchmarks;
- application-sample rendering; and
- optional poster rendering.

The environment switches `EMI_RUN_EXTENDED_DIAGNOSTICS`, `EMI_RUN_BENCHMARKS`, `EMI_RENDER_APPLICATION_SAMPLES`, and `EMI_RENDER_POSTER` control whether those optional families are included. Scientific configuration remains in `config/final.yml` or `config/fast.yml`.

[`_targets_processed.R`](../_targets_processed.R) is a separate, smaller composition root for district-level replication from tracked processed inputs. It uses its own `_targets_processed/` store and deliberately excludes source reconstruction and the individual-level selection model.

Target factories under `R/pipeline/` declare dependencies. They should not contain domain computation that could be called and tested independently. Moving declarations between factories should preserve target names and commands unless the scientific dependency itself changes.

## Registries and semantic authorities

The repository uses tracked registries where a scientific family is finite and reviewable. Important authorities include:

- `data/metadata/file_manifest.csv` for required local files;
- `data/metadata/variable_dictionary.csv` for shared construct semantics;
- `data/metadata/census_2001_control_registry.csv` for Census-2001 control definitions;
- `data/metadata/iv_candidate_designs.csv` plus the IV execution registries for bounded candidate/design governance;
- consumption survey/outcome registries for welfare concepts and endpoint use;
- district lineage/adjudication metadata for reviewed geographic identity decisions; and
- `application-samples/samples.yml` for application-sample selections.

`R/iv/analysis_design_registry.R` projects specialized execution registries onto a common cross-family design schema. The projection is an inventory layer: it should reference authoritative construct/specification definitions rather than create a neighboring set of formulas or labels.

When adding a new finite robustness family, declare the scientific dimensions and admissibility first, then make estimation consume that same declaration. Do not create a second specification grid only for output formatting or diagnostics.

## Geography roles

The repository distinguishes three concepts that should not be conflated:

1. **Source geography** — the native geography of a survey, Census table, administrative file, or boundary release.
2. **Reference analysis geography** — Census-2001 districts used by the principal district analyses.
3. **Alternative harmonized geography** — conservative, full-reviewed, historical, or other explicitly named specifications used for robustness or validation.

`district_panel_primary` is the reviewed production district panel and `district_panel` is its public alias. Alternative panels retain explicit names so a robustness analysis cannot silently replace the production geography.

District identity evidence and the rules used to transform observations across vintages are documented separately in [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md) and [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md).

## Core, extended, and benchmark boundaries

A target belongs in the core graph when a paper/release result depends on it. Extended diagnostics remain available for scientific review without becoming publication dependencies merely because they exist. Benchmarks evaluate implementation choices or performance and are not scientific results.

This boundary is semantic rather than a statement about whether the raw input is redistributable. The repository does not redistribute many inputs used by core work.

If an extended result becomes a manuscript input, promote the analytical dependency rather than reimplementing it in the presentation layer. Conversely, removing a table or figure from the paper does not require deleting the underlying scientific diagnostic if it remains useful for validation.

## Output conventions

The repository follows an **objects first, files last** rule.

- Cached target values are the default home for intermediate calculations.
- Machine-readable CSVs retain analytical schemas and identifiers.
- Presentation labels and formatting are applied downstream in `R/output/`.
- A persisted diagnostic file should be a scientific summary, a document input, or an independently useful review record.
- Multi-file writers should return their emitted paths to `{targets}` as file targets so incremental builds track the files themselves.

`R/output/table_contract.R` centralizes paper table captions/notes used by both target writers and QMD rendering helpers. The final audit writes `outputs/build/output_manifest.csv` from target metadata and the filesystem after the selected build families finish; the manifest is a discoverability index, while scientific design semantics remain authoritative in their registries.

Generated publication PDFs are tracked intentionally. Raw data, dependency libraries, target stores, and local caches are not publication outputs.

## Final and optional execution modes

`config/final.yml` is the release scientific configuration. `config/fast.yml` is for iteration and may reduce expensive validation work, but it should not redefine the registered estimands or variable semantics.

Optional build families are orthogonal to that configuration:

- extended diagnostics add scientific robustness/forensic checks;
- benchmarks add performance/implementation comparisons;
- application samples render selected paper/code excerpts; and
- the conference poster adds poster-only rendering dependencies.

See [`BUILD.md`](BUILD.md) for the human-facing command and flag reference.

## Error and status conventions

Required core inputs, invalid schemas, impossible accounting identities, and invalid final scientific states should fail the selected build rather than trigger silent estimator or specification substitution.

Optional analyses may return explicit status/reason rows when non-applicability is itself a meaningful result. Those schemas must distinguish expected non-applicability from computational failure, and downstream summaries must not treat status rows as estimates.

Static maintenance reports such as `outputs/build/source_health.csv` are advisory where R's dynamic dispatch prevents proof of reachability. Hard scientific or output-structure checks remain build failures.

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

Before adding a helper, search the shared modules and registries for an existing semantic authority. Before adding a new configuration flag or specification dimension, check whether it belongs in an existing registry rather than another branch of control flow.

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
- [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md), [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md), and [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md) — consumption construction, price adjustment, and paper-facing consumption analysis.
- [`LINGUISTIC_DISTANCE.md`](LINGUISTIC_DISTANCE.md) and [`EMI_MEASUREMENT.md`](EMI_MEASUREMENT.md) — inherited linguistic conditions and EMI measurement.
- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md), [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md), and [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md) — historical validation layers.

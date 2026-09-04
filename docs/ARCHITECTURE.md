# Architecture

This repository builds the EMI and inequality paper, diagnostics, application samples, and replication artifacts. Current source files are authoritative; historical refactor evidence remains under `archive/refactoring/` and is not active build machinery.

## Project structure

- `_targets.R` — composition root and target-group selection.
- `R/pipeline/` — target-family factories used by the composition root; these files declare target objects only and contain no statistical or data-construction logic. `core_consumption_targets.R` owns registered consumption ingestion, reconstruction validation, source-district identity attachment, and household deflation; `core_consumption_outcome_targets.R` owns lineage harmonization, welfare aggregation, and outcome/specification registries; `core_consumption_iv_targets.R` owns the production consumption-IV panel, coverage gate, and dynamic estimates. Extended factories own optional diagnostic families.
- `R/io/` — raw-data readers and path handling.
- `R/districts/` — district identities, lineage, crosswalks, and panel construction contracts.
- `R/measures/` — analysis measures and survey-weighted aggregation.
- `R/iv/` and `R/selection/` — estimation logic; `R/iv/specification_registry.R` remains the execution contract for IV specifications, while `R/iv/analysis_design_registry.R` projects specialized registries onto one cross-family design ontology without generating a Cartesian specification search.
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
Source directories are loaded with `{targets}`' native `tar_source()` directory support, which recursively sources only `.R`/`.r` scripts; the composition root should not maintain a parallel file-discovery wrapper.


## Analysis-design ontology

`R/iv/analysis_design_registry.R` is an inventory layer above the specialized
execution registries. It normalizes currently implemented public headline IV,
consumption, DISE, Census mechanism, C-17, district mechanism, labor, firm, and
historical first-stage designs onto explicit outcome, treatment, instrument,
vintage, adjustment, estimand, estimator, inference, sample-rule, role, and
admissibility fields. Public headline models are themselves declared by
`public_iv_specification_registry()` and converted to formulas only at the final
legacy model-interface boundary, so there is no second headline formula authority.
The separate candidate-design ledger records bounded robustness families,
unimplemented but theoretically admissible extensions, and explicit non-goals
without manufacturing or estimating an indiscriminate Cartesian product. New
design families should first declare why a combination is scientifically
admissible before adding another estimator branch. The implemented analysis-design
registry remains a cached R target; the compact candidate ledger is persisted as
reviewer-facing extended diagnostic metadata.

The Census-2001 control registry is likewise metadata-first. Main, absorption,
appendix, block-membership, labels, and alternative-measure relationships are
derived from `data/metadata/census_2001_control_registry.csv`; production targets
track that file explicitly.

The extended IV pipeline owns and persists the compiled cross-family ontology at `outputs/diagnostics/extended/iv/analysis_design_registry.csv`; Census-specific pipeline modules no longer own this cross-family object. Extended diagnostic artifacts must be reachable from a `diag_ext_` target because the public audit intentionally selects that prefix.

## District-panel roles

- `district_panel_conservative`: deterministic district specification.
- `district_panel_primary`: reviewed public production specification.
- `district_panel_full_reviewed`: fractional-allocation sensitivity specification.
- `district_panel`: public alias to `district_panel_primary`.
- `district_panel_legacy`: inherited harmonization panel used only in historical comparison diagnostics.

The lineage object uses matching names: `conservative_source_crosswalk`, `primary_source_crosswalk`, and `full_reviewed_source_crosswalk`.

## Production and legacy separation

`core_pipeline_targets` contains only production dependencies and composes domain target factories rather than requiring declarations to live inline. Consumption orchestration is split at real dependency boundaries: `core_consumption_target_definitions()` handles source reconstruction and deflation, `core_consumption_outcome_target_definitions()` handles lineage and welfare outcomes before panel construction, and `core_consumption_iv_target_definitions()` attaches those outcomes to the finalized district panel and estimates the registered dynamic IV specifications. The remaining production graph is similarly partitioned into measurement (`core_measurement_target_definitions()`), reviewed district lineage (`core_lineage_target_definitions()`), analysis-panel construction (`core_panel_target_definitions()`), and public estimation/rendering (`core_public_target_definitions()`).

Historical geography validation, Vanneman source QA, multivintage geography comparisons, and their `diag_ext_*` artifacts are extended-diagnostic work rather than public-production dependencies. They therefore live in `extended_historical_target_definitions()` and are selected only when extended diagnostics are enabled. The canonical reviewed lineage objects they consume remain in the core graph, including `district_lineage_sources`, `district_lineage`, and the shared `district_transition_2001_2011` alias. This keeps the strict public graph lean without deleting any historical object or forensic artifact from the full audit. Legacy boundaries, the inherited harmonization crosswalk, and district tracker inputs live in `legacy_geography_targets`; the inherited legacy panel and archived review ledger live in `legacy_comparison_targets`. The geography group is available to extended diagnostics and benchmarks, while the legacy panel is constructed only for extended historical comparisons. Extended diagnostics are grouped into historical, lineage/general-diagnostic, Census, DISE/mechanism, and IV/control target factories under `R/pipeline/`; `extended_diagnostic_target_definitions()` composes those families, while `_targets.R` remains responsible for deciding whether the combined family enters the selected graph.

The production lineage does not load `data/metadata/district_legacy_mapping_reviews.csv`, does not evaluate migration gates, and does not depend on the inherited panel.

## Output contracts

`R/output/table_contract.R` is the single source of public table captions and notes. It is sourced both by the targets table writer and by standalone Quarto helpers. Generated CSVs retain machine-readable schemas; public rendering applies presentation labels without changing stored data.

The repository follows an **objects first, artifacts last** retention rule. Cached target objects are the default home for intermediate calculations. A persisted diagnostic should have one of three roles: scientific summary, public/report input, or independently useful forensic QA ledger. Benchmark targets persist benchmark-specific results and reuse canonical diagnostic inputs instead of copying them. Comparison savers should likewise avoid re-serializing unchanged shared inputs under multiple prefixes.
Post-treatment mechanism inference follows the same retention rule through `save_posttreatment_mechanism_outputs()`: registered source/sample summaries, reduced forms, and compact weak-IV/Anderson--Rubin summaries are persisted, while pointwise Anderson--Rubin inversion grids remain cached target objects. `outputs/diagnostics/extended/mechanisms/` contains the cross-family evidence grid and family summary assembled from the common inference contract; source-specific modules remain authoritative for measurement.

All public models, tables, maps, diagnostics, processed data, paper outputs, poster outputs, and application samples depend on the generic production targets rather than an implementation-specific comparison target.

## Census mechanism diagnostics

Census migration, worker, housing/living-standard, and household-mechanism modules share the administrative-count harmonization layer in `R/measures/census_admin_counts.R`. Census-2011 counts are aggregated to Census-2001 parents only after the common complete deterministic transition rule has certified full parent reconstruction; all ratios are computed after count pooling.

The housing module keeps source decoding in `R/io/read_census_housing.R`, measure construction in `R/measures/build_census_housing.R`, and output-only QA in `R/diagnostics/diagnose_census_housing.R`. It is an extended diagnostic dependency, not a preferred-model control dependency. H-05/H-08/H-10/H-11 and HL-04/HL-06/HL-08/HL-09/HL-10 reuse that same layer: source-table room, water, sanitation/drainage, and kitchen/fuel partitions are validated before pooling; 2001 H-05's incomplete district coverage is carried as variable-specific missing baseline support; H-10 replaces H-12 as the complete baseline latrine source while H-12 remains an overlap cross-check; and richer 2011 categories are collapsed only to explicit 2001 counterparts.

The household-capacity branch follows the same boundary: `R/io/read_census_households.R` decodes and validates 2001 HH-09/HH-13/HH-15(/Appendix) and 2011 HH-08/HH-10/HH-11 accounting; `R/measures/build_census_households.R` reconciles independent household universes, pools only 2011 counts through the shared deterministic transition, and differences only exact common concepts; `R/diagnostics/diagnose_census_households.R` persists the two vintages, change coverage, and source reconciliations. Open-ended or nonmatching worker concepts are excluded rather than imputed. HH variables remain post-treatment descriptors and do not enter preferred controls or a new estimator by default.

## Strict final mode

`config/final.yml` enables strict district-panel and analysis-panel validation for production panels. Strict validation stops on error-severity panel issues, including duplicated production panel units, but retains warning-severity source-key reuse for documented split/merge allocations. Final builds also fail on incomplete analysis rows, placeholder model output, missing report values, unresolved cross-references, or missing required artifacts. The inherited legacy panel is exempt from production uniqueness gates only in its optional historical-comparison target because inherited duplicate split/merge rows are themselves an object of that review.

## Target groups

- `core_pipeline_targets` — production data, models, outputs, and documents, composed from focused measurement, lineage, panel, public-output, and consumption factories at their true dependency boundaries.
- `legacy_geography_targets` — inherited geometry and harmonization inputs shared by optional diagnostics and benchmarks.
- `legacy_comparison_targets` — the inherited legacy panel, archived historical reviews, and crosswalk comparisons used only by extended diagnostics.
- `extended_diagnostic_targets` — extended diagnostic target objects composed from the five domain-oriented factories under `R/pipeline/`; target names and dependency commands remain unchanged when orchestration is moved out of `_targets.R`.
- `benchmark_targets` — optional benchmarks.
- `analysis_note_targets` and `application_sample_targets` — optional rendered derivatives.

Legacy geography is appended once when either extended diagnostics or benchmarks are enabled.

## Build philosophy

`{targets}` is the build source of truth. Durable computation should be represented by functions and explicit targets. Target-family factories may reorganize declarations, but they must preserve target names and commands so caches, debugging metadata, `tar_traceback()`, and existing diagnostic entry points remain useful. A target belongs in the public core graph only when a public-production target depends on its value; diagnostic support objects and their persisted QA artifacts belong in the relevant optional family instead. Moving such a target out of the core is permitted only after checking the complete target-definition graph for public consumers, and the full audit must continue to select it. Tests that audit the target graph must inspect `_targets.R` together with `R/pipeline/*_targets.R`; physical-file placement is not a methodological invariant. `scripts/test_impact.py` maps each target-family file to its domain tests so modularization does not reduce failure localization. Avoid untracked side effects, compatibility aliases, source-text tests, and parallel implementations of the same contract. Tests should protect behavioral and methodological invariants.

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

## DISE geographic harmonization

Archived DISE district counts are harmonized to Census-2001 analysis units through a deterministic bridge derived from the reviewed district-lineage registry. Only exact Census-2001 identities and reviewed weight-one lineage mappings are eligible; fractional population allocations are excluded from administrative school counts. Count variables are aggregated before shares are recomputed.

## Census administrative-count harmonization

Census-2001 worker B-04/B-25/B-26 measures are read directly on the native 593-district analytical geography and enter only predetermined IV-balance diagnostics. Census-2011 count-valued outcomes that are compared on Census-2001 analytical geography use `build_complete_deterministic_transition_2011_to_2001()`. The resulting `district_transition_2001_2011` is a first-class pipeline target, rather than repeated nested access to the larger `district_lineage` object, because C-13, migration, worker, housing, and Census-population denominators all depend on the same transition. A Census-2001 parent is retained only when every contributing 2011 district is wholly and deterministically assigned to that parent. Counts are pooled before any rates or composition shares are recomputed; partial parents and fractional territorial allocations are not silently treated as complete administrative totals. Migration D-02 through D-07 and worker B-04/B-06/B-25A/B additionally enforce source-level accounting identities before harmonization, so geography pooling cannot conceal a malformed source table. The Census-2011 all-age population denominator is read from SHRUG's district PCA and pooled through this same count contract before D-02 population rates are formed. The resulting harmonized source universe is a measurement object, not an econometric sample: downstream diagnostic branches intersect it explicitly with the canonical analysis panel and audit that attrition separately.

Post-treatment district mechanism regressions use the shared post-treatment mechanism engine in `R/diagnostics/posttreatment_mechanism_inference.R`. Each module supplies a small outcome registry and harmonized source frames; the engine then audits source-to-IV-panel overlap, fixes one common support sample across the registered outcomes and six scalar-IV candidate designs, estimates reduced forms, and reuses the canonical weak-IV/Anderson-Rubin estimators. Conventional 2SLS p-values are retained for scale but Anderson-Rubin inference is the primary weak-identification safeguard. Migration D-02/D-03/D-04/D-07 outcomes and the full-support longitudinal housing-change registry use this contract. These post-treatment outcomes are mechanism diagnostics, not controls or automatically identified mediation effects.


Census 2001 H-04 Appendix (`PC01_H04a`) and Census 2011 HL-13 structural-durability counts reuse the shared count/geography path. H04A was activated only after all 35 raw workbooks passed structural accounting and their 593 district household totals matched H-09 exactly. Longitudinal durability changes are now available descriptively and are not imputed from H-03 materials or added to the fixed weak-IV housing registry.

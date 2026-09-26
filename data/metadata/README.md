# Data metadata

This directory contains tracked registries, reviewed crosswalks, acquisition manifests, and validation metadata used by the active analysis. The files here should describe stable data/design facts rather than duplicate methodological prose from `docs/`.

## General files

| File | Role |
|---|---|
| `file_manifest.csv` | Required/local input registry used by source preflight, including expected paths and optional byte/hash identities |
| `data_sources.csv` | Source catalog and acquisition/reference notes, including optional/review inputs |
| `checksums.csv` | SHA-256 registry for tracked metadata files |
| `variable_dictionary.csv` | Shared analysis-construct semantics for variables in the common panel architecture |
| `iv_candidate_designs.csv` | Declarative candidate-design/admissibility ledger for the bounded IV program |

Raw-data access and redistribution belong in [`../../DATA_AVAILABILITY.md`](../../DATA_AVAILABILITY.md); execution requirements belong in [`../../REPLICATION.md`](../../REPLICATION.md).

## Census acquisition and controls

- `census_1991_download_manifest.tsv`
- `census_2001_download_manifest.tsv`
- `census_2011_download_manifest.tsv`
- `census_2001_control_registry.csv`

The download manifests define maintained acquisition destinations and source URLs. The control registry is the authority for Census-2001 control membership, labels, blocks, and alternative relationships used by the analysis.

See the Census domain documents under `docs/` for table-specific construction and comparability rules.

## District lineage and geography

- `district_lineage_sources.csv` — reviewed lineage-source registry;
- `district_admin_events.csv` — administrative transition/event records;
- `district_adjudications.csv` and `district_primary_reviews.csv` — reviewed identity decisions;
- `district_match_gold.csv` — reviewed matching reference set;
- `district_allocation_weights.csv` — reviewed fractional allocations where the named specification permits them;
- `district_geometry_carrybacks.csv` — reviewed geometry carryback decisions;
- `district_harmonization_crosswalk.csv` — tracked harmonization authority used by processed replication;
- `district_legacy_mapping_reviews.csv` and `manual_district_corrections.csv` — historical/legacy comparison support;
- `map_disputed_areas.csv` — display classifications for manuscript cartography.

See [`../../docs/DISTRICT_LINEAGE.md`](../../docs/DISTRICT_LINEAGE.md) and [`../../docs/GEOGRAPHY_HARMONIZATION.md`](../../docs/GEOGRAPHY_HARMONIZATION.md).

## Consumption and prices

Consumption metadata separate source identity, welfare concepts, registered analysis endpoints, comparison checks, and price construction:

- `consumption_survey_registry.csv`
- `consumption_welfare_outcomes.csv`
- `consumption_iv_outcomes.csv`
- `consumption_welfare_comparisons.csv`
- `consumption_mpce_benchmarks.csv`
- `consumption_state_code_crosswalk.csv`
- `consumption_source_geography_special_units.csv`
- `consumption_lineage_identity_aliases.csv`
- `price_series_registry.csv`
- `price_state_crosswalk.csv`
- `cpi_iw_centres_1982.csv`
- `cpi_iw_centres_2001.csv`
- `tendulkar_poverty_lines_2011_12.csv`
- `hces_2022_24_district_codebook.csv`
- `hces_summary_items.csv`

See [`../../docs/CONSUMPTION_MEASUREMENT.md`](../../docs/CONSUMPTION_MEASUREMENT.md), [`../../docs/PRICE_DEFLATION.md`](../../docs/PRICE_DEFLATION.md), and [`../../docs/CONSUMPTION_ANALYSIS.md`](../../docs/CONSUMPTION_ANALYSIS.md).

## DISE/UDISE and EMI metadata

- `dise_archive_registry.csv` — archive/report-card source registry;
- `dise_medium_slot_crosswalk.csv` — medium-slot interpretation;
- `dise_publication_checks.csv` — registered source cells that must reproduce exactly;
- `dise_report_language_enrollment.csv`, `dise_report_total_enrollment_2010_11.csv`, and `dise_report_school_quality_2011_15.csv` — reviewed report extraction metadata;
- `english_opportunity_measures.csv` — stable EMI/opportunity construct definitions.

See [`../../docs/DISE_TREATMENTS.md`](../../docs/DISE_TREATMENTS.md), [`../../docs/EMI_MEASUREMENT.md`](../../docs/EMI_MEASUREMENT.md), and [`../../docs/LINGUISTIC_DISTANCE.md`](../../docs/LINGUISTIC_DISTANCE.md).

## Linguistic and historical metadata

- `shastry_language_distance.csv` and `shastry_language_adjudications.csv`
- `census_language_glottolog_crosswalk.csv`
- `asjp_language_index.csv`
- `lexical_language_index.csv`
- `kogan_2017_anchor_similarity.csv`
- `helms_lim_linguistic_distance_1991.csv`
- `language_atlas_1991_accepted_source.csv`
- `language_atlas_1991_cell_reviews.csv`
- `language_atlas_1991_languages.csv`
- `language_atlas_1991_state_crosswalk.csv`
- `vanneman_archive_2013_checksums.csv`
- `vanneman_panel4_dist91_adjudications.csv`
- `vanneman_panel_state_crosswalk.csv`

These files record reviewed identities, source selections, crosswalks, and validation anchors. See [`../../docs/LINGUISTIC_DISTANCE.md`](../../docs/LINGUISTIC_DISTANCE.md), [`../../docs/HISTORICAL_LANGUAGE_DATA.md`](../../docs/HISTORICAL_LANGUAGE_DATA.md), [`../../docs/HISTORICAL_GEOGRAPHY.md`](../../docs/HISTORICAL_GEOGRAPHY.md), and [`../../docs/HISTORICAL_BASELINE_VALIDATION.md`](../../docs/HISTORICAL_BASELINE_VALIDATION.md).

## Labor and source materialization

- `nesstar_conversion_contracts.csv` — reviewed external-conversion contracts for supported Nesstar containers;
- `plfs_labor_contracts.csv` — PLFS wave/source/design-field contract.

See [`../../docs/LABOR_MARKET.md`](../../docs/LABOR_MARKET.md).

## Editing rules

When changing metadata:

1. **Preserve stable IDs.** IDs referenced by code, outputs, or review histories should not be renamed merely for presentation.
2. **Keep paths repository-relative.** Local-machine absolute paths do not belong in tracked metadata.
3. **Use one authoritative file per fact.** Do not duplicate a source path, construct definition, or finite design declaration in a second CSV for convenience; project it downstream instead.
4. **Separate scientific declarations from run results.** Metadata may declare admissibility, source meaning, or expected identities. Realized estimates/counts that depend on execution belong in generated outputs unless they are reviewed external facts.
5. **Do not infer missing semantics from filenames.** Add an explicit stable ID/label/role field when downstream code needs a scientific meaning.
6. **Update dependent hashes deliberately.** After intentional tracked-metadata edits, refresh the digest registry with `Rscript scripts/update_checksums.R` when required by the checks.
7. **Keep acquisition manifests separate from core required-file status.** A downloadable source can be registered for acquisition without becoming a mandatory input to every build family.
8. **Add behavioral validation with the change.** New registries or columns should be checked at their read/compile boundary; avoid tests that merely search CSV text.

The repository's source/metadata preflight and domain readers enforce additional schema-specific invariants. Those checks, rather than this README, are the executable authority.

## Related documentation

- [`../../DATA_AVAILABILITY.md`](../../DATA_AVAILABILITY.md) — access and redistribution.
- [`../../REPLICATION.md`](../../REPLICATION.md) — local setup and supported replication paths.
- [`../../docs/ARCHITECTURE.md`](../../docs/ARCHITECTURE.md) — registry placement and module boundaries.
- [`../../docs/IV_DIAGNOSTICS.md`](../../docs/IV_DIAGNOSTICS.md) — IV candidate/design governance.
- [`../../docs/DISTRICT_LINEAGE.md`](../../docs/DISTRICT_LINEAGE.md) — lineage evidence and adjudication.

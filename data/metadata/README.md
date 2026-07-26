# Data metadata

This directory contains tracked descriptions, manifests, checksums, crosswalks, and adjudication ledgers. Raw survey, Census, LGD, SHRUG, and boundary files remain local under `data/raw/`.

## General metadata

- `data_sources.csv`: project-wide source catalog, acquisition route, local path, role, and redistribution caveat.
- `file_manifest.csv`: exact files required by the existing production pipeline. Missing required files must fail before a raw reader is called.
- `variable_dictionary.csv`: public processed-variable definitions.
- `checksums.csv`: checksums for tracked metadata and processed CSVs.

## Current production geography

- `district_harmonization_crosswalk.csv`: inherited reviewed crosswalk used by the present paper pipeline.
- `manual_district_corrections.csv`: tracked correction interface for the current pipeline.

The legacy harmonization crosswalk is retained only for historical comparison. Current public analysis uses the reviewed district-lineage crosswalks described below.

## District-lineage

- `district_lineage_sources.csv`: compact source IDs that can be cited by accepted matches, events, and weights.
- `district_match_gold.csv`: manually reviewed positive, negative, and ambiguous name-match examples used to evaluate candidate rules.
- `district_adjudications.csv`: one accepted, excluded, or needs-review source identity per source row.
- `district_admin_events.csv`: reviewed directed administrative-event edges.
- `district_allocation_weights.csv`: reviewed non-primary or sensitivity allocation shares; current accepted rows renormalize mapped population shares only when at least 99 percent of the source population is covered. `source_unit` is canonicalized as the zero-padded `SS.DDD` Census-2011 code.
- `district_geometry_carrybacks.csv`: reviewed cases where an official later-vintage polygon is carried back to an unchanged Census-2001 district.
- `district_primary_reviews.csv`: reviewed near-complete single-parent mappings admitted to the 573-district primary panel.
- `district_legacy_mapping_reviews.csv`: archived provenance for comparisons with the inherited pre-lineage panel; loaded only by extended legacy-comparison targets.

Ledgers may begin blank, but accepted rows must remain narrow, source-backed decisions. Generated candidates belong under `outputs/diagnostics/extended/district_lineage/`; they must not be copied into tracked adjudications without review.

See [`docs/DISTRICT_LINEAGE.md`](../../docs/DISTRICT_LINEAGE.md) for authority rules, source caveats, schemas, invariants, and the implementation plan.

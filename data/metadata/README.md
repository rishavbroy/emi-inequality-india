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

The current crosswalk is not proof that its geographic assignments are correct. It remains active only while district-lineage v2 is built and adjudicated in parallel.

## District-lineage v2

- `district_sources_v2.csv`: compact source IDs that can be cited by accepted matches, events, and weights.
- `district_match_gold.csv`: manually reviewed positive, negative, and ambiguous name-match examples used to evaluate candidate rules.
- `district_adjudications_v2.csv`: one accepted, excluded, or needs-review source identity per source row.
- `district_admin_events_v2.csv`: reviewed directed administrative-event edges.
- `district_allocation_weights_v2.csv`: reviewed non-primary or sensitivity allocation shares.
- `district_geometry_carrybacks_v2.csv`: reviewed cases where an official later-vintage polygon is carried back to an unchanged Census-2001 district.

Ledgers may begin blank, but accepted rows must remain narrow, source-backed decisions. Generated candidates belong under `outputs/diagnostics/extended/district_lineage_v2/`; they must not be copied into tracked adjudications without review.

See [`docs/DISTRICT_LINEAGE_V2.md`](../../docs/DISTRICT_LINEAGE_V2.md) for authority rules, source caveats, schemas, invariants, and the implementation plan.

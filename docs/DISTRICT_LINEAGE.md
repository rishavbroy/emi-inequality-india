# District Lineage System

This document describes the production district-tracking system used to place the NSS 2007–08 and NSS 2017–18 data on Census-2001 district geography.

## Production status

District lineage is part of the production pipeline. The public `district_panel` target uses `district_panel_primary`, the reviewed 573-district panel. The other two panels remain explicit comparison specifications:

| Panel | Target | Rule | Two-wave support |
|---|---|---|---:|
| Conservative | `district_panel_conservative` | Deterministic official, registry, alias, and accepted single-parent evidence | 408 |
| Primary | `district_panel_primary` | Conservative mappings plus 208 reviewed near-complete, single-parent NSS-75 mappings | 573 |
| Full reviewed | `district_panel_full_reviewed` | Primary mappings plus 21 reviewed multi-parent fractional allocations | 587 |

The full reviewed panel is a sensitivity specification. Fractional allocations do not enter public production unless future territorial evidence resolves them.

## Evidence hierarchy

Accepted lineage decisions should use the strongest available source in this order:

1. official Census or LGD identifiers and modification records;
2. official district histories, gazettes, or administrative atlases;
3. India State Stories district-change records;
4. SHRUG locality transitions and coverage;
5. published concordances or other documented secondary sources.

Names and fuzzy scores generate candidates only. They do not establish geographic continuity.

## Core invariants

The production lineage is ready only when all of the following hold:

- every NSS source identity is accepted or explicitly excluded;
- Census-2001 and Census-2011 unit identifiers are unique;
- SHRUG transition weights are finite, nonnegative, and do not overallocate a source unit;
- accepted allocation weights sum to one within source unit;
- every accepted decision cites a registered source;
- duplicate registry keys are absent or identical; repeated source district keys created by documented split/merge allocations remain warning-severity review information rather than fatal panel errors;
- every accepted identity has a conservative-panel disposition;
- every accepted identity appears in the full reviewed crosswalk or has an explicit exclusion;
- DataMeet Census-2001 geometry has no missing, unexpected, duplicate, empty, or invalid units after excluding its noncanonical 99/99 national aggregate.

These invariants are reported in `readiness.csv` and `completion_status.csv` under `outputs/diagnostics/extended/district_lineage/`.

## Crosswalk roles

The lineage bundle exposes three semantically distinct crosswalks:

- `conservative_source_crosswalk`: deterministic 408-district specification;
- `primary_source_crosswalk`: production 573-district specification;
- `full_reviewed_source_crosswalk`: 587-district fractional-allocation sensitivity specification.

The names describe analysis roles rather than implementation history. Production code should depend on `district_panel`, not directly on an implementation-specific panel target.

## Statistical aggregation

- Additive counts are summed.
- Means and shares are reconstructed from pooled numerators and denominators or pooled records.
- Gini coefficients are reconstructed from pooled household microdata; district Ginis are never averaged.
- Multi-parent allocations use tracked weights whose source-unit total is one.

All three panel variants use the same panel builder, pooled-Gini reconstruction, preferred public IV formulas, 2SLS estimator, and first-stage diagnostics. The lineage sensitivity therefore changes the geography while holding the current treatment, instrument, outcome, controls, and fixed effects fixed.

## Tracked metadata

The durable production ledgers are:

- `data/metadata/district_adjudications.csv`
- `data/metadata/district_admin_events.csv`
- `data/metadata/district_allocation_weights.csv`
- `data/metadata/district_primary_reviews.csv`
- `data/metadata/district_geometry_carrybacks.csv`
- `data/metadata/district_lineage_sources.csv`

`data/metadata/district_legacy_mapping_reviews.csv` is archived provenance. It is loaded only by `legacy_comparison_targets` during extended diagnostics and is not a production lineage input.

## Remaining bounded work

District tracking is complete for the current analysis. Remaining work is limited and explicitly queued:

- `multi_parent_review_queue.csv` contains 46 proposed target shares for 21 NSS-75 identities that require official territorial validation before primary use;
- six Census-2001 districts lack 2007–08 support and therefore cannot enter the current two-wave panel without new source evidence;
- a more authoritative code-complete Census-2001 boundary release may replace DataMeet through the same validated geometry interface.

These items do not block use of the 573-district production panel.

## Legacy comparison isolation

The inherited harmonization crosswalk and 2020 boundaries are constructed only when extended diagnostics or benchmarks require them. The legacy panel and historical review ledger are constructed only for extended historical comparisons. None is an upstream dependency of the production district panel, models, tables, maps, paper, poster, or application samples.

Historical outputs are written separately, including `legacy_crosswalk_comparison.csv`. They provide provenance and regression comparison, not production gates. The inherited legacy panel is therefore constructed with strict panel validation disabled: duplicated historical split/merge rows remain visible for comparison instead of aborting the optional diagnostic build. Historical regression comparisons use `build_legacy_iv_formulas()` on both the inherited and lineage panels so that those diagnostics isolate geography under the archived specification; the conservative/primary/full-reviewed panel-variant review instead uses the preferred public formulas.

## Main diagnostics

The extended lineage diagnostic writes:

- source inventory and evidence registry;
- Census registries and transition QA;
- conservative, primary, and full reviewed crosswalks;
- panel-variant counts, coefficients, first stages, and Gini audits;
- district-loss and identity-reclassification audits;
- multi-parent review queue;
- geometry QA and unit coverage;
- readiness, blockers, and six-step completion status;
- legacy comparisons when the legacy target group is enabled.

The canonical audit command is documented in `REPLICATION.md` and `docs/ARCHITECTURE.md`.

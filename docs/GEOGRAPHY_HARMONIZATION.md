# Geography harmonization

## Purpose

This document explains how observations are transformed across geographic definitions after district identities have been established. District identity/evidence belongs in [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md); this module governs analytical aggregation/allocation and the supported geography specifications.

## Supported geography specifications

The paper's primary geography is Census-2001 districts. Registered alternatives include reviewed lineage variants, exact/constant-boundary historical components, and the population-interpolated G2 construction used for specific historical robustness analyses.

Every analytical row records the geography specification or panel variant so results from different units cannot be silently combined.

## Multi-vintage representation

The geography layer stores explicit relationships among 1991, 2001, and 2011 units where evidence supports them. Exact three-vintage support requires all relevant links; a partial chain is not relabeled as exact three-vintage comparability.

## Aggregation and allocation

Administrative/source counts are aggregated through deterministic complete-parent relationships whenever possible. Rates/shares are calculated after counts have been pooled. Fractional allocation is used only in a separately declared method that provides a defensible weight; it is never inferred from name similarity.

## G2 population interpolation

G2 provides a population-interpolated harmonized geography for historical robustness when exact stable components are too restrictive. Its interpolation weights and support are explicit, and outputs remain labeled as G2 rather than as native Census-2001 districts.

## Population interpolation

Population-based allocation uses registered source/target population totals and preserves accounting within the declared component. Missing population anchors or inconsistent component totals fail the construction rather than being replaced by arbitrary equal shares.

## Analysis use

Geography variants answer sensitivity questions about boundary change and historical comparability. They should not be selected because one produces a stronger first stage or more favorable outcome coefficient.

## Validation

Validation covers component completeness, key uniqueness, weight sums, count preservation, expected support, and agreement between exact and interpolated variants where they overlap.

## Interpretation and limits

Changing geography can alter both sample composition and the estimand. Results on amalgamated or interpolated units therefore provide robustness evidence; they are not simply more or less precise versions of the native-district model.

## Implementation

Modern identity/linkage code is under `R/districts/`. Historical and G2 constructions are under historical geography/diagnostic modules with metadata in `data/metadata/`.

## Related documentation

- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)
- [`ARCHITECTURE.md`](ARCHITECTURE.md)

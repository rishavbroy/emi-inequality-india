# Geography harmonization

## Purpose

This document explains how observations are transformed across geographic definitions after district identities have been established. [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md) documents the identity evidence; this file documents aggregation, allocation, and the supported geography specifications.

## Supported geography specifications

The paper's primary geography is 2001 Census districts. Registered alternatives include reviewed lineage variants, exact/constant-boundary historical components, and the population-interpolated G2 construction used for specific historical robustness analyses.

Every analytical row records the geography specification or panel variant so analyses can distinguish results from different units.

## Multi-vintage representation

The geography layer stores explicit relationships among 1991, 2001, and 2011 units where evidence supports them. Exact three-vintage comparability requires every relevant link.

## Aggregation and allocation

Administrative/source counts are aggregated through deterministic complete-parent relationships whenever possible. Rates/shares are calculated after counts have been pooled. Fractional allocation is used only in a separately declared method that provides a defensible weight; it is never inferred from name similarity.

## G2 population interpolation

G2 provides a population-interpolated geography for historical robustness when exact stable components are too restrictive. Its interpolation weights and support are explicit, and outputs retain the G2 label.

## Population interpolation

Population-based allocation uses registered source and target population totals and preserves accounting within the declared component. Components with missing population anchors or inconsistent totals remain unresolved.

## Analysis use

Geography variants answer sensitivity questions about boundary change and historical comparability. Their definitions are fixed before first-stage and outcome results are compared.

## Validation

Validation covers component completeness, key uniqueness, weight sums, count preservation, expected support, and agreement between exact and interpolated variants where they overlap.

## Interpretation and limits

Changing geography can alter both sample composition and the estimand. Results on amalgamated or interpolated units therefore retain separate interpretations from the native-district model.

## Implementation

Modern identity/linkage code is under `R/districts/`. Historical and G2 constructions are under the historical geography and validation modules with metadata in `data/metadata/`.

## Related documentation

- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)
- [`ARCHITECTURE.md`](ARCHITECTURE.md)

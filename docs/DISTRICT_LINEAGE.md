# District lineage

## Purpose

The district-lineage system links source districts across vintages to the Census-2001 analysis geography. It provides reviewed identity evidence and deterministic aggregation rules; it is not a fuzzy matching shortcut.

## Reference geography

Census-2001 districts are the primary paper geography. Later-source districts may be pooled back to a 2001 parent only when the reviewed transition evidence supports a complete deterministic relationship. Historical analyses may use separate constant-boundary/harmonized geographies documented elsewhere.

## Evidence hierarchy

Final identities prioritize official codes/names and reviewed transition evidence, followed by corroborated external lineage sources and boundary evidence. Fuzzy string similarity is candidate-generation evidence only. A fuzzy-only candidate is never promoted to a production identity.

## Matching stages

1. Normalize state/district identifiers and names without changing substantive identity.
2. Resolve exact code/name evidence and registered aliases.
3. Generate candidate matches for unresolved cases using the centralized fuzzy-distance machinery.
4. Review candidates against independent lineage/boundary evidence.
5. Record the accepted relationship, evidence class, and unresolved status in tracked metadata.

## Adjudication rules

Reviewed decisions are data. They belong in the district-lineage metadata rather than hidden conditional branches. Multi-parent, cross-cutting, or incomplete transitions remain unresolved unless an explicit allocation rule is substantively justified and registered.

## Crosswalk roles

Crosswalks have explicit purposes: source-vintage identity, deterministic 2011-to-2001 pooling, historical constant-boundary construction, geometry attachment, or validation. A crosswalk valid for one role is not assumed valid for another.

## Aggregation rules

Counts are pooled before shares/rates. Means or percentages are never averaged across child districts without the underlying numerator/denominator or another declared weighting rule. Incomplete parent coverage is withheld rather than treated as a complete parent total.

## Unresolved districts

The system intentionally tolerates a bounded unresolved set. Full coverage is not a goal if achieving it would require unsupported assignments. Downstream analyses declare their support requirement and lose observations transparently when lineage is unavailable.

## Validation

Lineage checks cover key uniqueness, one-to-one/one-to-many role constraints, complete-parent conditions, evidence consistency, geometry attachment, and downstream support. The review archive retains lineage diagnostics needed to understand unresolved cases.

## Metadata and outputs

Tracked lineage inputs live under `data/metadata/district_lineage/` and related geography metadata. Derived lineage review files are written under diagnostic outputs. The full data-access inventory is in [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md).

## Legacy comparison

Older lineage variants may be retained for explicit sensitivity comparison, but they are isolated from the preferred reviewed lineage and must be labeled as legacy variants in outputs.

## Implementation

Primary code is under `R/districts/`, lineage/geography helpers, and the lineage target modules. The canonical audit command and build behavior are documented in [`BUILD.md`](BUILD.md) and [`../REPLICATION.md`](../REPLICATION.md).

## Related documentation

- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md)
- [`HISTORICAL_GEOGRAPHY.md`](HISTORICAL_GEOGRAPHY.md)

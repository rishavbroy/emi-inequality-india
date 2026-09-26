# Labor-market microdata

## Purpose and role

The labor module constructs district labor-market outcomes from NSS 64, NSS 66, and PLFS 2017-18. These surveys measure employment and labor-force structure around 2007-08, 2009-10, and 2017-18. They remain distinct from Economic Census establishment employment.

## NSS 64

NSS 64 Schedule 10.2 provides the 2007-08 labor and migration reference. The reader uses the registered DDI and relevant person blocks and validates their documented common population before district estimation.

The active outcomes use the declared usual-principal/subsidiary status definitions and denominator-specific age/support rules. Source geography is linked to 2001 Census districts only through the reviewed lineage system.

## NSS 66

NSS 66 provides labor outcomes for 2009-10. The original Nesstar package is retained as source evidence; the replication instructions describe how to materialize the local converted files. The adapter normalizes the converted person data to the shared labor-person schema before estimation.

## PLFS 2017-18

PLFS 2017-18 provides the later usual-status labor outcomes. The adapter uses the registered first-visit records, survey-design variables supplied by the data, and reviewed district lineage. The official package, data layout, and DDI/XML remain source evidence.

## Survey design

All three surveys use their registered weights, strata, PSUs, and domain support rules. District estimates are design based. The lonely-PSU convention is defined once in the shared survey layer and applied consistently across estimators.

## Geography

Native survey geography is preserved through ingestion. District results are produced only when the reviewed lineage supports the relevant source-to-2001 Census relationship. Unresolved districts remain unsupported; fuzzy matching is used only to generate candidates for lineage review.

## Outcomes

The registered outcomes include labor-force participation, employment, unemployment, and the sector and status measures supported by each wave. The outcome registry lists the exact columns used by the analysis.

## Inferential role

NSS 64 provides the 2007-08 labor reference. NSS 66 and PLFS contribute later district outcomes to the weak-IV and Anderson--Rubin analysis. These regressions describe how labor-market outcomes vary with the treatment measures under weak identification; they do not identify mediation.

## Validation

The module validates published population counts, person keys, weights and survey-design variables, outcome coding, denominator support, lineage coverage, and cross-wave schema consistency.

## Outputs

District labor estimates and inferential summaries are retained under the registered validation and table outputs. Source materialization files remain local and are not committed as raw data.

## Implementation

Relevant code is under `R/measures/build_nss_labor.R`, labor input/conversion helpers, shared survey-design utilities, lineage code, and the shared post-treatment estimation functions.

## Related documentation

- [`../REPLICATION.md`](../REPLICATION.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`ECONOMIC_CENSUS.md`](ECONOMIC_CENSUS.md)
- [pre-rewrite labor notes](../archive/research-log/documentation-before-2026-09-rewrite/docs/LABOR_MARKET.md)

# Labor-market microdata

## Purpose and role

The labor module constructs district labor-market outcomes from NSS 64, NSS 66, and PLFS 2017-18. These waves provide near-treatment, early-post, and longer-run evidence on employment and labor-force structure. They remain distinct from Economic Census establishment employment.

## NSS 64

NSS 64 Schedule 10.2 provides the near-treatment labor/migration reference. The source contract uses the registered DDI and relevant person blocks and validates their documented common universe before district estimation.

The active outcomes use the declared usual-principal/subsidiary status definitions and denominator-specific age/support rules. Source geography is linked to Census-2001 districts only through the reviewed lineage system.

## NSS 66

NSS 66 is the early-post labor wave. The original Nesstar package is retained as source evidence; the replication instructions describe how to materialize the local converted files. The adapter normalizes the converted person data to the shared labor-person contract before estimation.

## PLFS 2017-18

PLFS 2017-18 provides the long-run usual-status labor outcomes. The adapter uses the registered first-visit records, provider design variables, and reviewed district lineage. The official package, data layout, and DDI/XML remain source evidence.

## Survey design

All three surveys use their registered weights, strata, PSUs, and domain support rules. District estimates are design based. The lonely-PSU convention should be declared once in the shared survey layer and applied consistently; it is not a warning-suppression choice local to a single estimator.

## Geography

Native survey geography is preserved through ingestion. District results are produced only when the reviewed lineage supports the relevant source-to-Census-2001 relationship. Unresolved districts remain unsupported rather than being assigned with fuzzy-only matches.

## Outcomes

The registered family includes labor-force participation, employment, unemployment, and declared sector/status composition measures appropriate to each wave. The exact finite outcome registry is authoritative; this document does not duplicate every output column.

## Inferential role

NSS64 is primarily a near-treatment reference/balance source. NSS66 and PLFS contribute post-treatment local-development evidence and the shared weak-IV/Anderson--Rubin mechanism analysis. These regressions describe a possible economic channel under weak identification; they are not treated as identified mediation effects.

## Validation

The module validates source universe counts, person keys, weights/design variables, outcome-status coding, denominator support, lineage coverage, and cross-wave schema normalization.

## Outputs

District labor estimates and inferential summaries are retained under the registered diagnostics/table outputs. Source materialization files remain local and are not committed as raw data.

## Implementation

Relevant code is under `R/measures/build_nss_labor.R`, labor I/O/materialization helpers, shared survey-design utilities, lineage code, and the post-treatment mechanism inference layer.

## Related documentation

- [`../REPLICATION.md`](../REPLICATION.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`ECONOMIC_CENSUS.md`](ECONOMIC_CENSUS.md)
- [pre-rewrite labor notes](../archive/research-log/documentation-before-2026-09-rewrite/docs/LABOR_MARKET.md)

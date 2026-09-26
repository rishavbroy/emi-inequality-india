# Economic Census

## Purpose

The Economic Census module measures local establishment employment and sector structure using the Fifth/Sixth Economic Census and SHRUG district products. It provides post-treatment local-development outcomes and a predetermined EC05 computer/IT opportunity measure.

## EC05

The official Fifth Economic Census archive and SHRUG EC05 district product are the principal 2005 sources. The broad district family measures nonfarm establishments/employment and declared sector/employment shares. Source gaps remain missing rather than imputed.

The official fixed-width EC05 opportunity baseline keeps major-activity non-agricultural establishments and defines computer/IT activity using NIC-2004 Division 72. It reports IT establishments/workers and their shares of the internally consistent nonfarm universe.

## EC13

The SHRUG EC13 district product supplies the 2013 side of the broad longitudinal family. Only concepts published comparably in 2005 and 2013 enter longitudinal changes. EC05-only informal employment remains descriptive.

The available Sixth Economic Census coding does not permit an exact reconstruction of the NIC-2004 Division-72 concept because the NIC-2008-to-2004 concordance contains partial-class mappings below the published detail. The code therefore does not manufacture an exact 2005--2013 IT-growth measure.

## IT opportunity baseline

The paper-priority opportunity measure is the EC05 Division-72 employment share of nonfarm employment. It is used as predetermined effect-modification context for later welfare outcomes, not as an exclusion control or as evidence of IT growth.

## Geography

EC05 geography reflects the 2005 district system, including post-2001 changes. Same-code districts require compatible names; post-2001 children are pooled only through reviewed complete deterministic ancestry. Merged/cross-cutting districts are never split by assumption. Outputs retain the full Census-2001 registry with explicit availability flags.

## Construct semantics

Economic Census employment is employment located at establishments in the district. It is not resident-worker employment and should not be merged conceptually with NSS/PLFS labor outcomes.

## Inferential role

The broad 2005--2013 family enters the shared post-treatment mechanism inference layer on a finite registered outcome set. The EC05 IT opportunity baseline enters a separate descriptive heterogeneity analysis. Both roles remain explicitly labeled.

## Validation

Source readers validate schedule/type codes, geography, establishment/activity classification, worker counts, and internal universes before district aggregation. Longitudinal measures require comparable definitions across waves.

## Outputs

Economic Census district measures feed extended mechanism diagnostics and the paper's bounded IT-opportunity heterogeneity result. Raw archives remain local when redistribution is uncertain.

## Implementation

Relevant code is under Economic Census I/O/measure modules, geography harmonization, shared post-treatment inference, and output formatting.

## Related documentation

- [`LABOR_MARKET.md`](LABOR_MARKET.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)

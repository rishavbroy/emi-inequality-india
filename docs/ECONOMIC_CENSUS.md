# Economic Census

## Purpose

The Economic Census module measures local establishment employment and sector structure using the Fifth/Sixth Economic Census and SHRUG district products. It provides post-treatment local-development outcomes and a predetermined EC05 computer/IT opportunity measure.

## EC05

The official Fifth Economic Census archive and SHRUG EC05 district product are the principal 2005 sources. The broad district family measures nonfarm establishments, employment, and declared sector shares. Missing source cells remain missing.

The official fixed-width EC05 opportunity baseline keeps major-activity non-agricultural establishments and defines computer/IT activity using NIC-2004 Division 72. It reports IT establishments and workers as shares of the corresponding nonfarm establishment and employment denominators.

## EC13

The SHRUG EC13 district product supplies the 2013 side of the broad longitudinal family. Only concepts published comparably in 2005 and 2013 enter longitudinal changes. EC05-only informal employment remains descriptive.

The published Sixth Economic Census detail cannot reproduce the NIC-2004 Division-72 definition because the NIC-2008-to-2004 concordance includes partial-class mappings below the available detail. The project therefore reports no exact 2005--2013 IT-growth series.

## IT opportunity baseline

The paper uses the EC05 Division-72 employment share of nonfarm employment as a predetermined measure of local IT-sector opportunity when studying heterogeneity in later welfare outcomes.

## Geography

EC05 geography reflects the 2005 district system, including post-2001 changes. Same-code districts require compatible names; post-2001 children are pooled only through reviewed complete deterministic ancestry. Merged/cross-cutting districts are never split by assumption. Outputs retain the full 2001 Census district registry with explicit availability flags.

## Construct semantics

Economic Census employment counts jobs located at establishments in the district. NSS/PLFS labor outcomes instead describe residents, so the two measure different populations.

## Inferential role

The registered 2005--2013 outcomes enter the shared analysis of later district changes. The EC05 IT opportunity baseline enters a separate descriptive heterogeneity analysis, and the two roles remain explicitly labeled.

## Validation

Source readers validate schedule/type codes, geography, establishment/activity classification, worker counts, and internal universes before district aggregation. Longitudinal measures require comparable definitions across waves.

## Outputs

Economic Census district measures feed extended validation analyses and the paper's heterogeneity analysis using baseline IT employment share. Raw archives remain local when redistribution is uncertain.

## Implementation

Relevant code is under Economic Census I/O/measure modules, geography harmonization, shared post-treatment inference, and output formatting.

## Related documentation

- [`LABOR_MARKET.md`](LABOR_MARKET.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)

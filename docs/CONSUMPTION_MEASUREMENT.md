# Consumption measurement

## Purpose

This module constructs comparable district welfare outcomes from historical NSS consumer-expenditure rounds and modern HCES data. It defines the household consumption quantities, district estimands, survey design, support rules, and validation used by the paper's consumption analysis. Price conversion is documented separately in [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md).

## Role in the paper

The preferred consumption outcomes measure later district welfare in the paper. Historical rounds also support baseline adjustment, dynamics, and pre-trend checks. Modern HCES provides later welfare outcomes and cross-source consistency checks.

## Survey sources

The active family includes historical NSS consumer-expenditure rounds, the 2011-12 NSS round, and HCES 2022-23/2023-24. Exact required files and redistribution status are declared in `data/metadata/file_manifest.csv` and summarized in [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md).

## Household MPCE definitions

Input adapters normalize each survey to a common household-level schema before district estimation. The schema records the provider's consumption concept, household size, survey-design variables, and geographic identifiers.

Detailed-consumption rounds are reconstructed from their item and block files after accounting identities and person/household keys pass validation. The reconstructed household total is compared with any published aggregate measure available for that round before it is admitted to district estimation.

Modern HCES uses the registered nominal-MPCE schema. Nominal values are retained separately from real values so price conversion can be audited independently.

## District welfare estimands

District outcomes are registered rather than created ad hoc in downstream models. The main family includes mean/log-mean or distributional real-MPCE measures used by the paper and declared robustness specifications. Estimands are calculated with the survey design appropriate to the round; thin or unsupported domains are flagged rather than silently treated as precise district estimates.

For lineage-dependent historical rounds, household records are assigned only through reviewed/deterministic geographic links. Counts and survey-weighted aggregates are constructed after the source geography has been resolved to the analysis geography.

## Survey design

District welfare estimates use the registered survey weights, strata, and PSU identifiers for each round. Domain support is checked before estimating means or quantiles. Where a variance estimate is not defensible because support is too thin, the output records that limitation instead of fabricating precision.

The statistical treatment of lonely PSUs is a methodological choice and should be defined once in the shared survey-design layer. Any future change must update the relevant tests and this documentation together.

## Modern HCES validation

Modern HCES district estimates are compared across the available releases and visits using registered consistency checks. The preferred outcome is defined before downstream IV results are examined.

## Historical comparability

Historical consumption comparisons require both geographic harmonization and real-price conversion. The analysis retains the distinction among source-year nominal MPCE, real MPCE at the registered reference, and changes/long differences. A historical round enters a longitudinal comparison only when its district identity and price adjustment satisfy the registered support rules.

## Validation

The measurement layer checks, as applicable:

- household and item-level accounting identities;
- uniqueness of household/person keys;
- valid nonnegative weights and consumption quantities;
- district-domain support;
- lineage coverage before aggregation;
- agreement between reconstructed and published/aggregate consumption measures;
- consistency of modern HCES series on common support.

## Outputs

Paper tables and figures are generated downstream under `outputs/tables/` and `outputs/figures/`. Measurement and validation tables used for review are retained under `outputs/diagnostics/` according to the output-retention rules.

## Interpretation and limits

District consumption measures later local economic outcomes. Historical and modern rounds differ in questionnaire design and institutional context, so longitudinal comparisons use the registered harmonization and sensitivity analyses for each MPCE definition.

## Implementation

Primary implementation is under `R/consumption/`, `R/io/`, `R/measures/`, and the consumption target modules under `R/pipeline/`. Survey-family declarations and outcome definitions are centralized in their registries; output formatting remains under `R/output/`.

## Related documentation

- [`PRICE_DEFLATION.md`](PRICE_DEFLATION.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)
- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)

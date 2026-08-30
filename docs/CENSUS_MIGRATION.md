# Census migration module

This module starts the migration/sorting branch of the project with Census D-series tables that have defensible district-level contracts. It does not treat every downloaded migration workbook as district-identifiable merely because the state bundle is available.

## Current production scope

Extended diagnostics currently use:

- Census 2001 **D-02**: migrants classified by place of last residence, sex, and duration of residence in the place of enumeration;
- Census 2011 **D-02**: the corresponding origin/duration table; and
- Census 2011 **D-03**: migrants by place of last residence, duration of residence, and reason for migration.

The source workbooks are restored from `data/metadata/census_2001_download_manifest.tsv` and `data/metadata/census_2011_download_manifest.tsv` with `make download-census-tables`. Readers live in `R/io/read_census_migration.R`; count pooling and shares live in `R/measures/build_census_migration.R`.

## Why 2001 D-03 is excluded at district level

The official 2001 state D-03 workbooks have state-level geographic granularity and the attached raw workbooks contain only district code `00`. They therefore cannot support a district baseline for reason-for-migration composition. The 2011 D-03 state workbooks do include district rows. The pipeline stops rather than inferring 2001 district reason counts from state totals or another table.

This asymmetry means that D-02 can support a comparable 2001/2011 district origin-and-duration construct, while D-03 currently enters only as a 2011 post-treatment mechanism.

Official catalog references:

- Census 2001 D-02: `https://censusindia.gov.in/nada/index.php/catalog/19365`
- Census 2001 D-03: `https://censusindia.gov.in/nada/index.php/catalog/19463`
- Census 2011 D-02: `https://censusindia.gov.in/nada/index.php/catalog/10743`
- Census 2011 D-03: `https://censusindia.gov.in/nada/index.php/catalog/10840`

## D-02 district constructs

For each district, the reader retains total-person counts for place of enumeration = Total and the total origin row within each required origin category. The initial measures are:

- total migrants;
- migrants resident for 0-9 years (less than one year + 1-4 + 5-9 years);
- migrants whose last residence was elsewhere in the same district;
- migrants whose last residence was another district in the same state;
- interstate migrants; and
- migrants whose last residence was outside India.

The identity

`within-state outside place = elsewhere in same district + other district in same state`

is required exactly. A workbook that violates it fails rather than silently producing shares.

Composition shares are explicitly named `*_share_among_migrants`. For 2001, the module also joins the already validated `census_2001_district_totals$population_total` denominator and reports migrant stock, recent-migrant, and interstate-migrant shares of district population. Those population-denominated variables are the more direct predetermined sorting measures. No analogous 2011 population rate is created until a 2011 total-population denominator is separately integrated and validated.

## D-03 2011 reason constructs

The 2011 D-03 district total row partitions migrants into:

- work/employment;
- business;
- education;
- marriage;
- moved after birth;
- moved with household; and
- other reasons.

The seven reason counts must sum exactly to total migrants. Initial outputs retain both counts and shares among migrants. The source-level 2011 D-02 and D-03 district universes must also match exactly and report identical all-duration migrant totals; that cross-table identity is written to `d02_d03_2011_total_validation.csv`.

## Geography contract

Census 2001 D-02 rows already use the analytical Census-2001 district geography. Census 2011 D-02/D-03 counts are pooled backward only through `build_complete_deterministic_transition_2011_to_2001()`.

A 2001 parent is usable only when **every** 2011 district contributing territory to it is wholly and deterministically assigned to that same parent. Partial-parent reconstructions and fractional territorial allocations are excluded. Counts are summed first and shares are recomputed afterward; district shares are never averaged.

The same complete-parent bridge is now shared with the Census C-13 age-denominator module so administrative-count harmonization has one deterministic contract.

## Interpretation

These targets are extended diagnostics, not automatic controls or causal outcomes in the preferred models.

- 2001 D-02 measures are predetermined sorting/balance candidates and may later enter explicitly registered sensitivity specifications.
- 2011 D-02 and D-03 measures are post-treatment migration mechanisms. They must not be added to the preferred outcome equation as controls.
- D-03 does not recover migrant occupation or industry. The project will not synthesize missing 2011 D-08/D-09 cells by multiplying migration totals by destination-district B-series shares.

Later phases can add D-04/D-05/D-06/D-07, a validated 2011 population denominator, registered balance tests, and mechanism regressions after the source-level outputs and deterministic geography coverage have been reviewed.

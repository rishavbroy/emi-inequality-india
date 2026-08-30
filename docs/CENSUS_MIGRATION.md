# Census migration module

This module starts the migration/sorting branch of the project with Census D-series tables that have defensible district-level contracts. It does not treat every downloaded migration workbook as district-identifiable merely because the state bundle is available.

## Current production scope

Extended diagnostics currently use:

- Census 2001 **D-02**: migrants classified by place of last residence, sex, and duration of residence in the place of enumeration;
- Census 2011 **D-02**: the corresponding origin/duration table;
- Census 2011 **D-03**: migrants by place of last residence, duration of residence, and reason for migration;
- Census 2011 **D-04**: migrant education by duration, age, and last-residence type; and
- Census 2011 **D-07**: education and origin of migrants resident for 0-9 years who report work/employment as their reason for migration.

The source workbooks are restored from `data/metadata/census_2001_download_manifest.tsv` and `data/metadata/census_2011_download_manifest.tsv` with `make download-census-tables`. Readers live in `R/io/read_census_migration.R`; count pooling and shares live in `R/measures/build_census_migration.R`.

## Why 2001 D-03 is excluded at district level

The official 2001 state D-03 workbooks have state-level geographic granularity and the attached raw workbooks contain only district code `00`. They therefore cannot support a district baseline for reason-for-migration composition. The 2011 D-03 state workbooks do include district rows. The pipeline stops rather than inferring 2001 district reason counts from state totals or another table.

This asymmetry means that D-02 can support a comparable 2001/2011 district origin-and-duration construct, while D-03 currently enters only as a 2011 post-treatment mechanism.

Official catalog references:

- Census 2001 D-02: `https://censusindia.gov.in/nada/index.php/catalog/19365`
- Census 2001 D-03: `https://censusindia.gov.in/nada/index.php/catalog/19463`
- Census 2011 D-02: `https://censusindia.gov.in/nada/index.php/catalog/10743`
- Census 2011 D-03: `https://censusindia.gov.in/nada/index.php/catalog/10840`
- Census 2011 D-04: `https://censusindia.gov.in/nada/index.php/catalog/10994`
- Census 2011 D-07: `https://censusindia.gov.in/nada/index.php/catalog/11102`

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

For D-07 validation, D-03 additionally retains one narrow subtotal: work/employment migrants resident for less than one year, 1-4 years, or 5-9 years whose previous residence is classified as rural or urban within India. It is not used as a substantive D-03 outcome. Its purpose is to verify the D-07 universe exactly.


## D-04 migrant education constructs

D-04 is reduced to the district row with place of enumeration = Total, all durations of residence, all ages, and last residence = Total. Person counts retain:

- total, illiterate, and literate migrants;
- literate below matric/secondary;
- matric/secondary but below graduate;
- technical diploma/certificate below degree;
- graduate and above other than a technical degree; and
- technical degree/diploma equal to degree or postgraduate degree.

`illiterate + literate = total` must hold exactly. The published detailed educational-level columns do not exhaust the reported literate total in the attached workbooks, so the reader preserves the difference explicitly as `migrants_literate_education_not_classified`; it does not silently reallocate that residual. The detailed counts must sum to no more than the literate count.

The first mechanism shares are deliberately coarse and interpretable: literacy, graduate-or-technical-degree, and technical-credential shares among migrants. D-04 all-migrant totals must match D-02 exactly district by district before either table is harmonized.

## D-07 recent work-migrant constructs

D-07's universe is narrower than D-03: migrants resident for 0-9 years who report work/employment as the reason for migration and whose last residence is classified into one of four within-India origin cells:

1. rural within the state;
2. urban within the state;
3. rural outside the state; and
4. urban outside the state.

Those four cells must be present once per district. Counts are summed across them before any shares are constructed. The resulting diagnostics retain within-state versus outside-state and rural versus urban origin counts plus the same education categories used for D-04. Education accounting follows the same explicit-residual rule.

The four D-07 origin cells sum exactly to the D-03 0-9-year work/employment subtotal for rural/urban last residence within India. `d03_d07_2011_recent_work_validation.csv` records that source-level cross-table check. Because the D-07 universe excludes origins outside India and any unclassified origin, it is not compared with D-03's unrestricted recent-work total.

## Geography contract

Census 2001 D-02 rows already use the analytical Census-2001 district geography. Census 2011 D-02/D-03 counts are pooled backward only through `build_complete_deterministic_transition_2011_to_2001()`.

A 2001 parent is usable only when **every** 2011 district contributing territory to it is wholly and deterministically assigned to that same parent. Partial-parent reconstructions and fractional territorial allocations are excluded. Counts are summed first and shares are recomputed afterward; district shares are never averaged.

The same complete-parent bridge is now shared with the Census C-13 age-denominator module so administrative-count harmonization has one deterministic contract.


## Predetermined migration validity diagnostics

The 2001 D-02 measures now enter the same specification-aware IV validity engine used for other predetermined covariates. The compact balance family is:

- migrant stock as a share of district population;
- 0-9-year migrants as a share of district population;
- interstate migrants as a share of district population; and
- other-district-within-state share among migrants.

Balance is reported for the live candidate IV designs (region and state fixed effects crossed with the primary Shastry, Glottolog, and Dyen constructions). Individual p-values receive a Holm family-wise adjustment within specification, while the existing reverse-regression joint-balance diagnostic provides the omnibus test. These are falsification/balance diagnostics, not selection rules for adding controls.

A separate first-stage sensitivity compares the primary scalar linguistic-distance first stage under region and state fixed effects with and without the three population-denominated migration controls. Baseline and migration-adjusted specifications are estimated on one common complete-case sample, so changes in the excluded-instrument F statistic or partial R-squared cannot be attributed to sample composition. The migration controls remain a registered sensitivity block; they are not added to the preferred specification automatically.

## Interpretation

These targets are extended diagnostics, not automatic controls or causal outcomes in the preferred models.

- 2001 D-02 measures are predetermined sorting/balance variables. Their balance and first-stage sensitivity diagnostics are extended evidence; they do not automatically enter the preferred specification.
- 2011 D-02, D-03, D-04, and D-07 measures are post-treatment migration mechanisms. They must not be added to the preferred outcome equation as controls.
- D-03 does not recover migrant occupation or industry. The project will not synthesize missing 2011 D-08/D-09 cells by multiplying migration totals by destination-district B-series shares.

Later phases can add D-05/D-06, a validated 2011 population denominator, and registered mechanism regressions. The deterministic complete-parent bridge currently yields a substantially smaller 2011 mechanism sample than the 593-district 2001 baseline, so mechanism regressions should explicitly report that support and should not be generalized to excluded non-nested parents.

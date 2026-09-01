# Labor-market microdata

## Scope

The labor-market module is source-first and keeps resident-worker outcomes separate from establishment-location outcomes in the Economic Census. The first active source is NSS 64 Schedule 10.2 (July 2007–June 2008), which the official NSS catalog identifies as the Employment, Unemployment and Migration Survey.

## NSS 64 source contract

The active extended source contract uses three official files under `data/raw/nss/NSS 2007-08 Employment, Unemployment and Migration Survey 64th Round/`:

- `survey0/data/ddi.xml` for machine-readable file/schema metadata;
- `Block-4-demographic-usual-activity-members.sav` for demographic and usual-principal/subsidiary activity records;
- `Block-6-members-migration-records.sav` for member migration and temporary-away particulars.

The DDI reports 572,254 cases in both Block 4 and Block 6. Production ingestion therefore requires a complete one-to-one person-key universe across those two blocks. It also requires the common NSS design fields (sector, sub-round, sub-sample, NSS region, stratum, sub-stratum, FSU, second-stage stratum) and the combined multiplier `wgt_combined` to be present, with positive finite weights.

The official SPSS files store many discrete NSS codes as character-formatted variables. Production therefore normalizes published codes through the shared character-safe `num()` helper rather than applying `as.numeric()` directly to `haven_labelled` vectors. This preserves the underlying posted code values while avoiding accidental interpretation of value labels.

Block 4 exposes age, sex, education, usual principal activity status, NIC-2004, NCO-2004, and subsidiary-activity fields. Block 6 exposes temporary absence, migration-status/history fields, last usual place of residence, reason for leaving, and usual principal activity fields.

## Geography and survey estimation

NSS 64 uses two-digit state and district source codes. The source reader preserves those codes and the full survey design and does not assume that every survey district is a one-to-one Census-2001 district. District estimation is activated only after the reviewed deterministic lineage described below.

The realized target-support table contains 587 Census-2001 targets. Support is generally deep (median Kish effective N is about 486); the thin tail is concentrated in a few districts. Before inspecting any labor outcome estimates, the preferred support rule is frozen at at least 5 distinct FSUs and Kish effective N at least 100. Applied to the all-person target support, this excludes six of 587 targets. The rule is an analysis classification rather than a deletion gate: point estimates remain in diagnostics when estimable.

Outcome-specific support is recomputed on each outcome's actual denominator rather than inherited mechanically from the all-person table. This is important for unemployment and employment-composition shares, whose labor-force/employed denominators may be appreciably smaller.

The first predeclared district outcome family is deliberately compact:

- labor-force participation among persons age 15+;
- employment among persons age 15+;
- unemployment among the age-15+ labor force;
- regular-salaried employment among age-15+ employed persons;
- share of persons age 15+ whose place of enumeration differs from the last usual place of residence.

Employment classification follows the standard NSS usual-status principal-plus-subsidiary (ps+ss / UPSS) concept: a person is treated as employed if either principal or subsidiary usual status is an employment code. A principal-status unemployed person with subsidiary employment is therefore classified as employed under UPSS. Regular-salaried status uses the principal employment status when the principal status is employed and otherwise the subsidiary employment status.

The survey design preserves the published district-level stratification by nesting raw strata and FSUs under the reviewed source district, state, and sector, uses the posted combined multiplier, and estimates district domains with `survey::svydesign()`/`survey::svyby()`. Lonely-PSU handling uses the same shared adjustment helper as the consumption survey-design layer. Wage outcomes remain deferred to the weekly-status/earnings block rather than being inferred from Block 4.

## Reviewed NSS64 district lineage

NSS64 labor geography now reuses the project's existing reviewed `nss_2007_08` district lineage instead of creating a labor-specific concordance. The survey publishes a three-digit `State_Region` code and a two-digit district code; production reconstructs the same five-digit `SSRDD` source identity used by the reviewed NSS64 education lineage as `paste0(State_Region, District)`. The first two digits of `State_Region` must agree with the published state code.

Only reviewed deterministic mappings with unit weight are accepted for person-level labor data. Survey districts without such a mapping remain `unresolved_source_district`; population-allocation or fractional lineage variants are not applied to people. In the current NSS64 source, all 588 observed reviewed source identities resolve deterministically to 587 Census-2001 targets. `nss64_lineage_support.csv` reports source-district sample people, distinct FSUs, weighted person mass, Kish effective sample size, reviewed Census-2001 target, and resolution status. `nss64_target_support.csv` pools source districts mapping to the same Census-2001 target before reporting support and now carries the frozen preferred-support classification.

Block 4 and Block 6 must also agree person-by-person on the common survey geography and design fields. Matching person keys alone is insufficient because a cross-block geography/design disagreement would invalidate later labor/migration joins.

## Diagnostic outputs

The extended labor branch writes `nss64_source_validation.csv`, `nss64_lineage_support.csv`, `nss64_target_support.csv`, `nss64_outcome_registry.csv`, and `nss64_district_outcomes.csv`. The district-outcome table keeps point estimates, design standard errors, denominator-specific sample/PSU/Kish support, and the preferred-analysis eligibility flag in one long table. No labor outcome is yet routed into the causal IV mechanism family; the next phase should inspect these design-based district estimates first, then decide whether the near-treatment exercise is best treated as balance/validity evidence or as a separately registered mechanism family.

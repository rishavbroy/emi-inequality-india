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

The extended labor branch writes `nss64_source_validation.csv`, `nss64_lineage_support.csv`, `nss64_target_support.csv`, `nss64_outcome_registry.csv`, and `nss64_district_outcomes.csv`. The district-outcome table keeps point estimates, design standard errors, denominator-specific sample/PSU/Kish support, and the preferred-analysis eligibility flag in one long table.

NSS64 is frozen as a **near-treatment reference** rather than a predetermined balance test or a post-treatment causal mechanism family. Its July 2007-June 2008 field period overlaps the project's 2007-08 EMI measurement window, so contemporaneous labor differences cannot establish pre-treatment balance and should not be interpreted as long-run responses. The machine-readable outcome registry records this temporal role.

## NSS 66 source contract

The next wave is NSS 66 Schedule 10 (July 2009-June 2010). The official source is locally available as a DDI plus a proprietary `.Nesstar` container. The DDI reports 459,784 demographic records (F4), 459,784 usual-principal-activity records (F5), and 34,689 usual-subsidiary-activity records (F6). Unlike NSS64, subsidiary activity is a conditional separate block rather than one field on every principal-status person, so future production ingestion must left-join F6 onto the complete F4/F5 person universe rather than require equal F5/F6 row counts.

The DDI contract requires the common person/design fields and the all-subround combined `WEIGHT` variable, plus age/demographics, principal activity status/NIC/NCO and subsidiary activity status/NIC/NCO. The official catalog defines `WEIGHT` as the weight for all-subround combined estimation; `WEIGHT_SR` is reserved for subround-specific estimates.

Production does not implement a repository-specific `.Nesstar` parser. Conversion should use the maintained `nesstar-converter` package (pinned to a reviewed release when the converted files are materialized), which reads `.Nesstar` with companion DDI metadata and exports open tabular formats. The DDI validation target is active now; district estimation remains blocked until converted F4/F5/F6 tables have been materialized and inspected.

### NSS66 conversion and canonical person adapter

NSS66 now uses the repository's generic metadata-driven Nesstar materialization
boundary rather than a survey-specific converter script. `scripts/materialize_nesstar.py
nss66_eus` requires the reviewed `nesstar-converter==1.0.4` release and reads the
canonical DDI and `.Nesstar` paths
from `data/metadata/file_manifest.csv`, converts to CSV, identifies F4/F5/F6 by
their DDI-validated signature columns and exact case counts, and writes only the
three required tables to the gitignored `data/interim/nss66_eus/` directory.
The script does not install packages or mutate the R environment.

`data/metadata/nesstar_conversion_contracts.csv` is keyed by `source_id` and pins
the converter version, raw manifest IDs, exact block row counts, signature
columns, and deterministic interim filenames. This boundary is reusable for
later `.Nesstar` sources only when conversion is actually necessary; existing
open companion files remain preferred when available. The R
adapter then validates the converted blocks again. F4 and F5 must have the same
complete PID universe; F6 must equal exactly the subset of F5 persons coded
`Whether_in_Subsidiary_Activity == 1`. Shared survey-design and geography fields
must agree by PID. Annual pooled estimation uses `WEIGHT`, not `WEIGHT_SR`.
Urban blank `Sub_Stratum_No` values are represented internally as `__none__`,
consistent with the Round-66 design's lack of urban sub-stratification; rural
blank sub-strata remain invalid.

The extended audit now treats materialization as an explicit optional state. Before
conversion it writes `nss66_materialization.csv` with `not_materialized` status
and remains green. If any contracted block or sidecar exists without the complete
set, the audit fails rather than silently using a partial conversion. Once all
three blocks and the conversion manifest are present, the audit automatically
runs the real F4/F5/F6 canonical join and same-round lineage validation and writes
`nss66_source_validation.csv`. The inspector also recognizes the narrowly defined
legacy sidecar written by the original NSS66-only materializer (`file_id`, path,
row/byte/hash, converter version), derives only its missing source/package fields
from the pinned current contract, and labels it `legacy_v1` in diagnostics. Unknown
or partial manifest schemas remain fatal. Newly materialized sidecars carry an
explicit schema version. District outcomes remain deliberately inactive until
those reviewer-visible diagnostics have been inspected.

The district estimator is now wave-invariant. NSS64 keeps its five registered
outcomes, including migration, and its `near_treatment_reference` role. NSS66
registers the same four usual-status labor outcomes without migration and with
`temporal_role = "early_post"`. NSS66 district lineage reuses the already
reviewed 2009-10 Type-2 consumption bridge on the same state/district survey
frame, accepting only resolved one-to-one mappings. This avoids maintaining a
second 2009-10 geography adjudication system for labor data.

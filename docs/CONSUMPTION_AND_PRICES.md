# Consumption and price adjustment

## Consumption-survey architecture

`data/metadata/consumption_survey_registry.csv` is the canonical metadata contract for
consumption-survey timing and intended analytical role. It distinguishes the current
legacy short-form education-survey consumption measures from the detailed Schedule
1.0 and HCES sources planned for the welfare revision. Downstream code must not infer
a survey period from a year label when the registry already declares it.

The first migration step is deliberately behavior-preserving. Generic
`survey_period_months()`, `survey_subround_for_month()`, and
`build_survey_subround_deflators()` functions now own the quarterly survey-period
logic. The historical `nss_*` functions are compatibility wrappers backed by the
registry, so the current 2007-08 and 2017-18 public outputs remain unchanged while
later phases add canonical Schedule 1.0 and three-visit HCES household readers. The
Schedule 1.0 readers and the nominal HCES three-visit reconstruction are now active.
Modern HCES real expenditure uses its registered overlapping three-month panel timing,
not the non-overlapping quarterly NSS sub-round implementation.

The registry also records that the active 2007-08 outcome currently comes from the
education survey's household consumption question, whereas the planned 2007-08
Schedule 1.0 source is a distinct survey contract. This distinction must remain
explicit during the welfare migration.

### Modern HCES nominal-MPCE contract

`R/io/read_hces_three_visit.R` owns the release-schema differences between HCES
2022-23 and 2023-24. Production reconstruction reads only Level 14 (A1/B1/C1
questionnaire summaries) and Level 15 (A2/B2/C2 household size and visit
metadata). `data/metadata/hces_summary_items.csv` declares each summary item,
its 7/30/365-day reference period, and whether it belongs in MPCE. Monthlyization
uses `value * 30 / reference_days`; imputed house/garage rent (CSQ item 539) is
explicitly excluded.

For each complete household, questionnaire expenditure is converted to a
per-person component with the household size observed in that questionnaire,
then summed across FDQ, CSQ, and DGQ. This is algebraically the published
three-questionnaire MPCE formula. The Level-15 single-shot "usual monthly
consumption expenditure" cross-validation field is not read by the adapter.

Questionnaire order rotates in 2023-24, so the survey multiplier is taken from
the row with `VISIT == 3`, regardless of whether that visit canvassed F, C, or D.
The released 2022-23 Level 15 omits visit order; that adapter therefore requires
the F/C/D multiplier to be identical within household and fails otherwise.
Both rounds must reproduce the official all-India rural/urban MPCE benchmarks
within one rupee before their nominal household objects can feed later price,
geography, or welfare targets.

The distributed Level 14 summary is sparse rather than a rectangular
household-by-questionnaire table. A small number of F/C/D questionnaire pairs have
a Level-15 visit but no Level-14 A1/B1/C1 row. At the Level-14/15 join boundary,
those absent summaries are treated as zero questionnaire expenditure; a Level-14
summary with no matching Level-15 visit remains an error. The production diagnostic
`hces_summary_coverage.csv` reports the number and share of zero-filled summaries
by round and questionnaire. This convention remains protected by the blocking
official rural/urban MPCE benchmark test.

The 2022-23 public release also contains a valid household with blank `Sample SU
No.` and `Sample Sub-division No.` fields. Those two fields are therefore optional
components of the canonical household key; FSU, second-stage stratum, and sample
household number remain required. The full released Level-15 household roster is
collision-free under that contract.

Modern HCES Levels 14 and 15 are parsed once per round into a registered bundle.
The canonical household reconstruction and sparse-summary QA are derived from the
same in-memory levels, avoiding a second multi-hundred-megabyte read and preventing
diagnostic parsing from drifting away from production parsing.

### Modern HCES source geography

`data/metadata/hces_2022_24_district_codebook.csv` is a derived transcription of
Appendix I ("List of NSS Regions and their Composition") in the official HCES
2022-23 Volume I, paired with the released HCES tabulation state-code workbook.
It contains 695 unique state/district code pairs across 36 States/UTs. Every
state/district code observed in the released 2022-23 and 2023-24 household files
is covered by this codebook. The 2023-24 use is therefore an observed-code
continuity mapping, not an assertion that administrative geography was frozen.

The modern source-geography codebook also carries a `price_state_code` only where
post-2001 administrative reorganization makes the HCES state code incompatible
with the historical Tendulkar spatial-price geography. Diu and Daman retain the
former Daman & Diu anchor (`DADI`), Dadra & Nagar Haveli retains `DNHA`, and Leh
and Kargil retain the former Jammu & Kashmir anchor (`JNK`). Other districts
default to their ordinary source-state code. This keeps district-dependent
reorganization logic out of the generic price engine.

The existing `attach_consumption_source_district_identity()` contract resolves
modern HCES codes to named source districts before real-consumption objects move
to the lineage layer. The modern rounds then reuse the existing conservative
consumption-lineage reference: exact Census-2001 identities, reviewed identity
aliases, and stable reviewed cross-wave lineage are accepted; unresolved or
conflicting modern source districts remain explicit review-queue rows.

The repository already contains accepted post-2011 administrative-event
adjudications and 1951-2024 lineage sources. Those records are not duplicated in
the HCES reader. Where they are not yet represented in the conservative
consumption-lineage reference, the modern review queue identifies the remaining
integration gap instead of replacing it with a name-only shortcut.

For the primary real-MPCE construction, panel `r` is assigned the three consecutive
survey months `r:(r+2)`: panel 1 covers the first through third survey months and
panel 10 covers the tenth through twelfth. The state-sector CPI deflator is averaged
over those three months and attached at the household level before any district
aggregation. This deliberately uses the same panel-average timing rule in 2022-23
and 2023-24. The public 2022-23 Level 15 release does not identify questionnaire
visit order, while 2023-24 does; exact questionnaire-visit deflation for 2023-24 is
therefore reserved as a later timing sensitivity rather than changing the primary
cross-round estimand.


### Modern HCES district welfare and consistency

After source geography, price assignment, and conservative lineage attachment,
the 2022-23 and 2023-24 rounds use the same registered design-aware welfare
estimator as historical NSS rounds. Only `resolved_*` lineage households enter
district estimation. The public welfare output therefore contains the registered
real mean MPCE, mean log real MPCE, and person-weighted median MPCE together with
household, FSU, Kish-effective-N, support, precision, and eligibility diagnostics.

`modern_hces_welfare_consistency.csv` compares the two modern endpoints by
registered outcome on common Census-2001 districts. Preferred-eligible common
districts are used when at least three exist; otherwise the diagnostic falls back
to all finite common districts and reports that basis explicitly. This is a
measurement-stability diagnostic, not a rule for selecting whichever endpoint
produces a more favorable regression result.

## Main-paper specification after the revision gate

The preferred specification uses the person-weighted mean of real monthly
per-capita household expenditure in each Census 2001 district. Household
expenditure is adjusted before district aggregation using a state, rural/urban,
and survey-period price index. The outcome is the difference in log real
consumption between 2007-08 and 2017-18.

The revision gate has passed: the public model now uses this real-consumption outcome together with the predetermined Census 2001 controls, state fixed effects, all-child EMI exposure, and the preferred scalar linguistic-distance instrument. `build_revised_iv_formulas()` defines the public specification used by the production and conservative/primary/full-reviewed lineage-sensitivity models. `build_legacy_iv_formulas()` is confined to the optional inherited-geography comparison.

## Consumption estimands

The preferred district mean is

\[
\bar c_d = \frac{\sum_h w_h X_h}{\sum_h w_h n_h},
\]

where `X_h` is total monthly household expenditure, `n_h` is household size,
and `w_h` is the NSS household multiplier. This is mean expenditure per
represented person. The former household-weighted mean MPCE is retained for an
appendix comparison.

The 2007-08 and 2017-18 education surveys both use short household expenditure
questions. They remain imperfectly comparable. In particular, the NSS 75 UMPCE
question was intended to classify households rather than replace Schedule 1.0.
Price adjustment does not remove that measurement limitation.

## Price sources

The intended temporal series are:

- CPI-RL for rural households before 2013;
- a state aggregate of CPI-IW centres for urban households before 2013;
- state CPI-Rural and CPI-Urban from 2013 onward.

The CPI-IW state aggregate uses the Labour Bureau's 2001-base centre weights,
renormalized within state. Old and new series are linked with their median ratio
over common months. The 2011-12 Tendulkar state-sector poverty lines provide the
spatial price relatives.

Every substitution or fallback must appear in a generated coverage table. The
code must stop when a household lacks a valid state-sector-period deflator.

## Appendix results

The appendix will report:

- endpoint ANCOVA using real log consumption;
- nominal log change;
- household-weighted district consumption;
- the former 2007 control set;
- detailed 2007 Schedule 1.0 MPCE where available;
- sensitivity to CPI-AL in place of CPI-RL;
- alternative overlap-period links;
- the number and weighted share of households using each fallback.

## RBI price readers

`R/prices/read_price_sources.R` reads the four RBI DBIE extracts named in
`price_series_registry.csv`. It retains only the general rural and urban index
from the two CPI-R/U files, distinguishes CPI-AL from CPI-RL, and selects the
2001-base centre series from CPI-IW. The CPI-IW state series is a weighted mean
of centre indices using the Labour Bureau's 78 centre weights. For the 2007-08
series, Godavarikhani, Hyderabad, and Warangal are assigned to undivided Andhra
Pradesh. State CPI-IW aggregation is limited to the 2007-08 estimation months
and the 2013-14 overlap used to link CPI-IW to CPI-Urban. A required state-month
is rejected when one of its expected centres is absent; the code does not
silently reweight an incomplete set of centres. The full centre table remains
available for source diagnostics.

These readers construct validated source tables only. `R/prices/build_temporal_price_series.R` rescales CPI-RL and state CPI-IW to the 2012-base CPI-R/U scale using the median state-sector ratio over common 2013-14 months. The production chain retains the older sources only for the July 2007-June 2008 survey period and uses state CPI-Rural or CPI-Urban from January 2013 onward. A state-sector chain is rejected when it has fewer than the required number of common months; no link is inferred from another state. The base-2010 CPI-R/U observations for July 2011-June 2012 are converted to the 2012-base scale with the observed 2013-14 overlap and supply the common reference index. This avoids deriving the spatial anchor from incomplete CPI-IW centre coverage in isolated 2012 months.

The production target graph reads the four CPI files, constructs the monthly state-sector deflator, converts each NSS sub-round to its three survey months, and attaches the arithmetic mean of those monthly deflators to Block 3 household records before district aggregation. The resulting real-consumption measure is used by the active public model.

## State inheritance, union-territory fallbacks, and the spatial anchor

The tracked `price_state_crosswalk.csv` is deliberately narrow. A direct
state-sector-month observation always wins. A donor is used only when the
direct temporal series is absent and the target month falls within a dated,
sector-specific rule. The five small-UT donor states match the official
Planning Commission substitutions used for the 2011-12 Tendulkar estimates:
Tamil Nadu for Andaman and Nicobar Islands, Punjab for Chandigarh,
Maharashtra for Dadra and Nagar Haveli, Goa for Daman and Diu, and Kerala for
Lakshadweep. Telangana inherits undivided Andhra Pradesh only for months before
the post-2012 state CPI-R/U series. The code does not use an undocumented
nearest-state or all-India fallback.

`tendulkar_poverty_lines_2011_12.csv` resolves Table 1 and the five Table 2
notes to one rural and one urban line for every analysis state/UT. Telangana
uses the 2011-12 Andhra Pradesh line because it did not yet exist as a separate
state. Spatial relatives divide every line by the common all-India rural line
of Rs. 816. This retains both interstate differences and the national
rural-urban price-level gap; using separate rural and urban denominators would
remove the latter by construction.

For state `s`, sector `r`, and month `t`, the combined deflator is

\[
D_{srt}=\frac{PL_{sr}}{816}\times
\frac{CPI_{srt}}{\overline{CPI}_{sr,2011\text{-}12}}.
\]

The output records the temporal source state, whether the observation is
direct, inherited, or a fallback, the fallback reason, and the poverty-line
source state. Household attachment preserves these fields so later coverage
tables can report the number and survey-weighted share of households using
each rule.


## NSS timing and household consumption

Both NSS 64 (July 2007-June 2008) and NSS 75 (July 2017-June 2018) divide the annual field period into four three-month sub-rounds. `R/prices/nss_period_deflators.R` maps sub-rounds 1-4 to July-September, October-December, January-March, and April-June, respectively. A sub-round deflator is the arithmetic mean of exactly three monthly state-sector deflators; incomplete quarters fail rather than being averaged over the available months.

`prepare_2007_consumption_households()` and `prepare_2017_consumption_households()` deduplicate Block 3 household records, resolve NSS state and rural/urban codes, attach the sub-round price object, and retain nominal and real household totals and per-capita values. Deflation occurs before district aggregation. District real consumption is therefore

\[
\bar c^{real}_{dt}=\frac{\sum_h w_h X^{real}_h}{\sum_h w_h n_h},
\]

with the household-weighted mean retained separately. The district outputs also record the person-weighted mean deflator and the survey-weighted household share using inheritance or fallback rules.

## Implemented fixed-sample comparison

The extended-diagnostics graph estimates nominal log change, person-weighted
real log change, and real endpoint ANCOVA on one common district sample. These
diagnostic models retain the same non-outcome legacy controls and state fixed
effects to isolate the consequences of changing the outcome construction. The
public model is separate: it uses person-weighted real log consumption change,
the predetermined Census 2001 controls, and state fixed effects.

The same diagnostic target writes household price-assignment coverage by wave,
state, sector, sub-round, assignment type, and donor state, together with a
district-level comparison of nominal, real, household-weighted, and
person-weighted consumption constructions.

The fixed-sample outcome comparison deliberately removes the legacy nominal
`consumption_0708` level from the common control vector. The change-score models
therefore do not condition on their own baseline outcome, while the ANCOVA model
adds exactly one baseline term, `log_real_consumption_0708`. This keeps the
comparison interpretable and avoids including both nominal baseline consumption
and its logged real counterpart in the ANCOVA first and second stages. The
extended diagnostics also export the excluded-instrument first-stage statistic
for each specification and the estimated ANCOVA baseline coefficient.


## Public specification

The public tables, figures, report values, and spatial diagnostics use the person-weighted real log consumption change model with the predetermined Census 2001 controls and state fixed effects. Nominal log change, ANCOVA, and legacy-control variants remain diagnostic comparisons. Because the conditional excluded-instrument first stage is extremely weak, the public interpretation is explicitly non-causal.


## Canonical detailed-consumption household contract

Registered detailed Schedule 1.0 surveys are normalized before price attachment or district aggregation. The canonical household table contains one row per household with survey/source geography, household size, survey weight, nominal MPCE, and nominal household consumption. `canonicalize_detailed_consumption_households()` is the only adapter boundary for direct household-MPCE files and split household/MPCE files.

The survey registry declares the source fields and MPCE scale instead of embedding round-specific column choices in downstream estimators. This is already active for NSS 61 (2004-05), NSS 66 Type 1/Type 2 (2009-10), and NSS 68 Type 2 (2011-12). The NSS 68 public MPCE field is stored in hundredths of rupees, so its registry scale is `0.01`; NSS 61 and NSS 66 fields are already rupee values. NSS 66 Type 1 uses the published MRP MPCE for comparability with the modified-reference-period family.

Modern HCES rows deliberately declare a separate `three_questionnaire` adapter and are rejected by the direct-MPCE normalizer. Their FDQ/CSQ/DGQ construction belongs in the HCES-specific adapter phase rather than being forced through the legacy Schedule 1.0 contract. Likewise, the legacy education-survey consumption questions remain separate contracts and are not silently promoted to detailed-consumption measures.

## Detailed-consumption source ingestion

The historical detailed-consumption adapter now reads the distributed NSS CSV archives through one column-contract interface. It locates the unique household/MPCE members by the fields declared in `consumption_survey_registry.csv`, rather than by survey-specific filenames. The canonical output preserves FSU, stratum, and sub-stratum identifiers needed for later design-based district estimation. NSS 61 uses its MPCE-365/MRP field; NSS 66 Type 1 uses `MPCE_MRP`; NSS 66 Type 2 uses `MPCE`; and NSS 68 Type 2 applies the documented 0.01 scale to the distributed integer MPCE field. Modern HCES remains excluded from this adapter because its FDQ/CSQ/DGQ three-visit construction is methodologically distinct.

## Source district identities for detailed NSS rounds

Historical detailed-consumption households keep their released geography untouched until a round-specific official district dictionary is attached. NSS 61 uses `District_code_list_nss61_round.xls`; NSS 66 uses `District code_66.xls`; NSS 68 reads the labelled `District_Code` categories directly from the DDI XML embedded in the distributed Type-2 archive. The attachment layer accepts either two-digit within-state district codes or documented four-digit state+district codes, requires every observed household code to resolve exactly once, and preserves both the official source label and canonical matching keys. It does not infer district identity from numeric ordering.

Official source labels are retained even when a codebook appears textually damaged. Such anomalies are review evidence, not an invitation to silently rewrite the source; later lineage matching may use independently documented administrative identities while keeping the raw label auditable.


### Aggregate survey-frame geography is not a district

Source-geography resolution distinguishes named districts from documented survey-frame aggregates. NSS 61 is the first active example: the released Delhi records contain ordinary district codes together with codes 98 and 99. The round's official estimation procedure identifies the corresponding strata as an all-district aggregate and a Delhi Municipal Corporation aggregate; the released microdata show those special codes only with strata 99 and 10, respectively. These rows are retained for state/national reconstruction and official-aggregate QA, but `source_lineage_eligible = FALSE` prevents them from being silently assigned to a Census-2001 district. Unknown codes still fail. The small tracked special-unit registry records these exceptional source identities and their mapping basis rather than embedding round-specific exceptions in R.

The NSS 61 workbook spells Tamil Nadu as `Tamilnadu`; this is handled in the shared state-name canonicalization table, not in consumption-specific code. NSS 68 DDI parsing uses the document's declared XML namespace explicitly so default-namespace metadata do not generate XPath warnings.

### Historical MPCE reconstruction gate

Before historical detailed-consumption households are used for prices, lineage, or district welfare outcomes, the pipeline reconstructs all-India rural and urban person-weighted MPCE and compares it with official MoSPI benchmarks in `data/metadata/consumption_mpce_benchmarks.csv`. The estimator uses household survey weight times household size, so the estimand is average MPCE across persons rather than households. The target fails when an estimate lies outside its declared rupee tolerance and writes the passing comparison to `outputs/diagnostics/public/consumption_mpce_reconstruction.csv`.

The benchmark definitions match each registered survey construct: MRP for NSS 61 (2004-05) and NSS 66 Type 1, and MMRP for NSS 66 Type 2 and NSS 68 Type 2. This gate intentionally uses all survey households, including source-frame aggregate units that are ineligible for district lineage, because the published national estimates use the full survey sample.


## Historical detailed-consumption deflation

The registered NSS 61, NSS 66 Type 1/Type 2, and NSS 68 Type 2 household
records now use the same production state-sector monthly price chain as the
legacy outcomes. The pre-2013 CPI-RL/CPI-IW portion is retained from July 2004
through December 2012, so every quarterly sub-round in the 2004-05, 2009-10,
and 2011-12 surveys is covered before the January 2013 CPI-R/U switch.

`deflate_detailed_consumption_households()` operates only on the explicit
canonical detailed-survey contract: authoritative source state, sector,
registered period group, nominal MPCE, nominal household consumption, household
size, and survey weight. It does not reuse the legacy column-name heuristic that
guesses whether a source field is a household total. The production price window is derived from the consumption-survey registry rather
than hard-coded in `_targets.R`. Surveys whose household adapter is still
`legacy_schedule_pending` do not expand the production window; every implemented
survey does. With the current registry this yields July 2004 through July 2024.
The assembled deflator table must cover every month in that window before any
household-level price attachment is allowed to run.

Price attachment dispatches
from the survey registry: non-overlapping three-month sub-rounds for legacy NSS
Schedule 1.0 and overlapping three-month panels for modern HCES. When source
geography supplies an explicit historical `price_state_code`, that key is used
instead of trying to infer a pre-reorganization price geography from the modern
state code alone. The shared household-price diagnostic uses the same contract
and reports legacy `subround` and modern `panel` assignments in one long-format
output, with the appropriate survey-weight field selected from each canonical
household object. Only then are
`real_mpce` and `real_household_consumption` constructed and their household-size
identity checked.
The production targets depend on the corresponding official MPCE reconstruction
gate, so a survey cannot proceed to real welfare measures if its nominal
reconstruction has not first matched the published benchmark.

### CPI-IW base transition for the 2004-05 welfare baseline

The urban pre-2013 temporal series respects the Labour Bureau CPI-IW base regimes. The 2001-base 78-centre series begins in January 2006, so 2004-05 is constructed from the predecessor 1982-base system rather than by back-casting the 2001 centre weights. `data/metadata/cpi_iw_centres_1982.csv` records the published 70-centre All-India weights, three additional one-centre state series needed for Goa, Himachal Pradesh, and Tripura, and the published 2001/1982 centre linking factors. State indices retain the 1982 weighting system and are converted to 2001-base units with the weighted mean of available published linking factors within each state; `link_weight_coverage` records the share of the old state weighting system represented by centres with a direct published link when a retired centre has no 2001 successor. The published All-India linking factor (4.63) is used for the explicit All-India fallback series. January 2006 onward uses the native 2001-base 78-centre system.

### Historical CPI-IW source gaps

The historical urban chain remains weighted state CPI-IW before 2013. The RBI
centre extract is required to contain the complete official centre universe for
a month; partial months are never renormalized over the centres that happen to
be present. If a 2001-base CPI-IW month is incomplete and falls in the period
covered by the official state CPI-Urban series (2010=100, released from January
2011), the missing state-months are filled from CPI-Urban after a state-specific
median overlap calibration to CPI-IW units. The output records
`cpi_iw_completion = "scaled_cpi_u_2010_gap_fill"` for those observations.
Months that cannot be completed from an official overlapping series remain
fatal. This rule is designed for source-file gaps such as the incomplete RBI
centre months in 2012; it does not replace weighted CPI-IW as the primary
pre-2013 urban deflator.

### Historical consumption lineage handoff

Detailed historical consumption surveys now enter the existing lineage-v2
system only after source district identity and real household consumption are
established. Survey district numbers are retained as source identifiers but are
not interpreted as Census codes.

The consumption lineage bridge accepts an exact normalized state/district
identity in the Census-2001 registry. For non-exact identities it may reuse the
existing reviewed NSS lineage only when at least two reviewed NSS waves imply
the same complete Census-2001 target-weight distribution for that state/district
identity. Cross-wave disagreement remains unresolved rather than being selected
by proximity or name similarity. Source units explicitly marked non-district
(e.g. the reviewed NSS-61 Delhi aggregate units) remain in survey-level QA but
are not lineage eligible.

Population-allocation mappings duplicate a household across Census-2001 targets
with `lineage_weight`; `lineage_survey_weight` and `lineage_person_weight` apply
that allocation share so resolved household weight is conserved. Unresolved and
conflicting source districts remain in the lineaged household object with an
explicit `lineage_status` and no target weight. Coverage is written to
`outputs/diagnostics/public/consumption_lineage_coverage.csv` before district
welfare aggregation is introduced.

### Historical consumption lineage identity aliases

Historical consumption source labels are never fuzzy-matched into Census-2001 districts. A small reviewed metadata registry, `data/metadata/consumption_lineage_identity_aliases.csv`, handles only deterministic orthographic, abbreviation, truncation, or documented source-label corruption cases whose target is a unique Census-2001 district within the same normalized state. The lineage bridge applies exact Census-2001 identity first, then these reviewed identity aliases, then stable reviewed cross-wave lineage. Administrative-change cases remain unresolved for explicit adjudication.

### Historical district welfare survey design

Historical district mean MPCE is now estimated from the lineaged household
records with the project's existing `survey` dependency. The design uses the
NSS first-stage unit as the PSU and nests it within state, sector, stratum and
sub-stratum. For Round 66, the official design has no urban sub-stratification,
so the released blank urban `Sub_Stratum` values are represented internally as
an explicit no-subdivision category; blank rural sub-strata remain invalid.
Sub-round is retained as fieldwork timing metadata but is not promoted to a
sampling stratum. This matches the NSS stratified multistage sample design:
FSUs are selected within the applicable stratum/sub-stratum cells and households
are the ultimate sampling units.

Because household MPCE is a per-person welfare concept, the analysis weight for
mean MPCE is the combined household multiplier multiplied by household size and
by any reviewed lineage allocation weight. Unresolved source districts are not
silently assigned or dropped upstream; they remain visible in the lineage
coverage/review diagnostics and are excluded only at the district-estimation
boundary because they have no Census-2001 target.

The public district-welfare diagnostic is long-form and reports the estimate,
design-based standard error, coefficient of variation where meaningful, raw
household and FSU support, and Kish effective sample size. Thin districts remain
in the output with their precision metadata rather than being removed by an
arbitrary sample threshold. Means and quantiles reuse this survey-design layer;
Gini, Atkinson, poverty and lower-tail means remain later phases rather than
introducing hand-written variance estimators.

### District welfare outcome registry and support flags

Historical district welfare outcomes are declared in
`data/metadata/consumption_welfare_outcomes.csv`. The registry separates the
estimand from diagnostic support rules. Estimates are never dropped merely for
failing a support rule: the output retains the estimate, design standard error,
relative standard error, household and PSU counts, fractional sample-person
equivalent, represented person weight, and Kish effective sample size.
`sample_support_ok`, `precision_ok`, and `preferred_eligible` are therefore
review/specification flags, not upstream filters.

The registry currently declares real mean MPCE (primary), mean log real MPCE
(robustness), and the person-weighted median of real MPCE (robustness). Mean log
MPCE is estimated with the same NSS survey design and person weights after
applying `log()` at the household MPCE level. The median uses
`survey::svyquantile()` at probability 0.5 through the same district-domain
design and retains its design-based standard error. The registry stores the
quantile probability, interval method, and quantile rule explicitly so future
p10/p25 outcomes can use the same estimator without new branches. Quantile
uncertainty uses the logit-scale Woodruff interval (`interval.type = "xlogit"`)
rather than `survey`'s default mean-scale interval. The previously tested beta
interval is bounded in probability space but derives an effective sample size
from the domain variance estimate; thin district domains can yield invalid beta
shape parameters and `NaNs produced`. The xlogit method avoids that beta
effective-sample-size calculation while remaining a standard `survey`
probability-interval method. The mathematical quantile rule
(`qrule = "math"`) remains explicit as well. Warnings are not suppressed; a
non-finite quantile SE remains `not_estimable`. Level-valued outcomes retain
`cv = std_error / estimate`; log-mean rows set `cv` to missing because that ratio
is not a consumption coefficient of variation. `relative_se` is retained
generically for all outcomes.

The initial support thresholds (50 households, 2 PSUs, Kish effective N 20,
and a 20% relative-SE ceiling for the primary level mean) are explicit QA and
preferred-analysis rules rather than survey-theory cutoffs. They are stored in
the registry so sensitivity analyses can vary them transparently without
reconstructing district estimates.
### Quantile inference and thin district domains

Registered district quantiles keep a design-weighted point estimate for every resolved district. Quantile confidence intervals and standard errors are requested only when the district satisfies the registry's ex-ante sample-support thresholds (`min_households`, `min_fsu`, and `min_kish_effective_n`). This is an inference gate, not a data filter: thin districts remain in the public long-form welfare file with `status = "point_estimate_only"`, `uncertainty_requested = FALSE`, and their support diagnostics intact. Supported districts use the registry-declared `survey::svyquantile()` interval and quantile rule. The pipeline does not suppress non-lonely-PSU warnings from supported quantile inference; a warning there remains a strict-build failure. This prevents unstable Woodruff interval calculations in domains that are already declared too thin for preferred inference while preserving their descriptive weighted medians.

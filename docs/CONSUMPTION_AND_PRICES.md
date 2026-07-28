# Consumption and price adjustment

## Main-paper specification after the revision gate

The preferred specification uses the person-weighted mean of real monthly
per-capita household expenditure in each Census 2001 district. Household
expenditure is adjusted before district aggregation using a state, rural/urban,
and survey-period price index. The outcome is the difference in log real
consumption between 2007-08 and 2017-18.

The public headline switch remains conditional on four checks:

1. every household receives a positive price deflator;
2. all state and union-territory substitutions are recorded;
3. the Census 2001 control table has one row per district;
4. current and revised estimates are compared on one common sample.

Until those checks pass, `build_iv_formulas()` continues to reproduce the
current paper. `build_revised_iv_formulas()` defines the proposed replacement
without changing reported estimates prematurely.

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

The production target graph now reads the four CPI files, constructs the monthly state-sector deflator, converts each NSS sub-round to its three survey months, and attaches the arithmetic mean of those monthly deflators to Block 3 household records before district aggregation. The public headline formula remains unchanged until the fixed-sample comparison stage.

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

The extended-diagnostics graph now estimates three 2SLS specifications on one
common district sample: nominal log change, person-weighted real log change,
and real endpoint ANCOVA. All three retain the same non-outcome legacy controls
and state fixed effects so the diagnostic isolates the consequences of the outcome
construction before the unfinished Census 2001 control pipeline changes the
conditioning set. The person-weighted real log-change specification is marked
as preferred; the public headline model remains unchanged until the Census
controls and revision gate are complete.

The same diagnostic target writes household price-assignment coverage by wave,
state, sector, sub-round, assignment type, and donor state, together with a
district-level comparison of nominal, real, household-weighted, and
person-weighted consumption constructions.

The public headline model remains gated until the Census-control revision is complete.

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

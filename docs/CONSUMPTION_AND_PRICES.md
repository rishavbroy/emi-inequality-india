# Consumption and price adjustment

## Main-paper specification after the revision gate

The next main specification will use the person-weighted mean of real monthly
per-capita household expenditure in each Census 2001 district. Household
expenditure is adjusted before district aggregation using a state, rural/urban,
and survey-period price index. The outcome is the difference in log real
consumption between 2007-08 and 2017-18.

The switch is conditional on four checks:

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

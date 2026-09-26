# Historical geography

## Purpose

This module defines how 1991-era language and socioeconomic evidence is related to the Census-2001 district geography used by the main analysis. It is distinct from the modern district-lineage system: the problem here is historical comparability across older boundary regimes.

## Geography before allocation

Historical source values are first validated on their native geography. Aggregation or allocation to later geography occurs only after source accounting and district/state identities are resolved.

## Constant-boundary candidates

The analysis constructs constant-boundary components only from registered transition evidence. A candidate component is admissible when the underlying historical/2001 units can be linked deterministically enough for the intended aggregation. Unresolved or ambiguous transitions remain outside strict constant-boundary comparisons.

## Transition evidence

The historical geography layer draws on official/archival district-change evidence, reviewed boundary metadata, Vanneman geographies, and Kumar--Somanathan transition information where registered. Independent evidence is reconciled in tracked metadata rather than through fuzzy-only matching.

## Deterministic crosswalk

When an exact historical-to-2001 relationship is supported, the crosswalk records the component membership and aggregation rule explicitly. Count variables are aggregated before rates/shares. Derived rates are never averaged across districts without the corresponding numerator/denominator logic.

## Vanneman geography

Vanneman provides an external historical district framework and baseline variables used for robustness/benchmarking. Vanneman constructs remain separately labeled so a result on that geography is not presented as an official Census-2001 district result.

## Population-interpolated G2 geography

The G2 construction provides a population-interpolated harmonized geography for analyses that require comparable historical and modern support beyond exact constant-boundary components. It is a robustness framework, not a replacement for the paper's primary Census-2001 geography.

## Exact-three-vintage certification

Where the analysis claims an exact 1991/2001/2011 three-vintage unit, the registry requires all relevant transitions to be certified. Partial chains are not promoted to exact three-vintage support.

## Validation

The geography layer checks uniqueness, complete component membership, population/count accounting where available, transition consistency across evidence sources, and expected coverage of each declared geography specification.

## Interpretation and limits

Historical harmonization can reduce sample size and changes the estimand from individual districts to stable components. Robustness results on constant-boundary or G2 units should be interpreted as sensitivity to geographic comparability, not as more precise versions of the main specification.

## Implementation

Relevant code is under historical geography/diagnostic modules and `data/metadata/` crosswalks. The broader modern lineage architecture is documented in [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md) and [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md).

## Related documentation

- [`HISTORICAL_LANGUAGE_DATA.md`](HISTORICAL_LANGUAGE_DATA.md)
- [`HISTORICAL_BASELINE_VALIDATION.md`](HISTORICAL_BASELINE_VALIDATION.md)
- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md)

# Price deflation

## Purpose

This module converts nominal consumption measures from different places and survey periods to a common real-consumption scale. It keeps temporal price change and spatial price-level adjustment explicit so each component can be validated separately.

## Goal and reference period

The analysis selects one registered real-price reference and expresses historical/modern nominal MPCE relative to it. Reference values and series choices belong in the price registries/configuration rather than being repeated in model code.

## Spatial price relatives

State and rural/urban spatial relatives are based on the registered Tendulkar poverty-line relationship. Each applicable Tendulkar poverty line is divided by the common all-India rural 2011--12 value returned by `tendulkar_real_poverty_line()`. At the reference period, a nominal amount equal to the applicable Tendulkar poverty line therefore equals that common value on the real-consumption scale. Production constructors obtain the numerical reference from the shared helper rather than repeating it.

Spatial relatives are inputs to the real-consumption construction.

## Temporal price series

Temporal adjustment is assembled from registered official series, principally CPI-RL and CPI-IW for historical periods and state CPI rural/urban series for later periods. Readers preserve the published time units and base information before linking.

## CPI-RL and CPI-IW

CPI-RL supplies rural temporal variation where its coverage and period match the target survey. CPI-IW supplies the historical urban/worker link used by the registered historical consumption construction. Base changes use the shared linking procedure. The production bridge takes the median of valid monthly ratios of the later CPI to the historical CPI within each state and rural/urban sector, implemented once in `price_link_factor()`. International CPI guidance requires an overlap period and commonly illustrates linking at a designated common period; the multi-month median is the aggregation rule selected for this analysis. Review outputs retain the first and last valid overlap ratios alongside the production median and report their exact implied multiplicative effect on real consumption before the switch.

## State CPI rural/urban transition

Later state-level CPI-R/U series are linked to the historical national/sector series through an overlap period declared in the price code. The link uses the registered overlap statistic and validation checks.

## Overlap linking

A linked series is formed only when the required overlap observations exist and are positive and finite. Every historical state and rural/urban series used before the switch must meet the declared minimum number of paired overlap months. The link factor is calculated once by the shared price helper and then applied consistently. Tests verify the declared overlap rule through shared behavior.

## State and union-territory fallback rules

When a state and sector series is unavailable, the code uses only the declared donor/fallback hierarchy. If a target series inherits a donor state's CPI history, its temporal normalization uses that donor's reference period index as well, so the numerator and denominator belong to the same CPI series. Output metadata identify observations that use a declared fallback instead of a directly observed state and sector index.

## NSS sub-round aggregation

Survey-period prices are aligned to the months/sub-rounds covered by the NSS round. Household or district real-consumption construction uses the period-specific deflator implied by the survey timing.

## Validation and failure conditions

The price layer validates positive finite index values, overlap coverage, reference normalization, donor/fallback declarations, and complete state and rural/urban coverage for the observations admitted to the preferred analysis. Missing required links fail in final mode; optional sensitivity series may instead return an explicit unavailable status.

## Outputs

Price inputs and derived links are saved where they are needed for review. Paper outputs use the real-consumption variables produced by the measurement layer.

## Interpretation and limits

The deflator creates a comparable real-consumption metric under the declared spatial and temporal price assumptions. Differences in consumption concepts across survey rounds remain and are handled through the measurement definitions. Sensitivity to alternative welfare/price constructions belongs in [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md).

## Implementation

Primary implementation is in `R/prices/`, with source readers under `R/io/` and price metadata under `data/metadata/`. Keep price-series selection, overlap rules, and donor logic centralized there.

## Related documentation

- [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)
- [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md)

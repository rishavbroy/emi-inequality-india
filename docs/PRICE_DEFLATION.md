# Price deflation

## Purpose

This module converts nominal consumption measures from different places and survey periods to a common real-consumption scale. It keeps temporal price change and spatial price-level adjustment explicit so each component can be validated separately.

## Goal and reference period

The analysis selects one registered real-price reference and expresses historical/modern nominal MPCE relative to it. Reference values and series choices belong in the price registries/configuration rather than being repeated in model code.

## Spatial price relatives

State-sector spatial relatives anchor rural/urban purchasing-power differences. Historical spatial adjustment uses the registered Tendulkar poverty-line relationship where that is the declared basis. State/sector values are normalized to the selected reference before they are combined with temporal series.

Spatial relatives are measurement assumptions; they are not estimated from the paper's outcome regressions.

## Temporal price series

Temporal adjustment is assembled from registered official series, principally CPI-RL and CPI-IW for historical periods and state CPI rural/urban series for later periods. Readers preserve the published time units and base information before linking.

## CPI-RL and CPI-IW

CPI-RL supplies rural temporal variation where its coverage and period match the target survey. CPI-IW supplies the historical urban/worker link used by the registered historical consumption construction. Base changes are handled explicitly rather than by concatenating index levels from incompatible bases.

## State CPI rural/urban transition

Later state-level CPI-R/U series are linked to the historical national/sector series through an overlap period declared in the price code. The link uses the registered overlap statistic and validation checks; it should not be changed merely to improve a downstream coefficient.

## Overlap linking

A linked series is formed only when the required overlap observations exist and are finite/positive. The link factor is calculated once by the shared price helper and then applied consistently. Tests should compare behavior to the declared overlap rule rather than duplicate the arithmetic in multiple modules.

## State and union-territory fallback rules

When a state-sector series is unavailable, the code uses only the declared donor/fallback hierarchy. Fallbacks must remain visible in the output metadata so an observation is never mistaken for a directly observed state-sector index.

## NSS sub-round aggregation

Survey-period prices are aligned to the months/sub-rounds covered by the NSS round. Household or district real-consumption construction uses the period-specific deflator implied by that survey timing, not an annual index chosen after aggregation.

## Validation and failure conditions

The price layer validates positive finite index values, overlap coverage, reference normalization, donor/fallback declarations, and complete state-sector coverage for the observations admitted to the preferred analysis. Missing required links fail in final mode; optional sensitivity series may instead return an explicit unavailable status.

## Outputs

Price inputs and derived links are persisted only where they are needed for audit/review. Paper-facing outputs consume real-consumption variables from the measurement layer rather than embedding price-series manipulation in table code.

## Interpretation and limits

The deflator creates a comparable real-consumption metric under the declared spatial and temporal price assumptions. It does not eliminate all differences in consumption concepts across survey rounds. Sensitivity to alternative welfare/price constructions belongs in [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md).

## Implementation

Primary implementation is in `R/prices/`, with source readers under `R/io/` and price metadata under `data/metadata/`. Keep price-series selection, overlap rules, and donor logic centralized there.

## Related documentation

- [`CONSUMPTION_MEASUREMENT.md`](CONSUMPTION_MEASUREMENT.md)
- [`CONSUMPTION_ANALYSIS.md`](CONSUMPTION_ANALYSIS.md)
- [`../DATA_AVAILABILITY.md`](../DATA_AVAILABILITY.md)

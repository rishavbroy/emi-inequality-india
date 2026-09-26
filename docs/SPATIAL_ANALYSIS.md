# Spatial analysis

## Purpose

Spatial diagnostics test whether district variables or fitted-model residuals retain geographic autocorrelation on the reviewed Census-2001 geometry. They are model-adequacy and descriptive dependence checks; the preferred paper estimator is not a spatial IV model.

## Geometry

The analysis uses the reviewed Census-2001 district polygons attached to the main district panel. Geometry preparation and district identity are governed by the lineage/geography modules rather than by spatial diagnostics.

## Weight construction

The preferred neighborhood definition is rook contiguity with row-standardized weights (`style = "W"`). Queen contiguity is a sensitivity check for residual Moran results.

## Islands and zero-neighbor policy

Lakshadweep, South Andaman, and Nicobars are genuine offshore islands in the reference geometry. They remain in the dataset with `zero.policy = TRUE`. Their zero-neighbor status is not evidence that polygon snap tolerance should be inflated until every district receives a neighbor.

## Moran diagnostics

The diagnostic layer computes Moran tests for selected variables and model residuals on the exact fitted rows. Residual alignment should reuse the shared fitted-sample/index machinery so factor/transformation handling and row exclusions match the fitted model.

Asymptotic Moran tests are the routine diagnostic. Monte Carlo checks may be used as sensitivity analysis when computationally justified; if reported, the simulation count and random-seed policy should be explicit.

## Experimental spatial IV

`R/iv/estimate_spatial_iv_experimental.R` is experimental. Spatially lagged endogenous variables change the identifying assumptions, so these estimates are not promoted to the paper merely because residual spatial dependence is detected.

## Interpretation boundary

Spatial autocorrelation can reveal omitted geographic structure or residual dependence. It does not by itself identify the correct spatial causal model, nor does a non-significant residual Moran test prove the absence of spatial confounding.

## Outputs and tests

Spatial-weight and residual diagnostics are retained under the diagnostic output tree. Tests should protect neighbor/weight behavior, island handling, fitted-row alignment, and model residual use rather than implementation text.

## Implementation

- `R/diagnostics/diagnose_spatial_weights.R`
- `R/diagnostics/diagnose_spatial_autocorrelation.R`
- `R/iv/estimate_spatial_iv_experimental.R`
- optional timing comparisons under `R/benchmarking/`

## Related documentation

- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)

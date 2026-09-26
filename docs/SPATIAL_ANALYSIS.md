# Spatial analysis

## Purpose

Spatial checks test whether district variables or fitted-model residuals retain geographic autocorrelation on the reviewed 2001 Census geometry. These checks assess geographic dependence around the paper's non-spatial preferred estimator.

## Geometry

The analysis uses the reviewed 2001 Census district polygons attached to the main district panel. The lineage and geography modules determine district identity and prepare the geometry used here.

## Weight construction

The preferred neighborhood definition is rook contiguity with row-standardized weights (`style = "W"`). Queen contiguity is a sensitivity check for residual Moran results.

## Islands and zero-neighbor policy

Lakshadweep, South Andaman, and Nicobars are genuine offshore islands in the reference geometry. They remain in the dataset with zero neighbors under `zero.policy = TRUE`.

## Moran checks

The spatial-analysis code computes Moran tests for selected variables and model residuals on the exact fitted rows. Residual alignment should reuse the shared fitted-sample/index machinery so factor/transformation handling and row exclusions match the fitted model.

Asymptotic Moran tests are the routine check. Monte Carlo versions are used as sensitivity analyses when reported, with the simulation count and random-seed policy recorded.

## Experimental spatial IV

`R/iv/estimate_spatial_iv_experimental.R` is experimental. Spatially lagged endogenous variables require additional identifying assumptions and are evaluated separately from the paper's preferred estimator.

## Interpretation boundary

Spatial autocorrelation can reveal omitted geographic structure or residual dependence. Interpreting that pattern causally requires a separately specified spatial model; a residual Moran test alone does not establish such a model.

## Outputs and tests

Spatial-weight and residual checks are retained under the validation outputs. Tests protect neighbor and weight behavior, island handling, fitted-row alignment, and model residual use.

## Implementation

- `R/diagnostics/diagnose_spatial_weights.R`
- `R/diagnostics/diagnose_spatial_autocorrelation.R`
- `R/iv/estimate_spatial_iv_experimental.R`
- optional timing comparisons under `R/benchmarking/`

## Related documentation

- [`DISTRICT_LINEAGE.md`](DISTRICT_LINEAGE.md)
- [`GEOGRAPHY_HARMONIZATION.md`](GEOGRAPHY_HARMONIZATION.md)
- [`IV_DIAGNOSTICS.md`](IV_DIAGNOSTICS.md)

# Spatial analysis

Spatial analysis uses the reviewed Census-2001 district geometry carried by the main district panel. The principal implementation is in:

- `R/diagnostics/diagnose_spatial_weights.R`, which constructs polygon-contiguity neighbours and row-standardized weights;
- `R/diagnostics/diagnose_spatial_autocorrelation.R`, which computes Moran tests for selected variables and model residuals;
- `R/benchmarking/benchmarking_targets.R`, which exposes the optional rook/queen timing comparison; and
- `R/iv/estimate_spatial_iv_experimental.R`, which remains experimental and is not part of the paper's preferred estimation strategy.

The preferred spatial weights use rook contiguity and `style = "W"`. Three districts in the reference geometry are genuine offshore islands: Lakshadweep, South Andaman, and Nicobars. They are retained with `zero.policy = TRUE`; their lack of polygon-contiguous neighbours is not, by itself, treated as evidence that the polygon snap tolerance should be increased. Queen contiguity is used as a sensitivity check for the residual Moran tests.

The paper reports spatial clustering descriptively and uses residual Moran tests as a model-adequacy check. The spatial-IV code is intentionally separate because adding spatially lagged endogenous variables changes the identifying requirements and the current experimental specifications do not support promotion to the main analysis.

Tests for the spatial-weight construction are in `tests/testthat/test-spatial-weights.R`; the broader spatial and residual checks are covered in `tests/testthat/test-diagnostics.R`.

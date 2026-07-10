# Spatial diagnostics context

This note is intentionally kept under `docs/refactor/` rather than in the paper, README, or other visible-facing project overview. It records why current spatial diagnostic outputs should be read as active-pipeline results rather than exact legacy-object parity.

## Spatial weights

The legacy Rmd's commented rook/queen comparison recorded mean neighbor counts of approximately 4.780165 for rook contiguity and 4.783471 for queen contiguity. The refactored diagnostics preserve those legacy comments as reference values in `outputs/diagnostics/extended/spatial/rook_queen_contiguity_comparison.csv` and compute current deltas against the active matched district panel.

Current counts can differ because the active object is the refactored district panel with the current geometry join, not the exploratory legacy object used when the comments were written. A lower current mean neighbor count should therefore be treated as a reconciliation question, not as either automatic evidence of a bug or automatic evidence of improvement.

## Moran's I

The refactored public diagnostic reproduces the legacy method choices: rook contiguity, `spdep::nb2listw(..., style = "W", zero.policy = TRUE)`, and `spdep::moran.test(..., zero.policy = TRUE)`. The current paper consumes the active-pipeline p-values for `m_cons_resid` and `m_cons` through `report_values`.

The legacy comments recorded more extreme p-values for several tests. Some of those comments explicitly refer to residual diagnostics before additional controls were added. The current values should therefore be described as current active-pipeline diagnostics that preserve the qualitative conclusion of strong spatial autocorrelation, not as exact numeric parity with every legacy comment.

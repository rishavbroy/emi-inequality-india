# Spatial Autocorrelation Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

The legacy spatial-autocorrelation chunk used rook contiguity, a binary
adjacency matrix, row-standardized weights, and
`moran.test(..., zero.policy = TRUE)`. It checked IV residuals,
first-stage residuals, `EMIE`, linguistic distance, consumption growth,
and Gini growth, while also leaving a Monte Carlo Moran scaffold
commented out.

``` r
moran <- analysis_target_csv("diag_public_spatial_autocorrelation", "spatial_moran_tests.csv")
```

The current `m_cons_resid` p-value is NA, and the current `m_cons`
p-value is NA. These are current active-pipeline results, not hard-coded
legacy comments.

``` r
if ("legacy_name" %in% names(moran)) {
  keep <- moran$legacy_name %in% c("m_cons_resid", "m_cons", "m_EMIE", "m_wavg_ling_degrees")
  cols <- intersect(c("legacy_name", "statistic", "estimate", "p.value", "legacy_note"), names(moran))
  moran[keep, cols, drop = FALSE]
} else {
  moran
}
```

                                                                                      note
    1 Target output not found: diag_public_spatial_autocorrelation spatial_moran_tests.csv

``` r
analysis_table(moran, "Current Moran's I diagnostics")
```

| note |
|:---|
| Target output not found: diag_public_spatial_autocorrelation spatial_moran_tests.csv |

Current Moran’s I diagnostics

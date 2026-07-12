# Spatial Autocorrelation Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Build row-standardized spatial weights (`listw`) object.
`zero.policy = TRUE` lets it handle any islands, zero-neighbor units.
Extract residuals from IV models. Global Moran’s I on residuals.

Before more controls were added in: `m_cons_resid$p.value` =
2.779572e-23, `m_gini_resid$p.value` = 2.033012e-40,
`m_fscons_resid$p.value` = 1.189148e-105, and `m_fsgini_resid$p.value` =
1.189148e-105; obviously the same.

Moran’s I on explanatory variable and IV: `m_EMIE$p.value` =
8.990354e-180 and `m_wavg_ling_degrees$p.value` = 1.721903e-254. Each of
the following are named; put them in `unname()` to get the raw number:
`m_EMIE$statistic` = z-score; `m_EMIE$estimate[1]` = Moran’s I
statistic; `m_EMIE$estimate[2]` = expected value under the null;
`m_EMIE$estimate[3]` = variance under the null.

Test Moran’s I on the response variables: `m_cons$p.value` =
1.608813e-26 and `m_gini$p.value` = 8.51626e-22. Repeat for controls
which may have a strong degree of spatial autocorrelation
(infrastructure, poverty, etc.). View all the above statistics’
p-values. All of these Moran’s I stats are ridiculously, suspiciously
high.

Estimate p-values using Monte Carlo. `moran.test()` assumes asymptotic
normality. The legacy scaffold used `set.seed(999)`, `num_m = 9999`,
`moran.mc(resid_cons, listw_2020, nsim = num_m)`, `plot(mc)`, and
`mc$p.value`.

``` r
analysis_deviation_note("The current note keeps the legacy Moran prose and benchmark p-values, but reports active-pipeline Moran tests from the target output instead of copying legacy comments into the result fields.")
```

**Deviation note.** The current note keeps the legacy Moran prose and
benchmark p-values, but reports active-pipeline Moran tests from the
target output instead of copying legacy comments into the result fields.

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

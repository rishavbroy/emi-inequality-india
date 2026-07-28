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
analysis_deviation_note("The current note keeps the legacy Moran prose and benchmark p-values, but reports active-pipeline Moran tests from the target output instead of copying legacy comments into the result fields. The current analysis note retains the Monte Carlo scaffold as a target-backed reference row and explicitly marks the deviation from the legacy prose: the pipeline no longer maintains functionality for refreshing the expensive `moran.mc(..., nsim = 9999)` benchmark.")
```

**Deviation note.** The current note keeps the legacy Moran prose and
benchmark p-values, but reports active-pipeline Moran tests from the
target output instead of copying legacy comments into the result fields.
The current analysis note retains the Monte Carlo scaffold as a
target-backed reference row and explicitly marks the deviation from the
legacy prose: the pipeline no longer maintains functionality for
refreshing the expensive `moran.mc(..., nsim = 9999)` benchmark.

``` r
moran <- analysis_target_csv("diag_public_spatial_autocorrelation_files", "spatial_moran_tests.csv")
moran_mc <- analysis_target_csv("diag_public_spatial_autocorrelation_files", "spatial_moran_mc_reference.csv")
```

The current `m_cons_resid` p-value is 0.159344, and the current `m_cons`
p-value is 0. These are current active-pipeline results, not hard-coded
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

              legacy_name  statistic   estimate       p.value
    1        m_cons_resid  0.9971575 0.02500986  1.593440e-01
    5              m_EMIE 24.7195075 0.66044665 3.299595e-135
    6 m_wavg_ling_degrees 34.0723274 0.91455175 9.479860e-255
    7              m_cons 11.5306860 0.30758588  4.620220e-31
                                                                                                                       legacy_note
    1 Final-paper residual p-value: residuals(model_consumption_iv). Legacy comments reported a pre-control value of 2.779572e-23.
    5                                                                         Legacy comments reported p = 8.990354e-180 for EMIE.
    6                                         Legacy comments reported p = 1.721903e-254 for weighted average linguistic distance.
    7                              Final-paper outcome p-value: consumption_pct_change. Legacy comments reported p = 1.608813e-26.

``` r
analysis_table(moran, "Current Moran's I diagnostics")
```

| legacy_name | estimand | variable | source | test | status | statistic | estimate | expected | variance | p.value | method | alternative | n | contiguity | weights_style | matrix_style | zero_policy | n_spatial_rows | n_islands | mean_neighbors | warnings | reason | legacy_note |
|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|:---|:---|---:|:---|:---|:---|:---|---:|---:|---:|:---|:---|:---|
| m_cons_resid | consumption_iv_residual | resid_cons | second_stage_residual | moran | estimated | 0.997 | 0.025 | -0.002 | 0.001 | 0.159 | Moran I test under randomisation | greater | 573 | rook | W | B | TRUE | 573 | 3 | 5.204 | some observations have no neighbours; |  |  |

Current Moran’s I diagnostics

if this seems unexpected, try increasing the snap argument.; neighbour
object has 4 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Final-paper residual p-value:
residuals(model_consumption_iv). Legacy comments reported a pre-control
value of 2.779572e-23. \| \|m_gini_resid \|gini_iv_residual \|resid_gini
\|second_stage_residual \|moran \|estimated \| 0.997\| 0.025\| -0.002\|
0.001\| 0.159\|Moran I test under randomisation \|greater \| 573\|rook
\|W \|B \|TRUE \| 573\| 3\| 5.204\|some observations have no neighbours;
if this seems unexpected, try increasing the snap argument.; neighbour
object has 4 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Legacy residual diagnostic:
residuals(model_gini_iv). Legacy comments reported a pre-control value
of 2.033012e-40. \| \|m_fscons_resid \|consumption_first_stage_residual
\|resid_fscons \|first_stage_residual \|moran \|estimated \| 0.035\|
-0.001\| -0.002\| 0.001\| 0.486\|Moran I test under randomisation
\|greater \| 573\|rook \|W \|B \|TRUE \| 573\| 3\| 5.204\|some
observations have no neighbours; if this seems unexpected, try
increasing the snap argument.; neighbour object has 4 sub-graphs; if
this sub-graph count seems unexpected, try increasing the snap argument.
\|NA \|Legacy first-stage residual diagnostic:
residuals(first_stage_consumption). Legacy comments reported a
pre-control value of 1.189148e-105. \| \|m_fsgini_resid
\|gini_first_stage_residual \|resid_fsgini \|first_stage_residual
\|moran \|estimated \| 0.035\| -0.001\| -0.002\| 0.001\| 0.486\|Moran I
test under randomisation \|greater \| 573\|rook \|W \|B \|TRUE \| 573\|
3\| 5.204\|some observations have no neighbours; if this seems
unexpected, try increasing the snap argument.; neighbour object has 4
sub-graphs; if this sub-graph count seems unexpected, try increasing the
snap argument. \|NA \|Legacy first-stage residual diagnostic:
residuals(first_stage_gini). Legacy comments noted the same pre-control
value as first-stage consumption. \| \|m_EMIE \|emie \|EMIE \|treatment
\|moran \|estimated \| 24.720\| 0.660\| -0.002\| 0.001\| 0.000\|Moran I
test under randomisation \|greater \| 573\|rook \|W \|B \|TRUE \| 573\|
3\| 5.204\|some observations have no neighbours; if this seems
unexpected, try increasing the snap argument.; neighbour object has 4
sub-graphs; if this sub-graph count seems unexpected, try increasing the
snap argument. \|NA \|Legacy comments reported p = 8.990354e-180 for
EMIE. \| \|m_wavg_ling_degrees \|linguistic_distance \|wavg_ling_degrees
\|instrument \|moran \|estimated \| 34.072\| 0.915\| -0.002\| 0.001\|
0.000\|Moran I test under randomisation \|greater \| 573\|rook \|W \|B
\|TRUE \| 573\| 3\| 5.204\|some observations have no neighbours; if this
seems unexpected, try increasing the snap argument.; neighbour object
has 4 sub-graphs; if this sub-graph count seems unexpected, try
increasing the snap argument. \|NA \|Legacy comments reported p =
1.721903e-254 for weighted average linguistic distance. \| \|m_cons
\|real_consumption_growth \|real_log_consumption_change \|outcome
\|moran \|estimated \| 11.531\| 0.308\| -0.002\| 0.001\| 0.000\|Moran I
test under randomisation \|greater \| 573\|rook \|W \|B \|TRUE \| 573\|
3\| 5.204\|some observations have no neighbours; if this seems
unexpected, try increasing the snap argument.; neighbour object has 4
sub-graphs; if this sub-graph count seems unexpected, try increasing the
snap argument. \|NA \|Final-paper outcome p-value:
consumption_pct_change. Legacy comments reported p = 1.608813e-26. \|
\|m_gini \|gini_change \|gini_change \|outcome \|moran \|estimated \|
5.394\| 0.143\| -0.002\| 0.001\| 0.000\|Moran I test under randomisation
\|greater \| 573\|rook \|W \|B \|TRUE \| 573\| 3\| 5.204\|some
observations have no neighbours; if this seems unexpected, try
increasing the snap argument.; neighbour object has 4 sub-graphs; if
this sub-graph count seems unexpected, try increasing the snap argument.
\|NA \|Legacy outcome diagnostic: gini_change. Legacy comments reported
p = 8.51626e-22. \|

``` r
analysis_table(moran_mc, "Monte Carlo Moran scaffold retained as target-backed reference")
```

| scaffold | status | reason |
|:---|:---|:---|
| moran.mc(resid_cons, listw_2020, nsim = 9999) | documented_not_run_by_default | Legacy Chunk 29 kept this as a Monte Carlo robustness scaffold. The current pipeline deliberately documents this deviation instead of maintaining full-sample Monte Carlo benchmark functionality; active results use the asymptotic moran.test() path used by report_values. |

Monte Carlo Moran scaffold retained as target-backed reference

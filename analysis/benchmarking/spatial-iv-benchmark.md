# Spatial IV Benchmark


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Build spatial lags of dependent variables: `W_consY` and `W_giniY`.
Rebuild the spatial lags of endogenous `EMIE` and IV: `W_EMIE` and
`W_wLing`. Build the second-order lag `W2_wLing`. Rebuild spatial lags
of exogenous controls. Spatial-2SLS for consumption change, also
instrumenting `W_consY`.

The legacy comments left
`summary(model_sdm2sls_cons, diagnostics = TRUE)` and
`summary(model_sdm2sls_gini, diagnostics = TRUE)` commented out and
concluded: Don’t work even when diagnostics = FALSE.

Region clustered standard errors.
`vcovCL(..., cluster = ~ region, data = joined_df)`. HC0 by default.
Then `coeftest(model_sdm2sls_cons, vcov. = vcov_cluster_cons)` and
`coeftest(model_sdm2sls_gini, vcov. = vcov_cluster_gini)`.

``` r
analysis_deviation_note("The current benchmark preserves the legacy experimental specifications and clustered-SE attempt, but separates ivreg object creation from diagnostic numerical suitability.")
```

**Deviation note.** The current benchmark preserves the legacy
experimental specifications and clustered-SE attempt, but separates
ivreg object creation from diagnostic numerical suitability.

``` r
spatial_iv_status <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_model_status.csv")
spatial_iv_diag <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_diagnostics_summary.csv")
spatial_iv_coef <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_coefficient_summary.csv")
spatial_iv_cluster <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_clustered_coefficient_summary.csv")
spatial_iv_fail <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_failure_summary.csv")
```

The current benchmark estimates 2 experimental specifications. A status
of `estimated` means that `ivreg()` returned an object; the
`methodological_success` field is the stricter current analog of the
legacy “Don’t work even when diagnostics = FALSE” conclusion because it
requires diagnostics and clustered-SE extraction to succeed as well.

``` r
analysis_table(spatial_iv_status[setdiff(names(spatial_iv_status), "formula")], "Spatial-IV model status")
```

| model | status | methodological_success | reason | nobs | diagnostics_status | cluster_se_status |
|:---|:---|:---|:---|---:|:---|:---|
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 9.97406e-18 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 2.90551e-19 | estimated |

Spatial-IV model status

``` r
spatial_iv_status[, c("model", "formula"), drop = FALSE]
```

                   model
    1 model_sdm2sls_cons
    2 model_sdm2sls_gini
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             formula
    1 consumption_pct_change ~ W_consY + EMIE + W_EMIE + npeople_0708 +      nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban +      pct_head_secondary_plus + pct_muslim + pct_st + pct_obc +      pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 +      W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 |      wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 +          consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus +          pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land +          pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 +          W_gini_cons_0708
    2                                 gini_change ~ W_giniY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 +      consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus +      pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land +      pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 +      W_gini_cons_0708 | wavg_ling_degrees + W_wLing + W2_wLing +      npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 +      pct_urban + pct_head_secondary_plus + pct_muslim + pct_st +      pct_obc + pct_fem_head + pct_medium_land + pct_large_land +      W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708

``` r
analysis_table(spatial_iv_diag, "IV diagnostic summaries")
```

| model | status | reason |
|:---|:---|:---|
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 9.97406e-18 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 2.90551e-19 |

IV diagnostic summaries

``` r
analysis_table(spatial_iv_coef, "Default coefficient summaries")
```

| model              | vcov_type     | term                    | estimate |
|:-------------------|:--------------|:------------------------|---------:|
| model_sdm2sls_cons | model_default | (Intercept)             | -262.401 |
| model_sdm2sls_cons | model_default | W_consY                 |    3.329 |
| model_sdm2sls_cons | model_default | EMIE                    |    5.358 |
| model_sdm2sls_cons | model_default | W_EMIE                  |   -6.653 |
| model_sdm2sls_cons | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |   -0.134 |
| model_sdm2sls_cons | model_default | gini_cons_0708          |  -33.019 |
| model_sdm2sls_cons | model_default | pct_urban               |   -0.904 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |    0.702 |
| model_sdm2sls_cons | model_default | pct_muslim              |    0.507 |
| model_sdm2sls_cons | model_default | pct_st                  |    0.526 |
| model_sdm2sls_cons | model_default | pct_obc                 |    0.417 |
| model_sdm2sls_cons | model_default | pct_fem_head            |    0.333 |
| model_sdm2sls_cons | model_default | pct_medium_land         |    1.316 |
| model_sdm2sls_cons | model_default | pct_large_land          |   -3.179 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |    0.172 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -784.305 |
| model_sdm2sls_gini | model_default | (Intercept)             |    0.103 |
| model_sdm2sls_gini | model_default | W_giniY                 |   -0.585 |
| model_sdm2sls_gini | model_default | EMIE                    |   -0.001 |
| model_sdm2sls_gini | model_default | W_EMIE                  |    0.001 |
| model_sdm2sls_gini | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |    0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |   -0.684 |
| model_sdm2sls_gini | model_default | pct_urban               |    0.001 |
| model_sdm2sls_gini | model_default | pct_head_secondary_plus |    0.001 |
| model_sdm2sls_gini | model_default | pct_muslim              |    0.000 |
| model_sdm2sls_gini | model_default | pct_st                  |    0.001 |
| model_sdm2sls_gini | model_default | pct_obc                 |    0.000 |
| model_sdm2sls_gini | model_default | pct_fem_head            |    0.001 |
| model_sdm2sls_gini | model_default | pct_medium_land         |    0.000 |
| model_sdm2sls_gini | model_default | pct_large_land          |   -0.001 |
| model_sdm2sls_gini | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_consumption_0708      |    0.000 |
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |    0.116 |

Default coefficient summaries

``` r
analysis_table(spatial_iv_cluster, "Clustered-SE coeftest attempt")
```

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -262.401 | 1015.397 | -0.258 | 0.796 |
| model_sdm2sls_cons | estimated | region | W_consY | 3.329 | 6.672 | 0.499 | 0.618 |
| model_sdm2sls_cons | estimated | region | EMIE | 5.358 | 8.516 | 0.629 | 0.530 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -6.653 | 13.111 | -0.507 | 0.612 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.466 | 0.641 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.001 | -0.434 | 0.664 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.134 | 0.064 | -2.104 | 0.036 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | -33.019 | 128.540 | -0.257 | 0.797 |
| model_sdm2sls_cons | estimated | region | pct_urban | -0.904 | 3.804 | -0.238 | 0.812 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | 0.702 | 2.823 | 0.249 | 0.804 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.507 | 1.758 | 0.289 | 0.773 |
| model_sdm2sls_cons | estimated | region | pct_st | 0.526 | 2.967 | 0.177 | 0.859 |
| model_sdm2sls_cons | estimated | region | pct_obc | 0.417 | 1.929 | 0.216 | 0.829 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 0.333 | 2.063 | 0.161 | 0.872 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 1.316 | 2.754 | 0.478 | 0.633 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -3.179 | 8.302 | -0.383 | 0.702 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.585 | 0.559 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 0.604 | 0.546 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.172 | 0.253 | 0.677 | 0.498 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -784.305 | 1904.714 | -0.412 | 0.681 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.103 | 0.075 | 1.370 | 0.171 |
| model_sdm2sls_gini | estimated | region | W_giniY | -0.585 | 3.627 | -0.161 | 0.872 |
| model_sdm2sls_gini | estimated | region | EMIE | -0.001 | 0.005 | -0.273 | 0.785 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.001 | 0.005 | 0.268 | 0.789 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | -0.081 | 0.935 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | 0.065 | 0.948 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | -0.098 | 0.922 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.684 | 0.345 | -1.979 | 0.048 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.001 | 0.002 | 0.553 | 0.580 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.001 | 0.001 | 0.634 | 0.526 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.000 | -0.061 | 0.951 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.001 | 0.001 | 0.435 | 0.664 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.000 | -0.907 | 0.365 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.001 | 0.003 | 0.210 | 0.834 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -0.901 | 0.368 |
| model_sdm2sls_gini | estimated | region | pct_large_land | -0.001 | 0.002 | -0.374 | 0.708 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | 0.146 | 0.884 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.340 | 0.734 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | -1.469 | 0.142 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | 0.116 | 1.460 | 0.079 | 0.937 |

Clustered-SE coeftest attempt

``` r
analysis_table(spatial_iv_fail, "Failure/status summary")
```

| status    |   n |
|:----------|----:|
| estimated |   2 |

Failure/status summary

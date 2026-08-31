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
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 2.23485e-17 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 3.40683e-18 | estimated |

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
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 2.23485e-17 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 3.40683e-18 |

IV diagnostic summaries

``` r
analysis_table(spatial_iv_coef, "Default coefficient summaries")
```

| model              | vcov_type     | term                    | estimate |
|:-------------------|:--------------|:------------------------|---------:|
| model_sdm2sls_cons | model_default | (Intercept)             | -164.879 |
| model_sdm2sls_cons | model_default | W_consY                 |    2.611 |
| model_sdm2sls_cons | model_default | EMIE                    |    3.244 |
| model_sdm2sls_cons | model_default | W_EMIE                  |   -4.601 |
| model_sdm2sls_cons | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |   -0.145 |
| model_sdm2sls_cons | model_default | gini_cons_0708          |   28.818 |
| model_sdm2sls_cons | model_default | pct_urban               |   -0.304 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |    0.573 |
| model_sdm2sls_cons | model_default | pct_muslim              |    0.366 |
| model_sdm2sls_cons | model_default | pct_st                  |    0.475 |
| model_sdm2sls_cons | model_default | pct_obc                 |    0.103 |
| model_sdm2sls_cons | model_default | pct_fem_head            |    0.482 |
| model_sdm2sls_cons | model_default | pct_medium_land         |    0.788 |
| model_sdm2sls_cons | model_default | pct_large_land          |   -2.050 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |    0.200 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -838.973 |
| model_sdm2sls_gini | model_default | (Intercept)             |    0.086 |
| model_sdm2sls_gini | model_default | W_giniY                 |    0.737 |
| model_sdm2sls_gini | model_default | EMIE                    |    0.000 |
| model_sdm2sls_gini | model_default | W_EMIE                  |    0.000 |
| model_sdm2sls_gini | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |    0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |   -0.769 |
| model_sdm2sls_gini | model_default | pct_urban               |    0.001 |
| model_sdm2sls_gini | model_default | pct_head_secondary_plus |    0.000 |
| model_sdm2sls_gini | model_default | pct_muslim              |    0.000 |
| model_sdm2sls_gini | model_default | pct_st                  |    0.000 |
| model_sdm2sls_gini | model_default | pct_obc                 |    0.000 |
| model_sdm2sls_gini | model_default | pct_fem_head            |    0.000 |
| model_sdm2sls_gini | model_default | pct_medium_land         |    0.000 |
| model_sdm2sls_gini | model_default | pct_large_land          |    0.000 |
| model_sdm2sls_gini | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_consumption_0708      |    0.000 |
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |    0.611 |

Default coefficient summaries

``` r
analysis_table(spatial_iv_cluster, "Clustered-SE coeftest attempt")
```

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -164.879 | 237.392 | -0.695 | 0.488 |
| model_sdm2sls_cons | estimated | region | W_consY | 2.611 | 1.374 | 1.900 | 0.058 |
| model_sdm2sls_cons | estimated | region | EMIE | 3.244 | 4.433 | 0.732 | 0.465 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -4.601 | 5.541 | -0.830 | 0.407 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.000 | 1.125 | 0.261 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.000 | -1.037 | 0.300 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.145 | 0.032 | -4.595 | 0.000 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | 28.818 | 94.234 | 0.306 | 0.760 |
| model_sdm2sls_cons | estimated | region | pct_urban | -0.304 | 1.735 | -0.175 | 0.861 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | 0.573 | 0.957 | 0.599 | 0.549 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.366 | 0.674 | 0.543 | 0.588 |
| model_sdm2sls_cons | estimated | region | pct_st | 0.475 | 1.052 | 0.452 | 0.652 |
| model_sdm2sls_cons | estimated | region | pct_obc | 0.103 | 0.325 | 0.318 | 0.750 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 0.482 | 1.125 | 0.428 | 0.669 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 0.788 | 0.496 | 1.588 | 0.113 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -2.050 | 2.388 | -0.859 | 0.391 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -1.196 | 0.232 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 1.041 | 0.299 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.200 | 0.138 | 1.447 | 0.148 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -838.973 | 545.906 | -1.537 | 0.125 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.086 | 0.049 | 1.732 | 0.084 |
| model_sdm2sls_gini | estimated | region | W_giniY | 0.737 | 0.434 | 1.700 | 0.090 |
| model_sdm2sls_gini | estimated | region | EMIE | 0.000 | 0.001 | -0.020 | 0.984 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.000 | 0.001 | 0.004 | 0.997 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.704 | 0.482 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | -0.278 | 0.781 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | 0.410 | 0.682 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.769 | 0.053 | -14.514 | 0.000 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.001 | 0.001 | 0.850 | 0.396 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.000 | 0.000 | 1.149 | 0.251 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.000 | -0.248 | 0.805 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.000 | 0.000 | 0.275 | 0.784 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.000 | -2.503 | 0.013 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.000 | 0.001 | -0.320 | 0.749 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -0.774 | 0.439 |
| model_sdm2sls_gini | estimated | region | pct_large_land | 0.000 | 0.001 | 0.222 | 0.825 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.194 | 0.846 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.216 | 0.829 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | -1.282 | 0.200 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | 0.611 | 0.202 | 3.018 | 0.003 |

Clustered-SE coeftest attempt

``` r
analysis_table(spatial_iv_fail, "Failure/status summary")
```

| status    |   n |
|:----------|----:|
| estimated |   2 |

Failure/status summary

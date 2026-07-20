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
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 482 | failed: system is computationally singular: reciprocal condition number = 1.6623e-17 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 482 | failed: system is computationally singular: reciprocal condition number = 2.57101e-19 | estimated |

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
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 1.6623e-17 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 2.57101e-19 |

IV diagnostic summaries

``` r
analysis_table(spatial_iv_coef, "Default coefficient summaries")
```

| model              | vcov_type     | term                    | estimate |
|:-------------------|:--------------|:------------------------|---------:|
| model_sdm2sls_cons | model_default | (Intercept)             | -634.222 |
| model_sdm2sls_cons | model_default | W_consY                 |    3.727 |
| model_sdm2sls_cons | model_default | EMIE                    |   15.079 |
| model_sdm2sls_cons | model_default | W_EMIE                  |  -14.633 |
| model_sdm2sls_cons | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |   -0.093 |
| model_sdm2sls_cons | model_default | gini_cons_0708          | -173.638 |
| model_sdm2sls_cons | model_default | pct_urban               |   -3.247 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |   -1.596 |
| model_sdm2sls_cons | model_default | pct_muslim              |    0.211 |
| model_sdm2sls_cons | model_default | pct_st                  |   -0.331 |
| model_sdm2sls_cons | model_default | pct_obc                 |    1.703 |
| model_sdm2sls_cons | model_default | pct_fem_head            |    2.887 |
| model_sdm2sls_cons | model_default | pct_medium_land         |    1.822 |
| model_sdm2sls_cons | model_default | pct_large_land          |   -5.396 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |    0.200 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -216.733 |
| model_sdm2sls_gini | model_default | (Intercept)             |    0.146 |
| model_sdm2sls_gini | model_default | W_giniY                 |   -1.412 |
| model_sdm2sls_gini | model_default | EMIE                    |   -0.004 |
| model_sdm2sls_gini | model_default | W_EMIE                  |    0.003 |
| model_sdm2sls_gini | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |    0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |   -0.564 |
| model_sdm2sls_gini | model_default | pct_urban               |    0.002 |
| model_sdm2sls_gini | model_default | pct_head_secondary_plus |    0.001 |
| model_sdm2sls_gini | model_default | pct_muslim              |    0.000 |
| model_sdm2sls_gini | model_default | pct_st                  |    0.001 |
| model_sdm2sls_gini | model_default | pct_obc                 |    0.000 |
| model_sdm2sls_gini | model_default | pct_fem_head            |    0.001 |
| model_sdm2sls_gini | model_default | pct_medium_land         |    0.000 |
| model_sdm2sls_gini | model_default | pct_large_land          |   -0.002 |
| model_sdm2sls_gini | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_consumption_0708      |    0.000 |
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |   -0.452 |

Default coefficient summaries

``` r
analysis_table(spatial_iv_cluster, "Clustered-SE coeftest attempt")
```

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -634.222 | 3951.867 | -0.160 | 0.873 |
| model_sdm2sls_cons | estimated | region | W_consY | 3.727 | 16.768 | 0.222 | 0.824 |
| model_sdm2sls_cons | estimated | region | EMIE | 15.079 | 50.018 | 0.301 | 0.763 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -14.633 | 43.524 | -0.336 | 0.737 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.272 | 0.786 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.001 | -0.264 | 0.792 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.093 | 0.315 | -0.296 | 0.768 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | -173.638 | 305.983 | -0.567 | 0.571 |
| model_sdm2sls_cons | estimated | region | pct_urban | -3.247 | 17.190 | -0.189 | 0.850 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | -1.596 | 4.925 | -0.324 | 0.746 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.211 | 0.473 | 0.446 | 0.655 |
| model_sdm2sls_cons | estimated | region | pct_st | -0.331 | 1.495 | -0.221 | 0.825 |
| model_sdm2sls_cons | estimated | region | pct_obc | 1.703 | 9.787 | 0.174 | 0.862 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 2.887 | 13.174 | 0.219 | 0.827 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 1.822 | 8.894 | 0.205 | 0.838 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -5.396 | 27.855 | -0.194 | 0.846 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.448 | 0.654 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 0.721 | 0.471 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.200 | 0.146 | 1.370 | 0.171 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -216.733 | 998.062 | -0.217 | 0.828 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.146 | 0.232 | 0.631 | 0.528 |
| model_sdm2sls_gini | estimated | region | W_giniY | -1.412 | 5.153 | -0.274 | 0.784 |
| model_sdm2sls_gini | estimated | region | EMIE | -0.004 | 0.009 | -0.467 | 0.641 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.003 | 0.007 | 0.442 | 0.658 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | -0.149 | 0.881 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | 0.100 | 0.921 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | -0.205 | 0.837 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.564 | 0.800 | -0.706 | 0.481 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.002 | 0.004 | 0.584 | 0.560 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.001 | 0.001 | 0.745 | 0.457 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.001 | -0.170 | 0.865 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.001 | 0.003 | 0.511 | 0.609 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.001 | -0.585 | 0.559 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.001 | 0.003 | 0.346 | 0.730 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -2.307 | 0.021 |
| model_sdm2sls_gini | estimated | region | pct_large_land | -0.002 | 0.003 | -0.552 | 0.582 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | 0.228 | 0.820 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.327 | 0.744 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | 0.002 | 0.998 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | -0.452 | 2.582 | -0.175 | 0.861 |

Clustered-SE coeftest attempt

``` r
analysis_table(spatial_iv_fail, "Failure/status summary")
```

| status    |   n |
|:----------|----:|
| estimated |   2 |

Failure/status summary

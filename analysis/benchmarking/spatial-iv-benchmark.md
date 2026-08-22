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
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 2.39256e-17 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 3.32268e-18 | estimated |

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
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 2.39256e-17 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 3.32268e-18 |

IV diagnostic summaries

``` r
analysis_table(spatial_iv_coef, "Default coefficient summaries")
```

| model              | vcov_type     | term                    | estimate |
|:-------------------|:--------------|:------------------------|---------:|
| model_sdm2sls_cons | model_default | (Intercept)             | -140.717 |
| model_sdm2sls_cons | model_default | W_consY                 |    2.492 |
| model_sdm2sls_cons | model_default | EMIE                    |    3.031 |
| model_sdm2sls_cons | model_default | W_EMIE                  |   -4.314 |
| model_sdm2sls_cons | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |   -0.144 |
| model_sdm2sls_cons | model_default | gini_cons_0708          |    8.431 |
| model_sdm2sls_cons | model_default | pct_urban               |   -0.166 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |    0.524 |
| model_sdm2sls_cons | model_default | pct_muslim              |    0.361 |
| model_sdm2sls_cons | model_default | pct_st                  |    0.447 |
| model_sdm2sls_cons | model_default | pct_obc                 |    0.066 |
| model_sdm2sls_cons | model_default | pct_fem_head            |    0.376 |
| model_sdm2sls_cons | model_default | pct_medium_land         |    0.715 |
| model_sdm2sls_cons | model_default | pct_large_land          |   -1.925 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |    0.192 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -773.427 |
| model_sdm2sls_gini | model_default | (Intercept)             |    0.085 |
| model_sdm2sls_gini | model_default | W_giniY                 |    0.773 |
| model_sdm2sls_gini | model_default | EMIE                    |    0.000 |
| model_sdm2sls_gini | model_default | W_EMIE                  |    0.000 |
| model_sdm2sls_gini | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |    0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |   -0.778 |
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
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |    0.639 |

Default coefficient summaries

``` r
analysis_table(spatial_iv_cluster, "Clustered-SE coeftest attempt")
```

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -140.717 | 210.678 | -0.668 | 0.504 |
| model_sdm2sls_cons | estimated | region | W_consY | 2.492 | 1.229 | 2.028 | 0.043 |
| model_sdm2sls_cons | estimated | region | EMIE | 3.031 | 4.050 | 0.748 | 0.455 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -4.314 | 5.048 | -0.855 | 0.393 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.000 | 1.160 | 0.247 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.000 | -1.076 | 0.282 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.144 | 0.032 | -4.518 | 0.000 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | 8.431 | 102.689 | 0.082 | 0.935 |
| model_sdm2sls_cons | estimated | region | pct_urban | -0.166 | 1.574 | -0.106 | 0.916 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | 0.524 | 0.901 | 0.582 | 0.561 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.361 | 0.646 | 0.559 | 0.576 |
| model_sdm2sls_cons | estimated | region | pct_st | 0.447 | 0.992 | 0.451 | 0.652 |
| model_sdm2sls_cons | estimated | region | pct_obc | 0.066 | 0.312 | 0.212 | 0.832 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 0.376 | 0.984 | 0.382 | 0.703 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 0.715 | 0.427 | 1.675 | 0.094 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -1.925 | 2.174 | -0.885 | 0.376 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -1.238 | 0.216 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 1.078 | 0.281 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.192 | 0.127 | 1.504 | 0.133 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -773.427 | 473.714 | -1.633 | 0.103 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.085 | 0.049 | 1.733 | 0.084 |
| model_sdm2sls_gini | estimated | region | W_giniY | 0.773 | 0.408 | 1.894 | 0.059 |
| model_sdm2sls_gini | estimated | region | EMIE | 0.000 | 0.001 | -0.060 | 0.952 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.000 | 0.001 | 0.063 | 0.949 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.674 | 0.501 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | -0.255 | 0.798 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | 0.441 | 0.660 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.778 | 0.047 | -16.535 | 0.000 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.001 | 0.001 | 0.879 | 0.380 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.000 | 0.000 | 1.107 | 0.269 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.000 | -0.215 | 0.829 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.000 | 0.000 | 0.279 | 0.780 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.000 | -2.544 | 0.011 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.000 | 0.001 | -0.400 | 0.689 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -0.851 | 0.395 |
| model_sdm2sls_gini | estimated | region | pct_large_land | 0.000 | 0.001 | 0.246 | 0.806 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.111 | 0.912 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.298 | 0.766 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | -1.335 | 0.182 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | 0.639 | 0.190 | 3.370 | 0.001 |

Clustered-SE coeftest attempt

``` r
analysis_table(spatial_iv_fail, "Failure/status summary")
```

| status    |   n |
|:----------|----:|
| estimated |   2 |

Failure/status summary

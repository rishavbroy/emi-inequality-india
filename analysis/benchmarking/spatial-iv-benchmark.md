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
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 7.76419e-18 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | 573 | failed: system is computationally singular: reciprocal condition number = 2.54894e-18 | estimated |

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
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 7.76419e-18 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 2.54894e-18 |

IV diagnostic summaries

``` r
analysis_table(spatial_iv_coef, "Default coefficient summaries")
```

| model              | vcov_type     | term                    |  estimate |
|:-------------------|:--------------|:------------------------|----------:|
| model_sdm2sls_cons | model_default | (Intercept)             |  -483.335 |
| model_sdm2sls_cons | model_default | W_consY                 |     4.586 |
| model_sdm2sls_cons | model_default | EMIE                    |     1.911 |
| model_sdm2sls_cons | model_default | W_EMIE                  |    -3.699 |
| model_sdm2sls_cons | model_default | npeople_0708            |     0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |     0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |    -0.106 |
| model_sdm2sls_cons | model_default | gini_cons_0708          |    11.944 |
| model_sdm2sls_cons | model_default | pct_urban               |    -0.282 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |     1.442 |
| model_sdm2sls_cons | model_default | pct_muslim              |     0.713 |
| model_sdm2sls_cons | model_default | pct_st                  |     1.531 |
| model_sdm2sls_cons | model_default | pct_obc                 |     0.475 |
| model_sdm2sls_cons | model_default | pct_fem_head            |     0.984 |
| model_sdm2sls_cons | model_default | pct_medium_land         |     1.250 |
| model_sdm2sls_cons | model_default | pct_large_land          |    -4.230 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |     0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |     0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |     0.173 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -1358.480 |
| model_sdm2sls_gini | model_default | (Intercept)             |     0.076 |
| model_sdm2sls_gini | model_default | W_giniY                 |     0.670 |
| model_sdm2sls_gini | model_default | EMIE                    |     0.000 |
| model_sdm2sls_gini | model_default | W_EMIE                  |     0.000 |
| model_sdm2sls_gini | model_default | npeople_0708            |     0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |     0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |     0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |    -0.767 |
| model_sdm2sls_gini | model_default | pct_urban               |     0.000 |
| model_sdm2sls_gini | model_default | pct_head_secondary_plus |     0.000 |
| model_sdm2sls_gini | model_default | pct_muslim              |     0.000 |
| model_sdm2sls_gini | model_default | pct_st                  |     0.000 |
| model_sdm2sls_gini | model_default | pct_obc                 |     0.000 |
| model_sdm2sls_gini | model_default | pct_fem_head            |     0.000 |
| model_sdm2sls_gini | model_default | pct_medium_land         |     0.000 |
| model_sdm2sls_gini | model_default | pct_large_land          |     0.000 |
| model_sdm2sls_gini | model_default | W_npeople_0708          |     0.000 |
| model_sdm2sls_gini | model_default | W_nhouses_0708          |     0.000 |
| model_sdm2sls_gini | model_default | W_consumption_0708      |     0.000 |
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |     0.596 |

Default coefficient summaries

``` r
analysis_table(spatial_iv_cluster, "Clustered-SE coeftest attempt")
```

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -483.335 | 792.605 | -0.610 | 0.542 |
| model_sdm2sls_cons | estimated | region | W_consY | 4.586 | 4.412 | 1.039 | 0.299 |
| model_sdm2sls_cons | estimated | region | EMIE | 1.911 | 4.982 | 0.384 | 0.701 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -3.699 | 6.840 | -0.541 | 0.589 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.814 | 0.416 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.000 | -0.764 | 0.445 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.106 | 0.094 | -1.126 | 0.261 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | 11.944 | 158.901 | 0.075 | 0.940 |
| model_sdm2sls_cons | estimated | region | pct_urban | -0.282 | 2.378 | -0.119 | 0.906 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | 1.442 | 2.462 | 0.586 | 0.558 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.713 | 1.593 | 0.448 | 0.654 |
| model_sdm2sls_cons | estimated | region | pct_st | 1.531 | 2.964 | 0.516 | 0.606 |
| model_sdm2sls_cons | estimated | region | pct_obc | 0.475 | 1.039 | 0.457 | 0.648 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 0.984 | 2.492 | 0.395 | 0.693 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 1.250 | 1.082 | 1.155 | 0.249 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -4.230 | 6.774 | -0.624 | 0.533 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.762 | 0.447 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 0.660 | 0.510 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.173 | 0.206 | 0.838 | 0.402 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -1358.480 | 1473.704 | -0.922 | 0.357 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.076 | 0.053 | 1.440 | 0.151 |
| model_sdm2sls_gini | estimated | region | W_giniY | 0.670 | 0.635 | 1.056 | 0.292 |
| model_sdm2sls_gini | estimated | region | EMIE | 0.000 | 0.002 | 0.227 | 0.821 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.000 | 0.002 | -0.226 | 0.821 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.651 | 0.515 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | -0.348 | 0.728 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | 0.166 | 0.868 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.767 | 0.057 | -13.367 | 0.000 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.000 | 0.001 | 0.645 | 0.519 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.000 | 0.000 | 0.672 | 0.502 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.000 | -0.246 | 0.805 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.000 | 0.000 | 0.074 | 0.941 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.000 | -2.851 | 0.005 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.000 | 0.001 | -0.142 | 0.887 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -0.561 | 0.575 |
| model_sdm2sls_gini | estimated | region | pct_large_land | 0.000 | 0.001 | 0.104 | 0.917 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.259 | 0.796 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.135 | 0.893 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | -1.023 | 0.307 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | 0.596 | 0.265 | 2.254 | 0.025 |

Clustered-SE coeftest attempt

``` r
analysis_table(spatial_iv_fail, "Failure/status summary")
```

| status    |   n |
|:----------|----:|
| estimated |   2 |

Failure/status summary

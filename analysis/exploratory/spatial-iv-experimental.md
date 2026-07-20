# Spatial IV Experimental Models


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy diagnostic intent

This exploratory landing page keeps the legacy `eval = FALSE` spatial-IV
model attempts in `analysis/`, while the benchmark notebook gives the
fuller diagnostic summary. The current result is target-backed and
should be refreshed with `make benchmarking`.

``` r
spatial_iv_status <- analysis_target_csv("bench_spatial_iv_experimental", "spatial_iv_model_status.csv")
analysis_table(spatial_iv_status, "Spatial-IV model status")
```

| model | status | methodological_success | reason | formula | nobs | diagnostics_status | cluster_se_status |
|:---|:---|:---|:---|:---|---:|:---|:---|
| model_sdm2sls_cons | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | consumption_pct_change ~ W_consY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 \| wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 | 482 | failed: system is computationally singular: reciprocal condition number = 1.6623e-17 | estimated |
| model_sdm2sls_gini | estimated | FALSE | Legacy comments said these attempts did not work; current status only means ivreg returned an object. A model is marked as methodologically successful only when diagnostics and clustered-SE extraction also succeed. | gini_change ~ W_giniY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 \| wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 | 482 | failed: system is computationally singular: reciprocal condition number = 2.57101e-19 | estimated |

Spatial-IV model status

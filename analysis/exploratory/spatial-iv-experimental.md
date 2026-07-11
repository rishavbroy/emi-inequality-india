# Spatial IV Experimental Models


## Legacy comments

### Legacy Chunk 30: exploratory spatial-IV comments

Build spatial lags of dependent variables

(Re)build the spatial lags of endogenous EMIE + IV

2nd order lag

(Re)build spatial lags of exogenous controls

Spatial-2SLS for consumption change, also instrumenting W_consY

``` r
summary(model_sdm2sls_cons, diagnostics = TRUE)
summary(model_sdm2sls_gini,  diagnostics = TRUE)
Don't work even when diagnostics = FALSE
```

Region clustered standard errors

HC0 by default

``` r
coeftest(model_sdm2sls_cons, vcov. = vcov_cluster_cons)
coeftest(model_sdm2sls_gini, vcov. = vcov_cluster_gini)
```

**Deviation note.** This note intentionally mirrors the benchmark
notebook rather than adding a second interpretation. It exists as the
exploratory analysis landing page for the legacy eval=FALSE spatial-IV
work.

## Current targets-backed result

| model | status | reason | formula | nobs | diagnostics_status | cluster_se_status |
|:---|:---|:---|:---|---:|:---|:---|
| model_sdm2sls_cons | estimated | Legacy comments said these attempts did not work; current status only means ivreg returned an object, not that the model is suitable for final use. | consumption_pct_change ~ W_consY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 \| wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 | 482 | failed: system is computationally singular: reciprocal condition number = 6.38409e-18 | estimated |
| model_sdm2sls_gini | estimated | Legacy comments said these attempts did not work; current status only means ivreg returned an object, not that the model is suitable for final use. | gini_change ~ W_giniY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 \| wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus + pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 | 482 | failed: system is computationally singular: reciprocal condition number = 3.61172e-19 | estimated |

Spatial-IV model status

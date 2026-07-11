# Spatial IV Benchmark


## Legacy comments

### Legacy Chunk 30: spatial-2SLS attempts

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

**Deviation note.** The legacy comments said these attempts did not work
even with diagnostics disabled. The current tables therefore distinguish
between `ivreg()` returning an object and diagnostics/coefficient
summaries being numerically meaningful. These outputs remain exploratory
and are not promoted to the final paper.

## Current targets-backed results

| model | status | reason | nobs | diagnostics_status | cluster_se_status |
|:---|:---|:---|---:|:---|:---|
| model_sdm2sls_cons | estimated | Legacy comments said these attempts did not work; current status only means ivreg returned an object, not that the model is suitable for final use. | 482 | failed: system is computationally singular: reciprocal condition number = 6.38409e-18 | estimated |
| model_sdm2sls_gini | estimated | Legacy comments said these attempts did not work; current status only means ivreg returned an object, not that the model is suitable for final use. | 482 | failed: system is computationally singular: reciprocal condition number = 3.61172e-19 | estimated |

Spatial-IV model status

### Formula: `model_sdm2sls_cons`

``` text
consumption_pct_change ~ W_consY + EMIE + W_EMIE + npeople_0708 +      nhouses_0708 + consumption_0708 + gini_cons_0708 + pct_urban +      pct_head_secondary_plus + pct_muslim + pct_st + pct_obc +      pct_fem_head + pct_medium_land + pct_large_land + W_npeople_0708 +      W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708 |      wavg_ling_degrees + W_wLing + W2_wLing + npeople_0708 + nhouses_0708 +          consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus +          pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land +          pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 +          W_gini_cons_0708
```

### Formula: `model_sdm2sls_gini`

``` text
gini_change ~ W_giniY + EMIE + W_EMIE + npeople_0708 + nhouses_0708 +      consumption_0708 + gini_cons_0708 + pct_urban + pct_head_secondary_plus +      pct_muslim + pct_st + pct_obc + pct_fem_head + pct_medium_land +      pct_large_land + W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 +      W_gini_cons_0708 | wavg_ling_degrees + W_wLing + W2_wLing +      npeople_0708 + nhouses_0708 + consumption_0708 + gini_cons_0708 +      pct_urban + pct_head_secondary_plus + pct_muslim + pct_st +      pct_obc + pct_fem_head + pct_medium_land + pct_large_land +      W_npeople_0708 + W_nhouses_0708 + W_consumption_0708 + W_gini_cons_0708
```

| model | status | reason |
|:---|:---|:---|
| model_sdm2sls_cons | failed | system is computationally singular: reciprocal condition number = 6.38409e-18 |
| model_sdm2sls_gini | failed | system is computationally singular: reciprocal condition number = 3.61172e-19 |

IV diagnostic summaries

| model              | vcov_type     | term                    | estimate |
|:-------------------|:--------------|:------------------------|---------:|
| model_sdm2sls_cons | model_default | (Intercept)             | -627.654 |
| model_sdm2sls_cons | model_default | W_consY                 |    4.316 |
| model_sdm2sls_cons | model_default | EMIE                    |   16.197 |
| model_sdm2sls_cons | model_default | W_EMIE                  |  -16.524 |
| model_sdm2sls_cons | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_cons | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_cons | model_default | consumption_0708        |   -0.112 |
| model_sdm2sls_cons | model_default | gini_cons_0708          | -253.372 |
| model_sdm2sls_cons | model_default | pct_urban               |   -3.349 |
| model_sdm2sls_cons | model_default | pct_head_secondary_plus |   -1.319 |
| model_sdm2sls_cons | model_default | pct_muslim              |    0.490 |
| model_sdm2sls_cons | model_default | pct_st                  |    0.065 |
| model_sdm2sls_cons | model_default | pct_obc                 |    1.931 |
| model_sdm2sls_cons | model_default | pct_fem_head            |    7.952 |
| model_sdm2sls_cons | model_default | pct_medium_land         |    2.373 |
| model_sdm2sls_cons | model_default | pct_large_land          |   -4.581 |
| model_sdm2sls_cons | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_cons | model_default | W_consumption_0708      |    0.256 |
| model_sdm2sls_cons | model_default | W_gini_cons_0708        | -396.297 |
| model_sdm2sls_gini | model_default | (Intercept)             |    0.110 |
| model_sdm2sls_gini | model_default | W_giniY                 |   -0.908 |
| model_sdm2sls_gini | model_default | EMIE                    |   -0.003 |
| model_sdm2sls_gini | model_default | W_EMIE                  |    0.002 |
| model_sdm2sls_gini | model_default | npeople_0708            |    0.000 |
| model_sdm2sls_gini | model_default | nhouses_0708            |    0.000 |
| model_sdm2sls_gini | model_default | consumption_0708        |    0.000 |
| model_sdm2sls_gini | model_default | gini_cons_0708          |   -0.613 |
| model_sdm2sls_gini | model_default | pct_urban               |    0.002 |
| model_sdm2sls_gini | model_default | pct_head_secondary_plus |    0.001 |
| model_sdm2sls_gini | model_default | pct_muslim              |    0.000 |
| model_sdm2sls_gini | model_default | pct_st                  |    0.001 |
| model_sdm2sls_gini | model_default | pct_obc                 |    0.000 |
| model_sdm2sls_gini | model_default | pct_fem_head            |    0.004 |
| model_sdm2sls_gini | model_default | pct_medium_land         |    0.000 |
| model_sdm2sls_gini | model_default | pct_large_land          |   -0.001 |
| model_sdm2sls_gini | model_default | W_npeople_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_nhouses_0708          |    0.000 |
| model_sdm2sls_gini | model_default | W_consumption_0708      |    0.000 |
| model_sdm2sls_gini | model_default | W_gini_cons_0708        |   -0.218 |

Default coefficient summaries

| model | status | cluster_column | term | estimate | std.\_error | t_value | pr(\>\|t\|) |
|:---|:---|:---|:---|---:|---:|---:|---:|
| model_sdm2sls_cons | estimated | region | (Intercept) | -627.654 | 4553.005 | -0.138 | 0.890 |
| model_sdm2sls_cons | estimated | region | W_consY | 4.316 | 22.105 | 0.195 | 0.845 |
| model_sdm2sls_cons | estimated | region | EMIE | 16.197 | 63.572 | 0.255 | 0.799 |
| model_sdm2sls_cons | estimated | region | W_EMIE | -16.524 | 59.049 | -0.280 | 0.780 |
| model_sdm2sls_cons | estimated | region | npeople_0708 | 0.000 | 0.001 | 0.217 | 0.828 |
| model_sdm2sls_cons | estimated | region | nhouses_0708 | 0.000 | 0.002 | -0.210 | 0.833 |
| model_sdm2sls_cons | estimated | region | consumption_0708 | -0.112 | 0.245 | -0.457 | 0.648 |
| model_sdm2sls_cons | estimated | region | gini_cons_0708 | -253.372 | 785.322 | -0.323 | 0.747 |
| model_sdm2sls_cons | estimated | region | pct_urban | -3.349 | 20.313 | -0.165 | 0.869 |
| model_sdm2sls_cons | estimated | region | pct_head_secondary_plus | -1.319 | 4.677 | -0.282 | 0.778 |
| model_sdm2sls_cons | estimated | region | pct_muslim | 0.490 | 1.617 | 0.303 | 0.762 |
| model_sdm2sls_cons | estimated | region | pct_st | 0.065 | 3.058 | 0.021 | 0.983 |
| model_sdm2sls_cons | estimated | region | pct_obc | 1.931 | 12.371 | 0.156 | 0.876 |
| model_sdm2sls_cons | estimated | region | pct_fem_head | 7.952 | 46.303 | 0.172 | 0.864 |
| model_sdm2sls_cons | estimated | region | pct_medium_land | 2.373 | 13.171 | 0.180 | 0.857 |
| model_sdm2sls_cons | estimated | region | pct_large_land | -4.581 | 26.602 | -0.172 | 0.863 |
| model_sdm2sls_cons | estimated | region | W_npeople_0708 | 0.000 | 0.000 | -0.443 | 0.658 |
| model_sdm2sls_cons | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | 0.782 | 0.435 |
| model_sdm2sls_cons | estimated | region | W_consumption_0708 | 0.256 | 0.351 | 0.730 | 0.466 |
| model_sdm2sls_cons | estimated | region | W_gini_cons_0708 | -396.297 | 1946.040 | -0.204 | 0.839 |
| model_sdm2sls_gini | estimated | region | (Intercept) | 0.110 | 0.154 | 0.716 | 0.474 |
| model_sdm2sls_gini | estimated | region | W_giniY | -0.908 | 3.326 | -0.273 | 0.785 |
| model_sdm2sls_gini | estimated | region | EMIE | -0.003 | 0.006 | -0.525 | 0.600 |
| model_sdm2sls_gini | estimated | region | W_EMIE | 0.002 | 0.005 | 0.491 | 0.624 |
| model_sdm2sls_gini | estimated | region | npeople_0708 | 0.000 | 0.000 | 0.512 | 0.609 |
| model_sdm2sls_gini | estimated | region | nhouses_0708 | 0.000 | 0.000 | -0.879 | 0.380 |
| model_sdm2sls_gini | estimated | region | consumption_0708 | 0.000 | 0.000 | -0.172 | 0.863 |
| model_sdm2sls_gini | estimated | region | gini_cons_0708 | -0.613 | 0.552 | -1.111 | 0.267 |
| model_sdm2sls_gini | estimated | region | pct_urban | 0.002 | 0.002 | 0.715 | 0.475 |
| model_sdm2sls_gini | estimated | region | pct_head_secondary_plus | 0.001 | 0.001 | 0.902 | 0.367 |
| model_sdm2sls_gini | estimated | region | pct_muslim | 0.000 | 0.001 | 0.047 | 0.963 |
| model_sdm2sls_gini | estimated | region | pct_st | 0.001 | 0.002 | 0.614 | 0.540 |
| model_sdm2sls_gini | estimated | region | pct_obc | 0.000 | 0.001 | -0.717 | 0.474 |
| model_sdm2sls_gini | estimated | region | pct_fem_head | 0.004 | 0.006 | 0.666 | 0.506 |
| model_sdm2sls_gini | estimated | region | pct_medium_land | 0.000 | 0.000 | -0.643 | 0.521 |
| model_sdm2sls_gini | estimated | region | pct_large_land | -0.001 | 0.001 | -0.656 | 0.512 |
| model_sdm2sls_gini | estimated | region | W_npeople_0708 | 0.000 | 0.000 | 0.274 | 0.784 |
| model_sdm2sls_gini | estimated | region | W_nhouses_0708 | 0.000 | 0.000 | -0.414 | 0.679 |
| model_sdm2sls_gini | estimated | region | W_consumption_0708 | 0.000 | 0.000 | -0.052 | 0.958 |
| model_sdm2sls_gini | estimated | region | W_gini_cons_0708 | -0.218 | 1.728 | -0.126 | 0.900 |

Clustered-SE coeftest attempt

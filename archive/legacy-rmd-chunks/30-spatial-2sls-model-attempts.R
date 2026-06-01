# Build spatial lags of dependent variables
joined_df$W_consY <- lag.listw(listw_2020, joined_df$consumption_pct_change, zero.policy=TRUE)
joined_df$W_giniY <- lag.listw(listw_2020, joined_df$gini_change, zero.policy=TRUE)

# (Re)build the spatial lags of  endogenous EMIE + IV
joined_df$W_EMIE <- lag.listw(listw_2020, joined_df$EMIE, zero.policy=TRUE)
joined_df$W_wLing <- lag.listw(listw_2020, joined_df$wavg_ling_degrees, zero.policy=TRUE)

# 2nd order lag
joined_df$W2_wLing  <- lag.listw(listw_2020, joined_df$W_wLing, zero.policy=TRUE)

# (Re)build spatial lags of exogenous controls
controls <- c("npeople_0708", "nhouses_0708", "consumption_0708", "gini_cons_0708")
for(v in controls) {
  joined_df[[paste0("W_", v)]] <- lag.listw(listw_2020, joined_df[[v]], zero.policy=TRUE)
}

# Spatial-2SLS for consumption change, also instrumenting W_consY
model_sdm2sls_cons <- ivreg(
  make_iv_formula(
    dep = "consumption_pct_change",
    endog = c("W_consY", "EMIE", "W_EMIE"),
    exog = c(controls_needlag, controls_nolag, controls_lagged),
    inst = c(controls_needlag, controls_nolag, controls_lagged,
              "wavg_ling_degrees", "W_wLing", "W2_wLing")
  ),
  data = joined_df
)

model_sdm2sls_gini <- ivreg(
  make_iv_formula(
    dep = "gini_change",
    endog = c("W_giniY", "EMIE", "W_EMIE"),
    exog = c(controls_needlag, controls_nolag, controls_lagged),
    inst = c("wavg_ling_degrees", "W_wLing", "W2_wLing", 
             controls_needlag, controls_nolag, controls_lagged)
  ),
  data = joined_df
)

# summary(model_sdm2sls_cons, diagnostics = TRUE)
# summary(model_sdm2sls_gini,  diagnostics = TRUE)
# Don't work even when diagnostics = FALSE



# Region clustered standard errors

vcov_cluster_cons <- vcovCL(
  model_sdm2sls_cons,
  cluster = ~ region,
  data = joined_df
)
#HC0 by default

vcov_cluster_gini <- vcovCL(
  model_sdm2sls_gini,
  cluster = ~ region,
  data = joined_df
)


# coeftest(model_sdm2sls_cons, vcov. = vcov_cluster_cons)
# coeftest(model_sdm2sls_gini, vcov. = vcov_cluster_gini)

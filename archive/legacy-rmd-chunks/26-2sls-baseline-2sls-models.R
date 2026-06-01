# Run 2SLS regressions using ivreg.
# Formula syntax: outcome ~ endogenous + controls | instrument + controls
model_consumption_iv <- ivreg(
  make_iv_formula(
    dep = "consumption_pct_change",
    endog = "EMIE",
    exog = c(controls_needlag, controls_nolag),
    inst = "wavg_ling_degrees"    # ← just the new instrument
  ),
  data = joined_df
)

model_gini_iv <- ivreg(
  make_iv_formula(
    dep = "gini_change",
    endog = "EMIE",
    exog = c(controls_needlag, controls_nolag),
    inst = "wavg_ling_degrees"
  ),
  data = joined_df
)


# Extract p-values from each IV model (to override standard errors)
# pvalues_consumption_iv <- coef(summary(model_consumption_iv))[, "Pr(>|t|)"]
# pvalues_gini_iv <- coef(summary(model_gini_iv))[, "Pr(>|t|)"]


# Run first-stage regressions for instrument relevance. Get F-stat.
# Predict EMIE using weighted_avg_ling_distance and the same controls.
first_stage_consumption <- lm(
  reformulate(
    c("wavg_ling_degrees", controls_needlag, controls_nolag), 
    response = "EMIE"
    ),
  data = joined_df
)


fs_consumption <- summary(first_stage_consumption)$fstatistic
# F-stat returned as vector: value, numerator df, denominator df.
p_fs_consumption <- pf(fs_consumption[1], fs_consumption[2], fs_consumption[3], lower.tail = FALSE)



first_stage_gini <- lm(
  reformulate(
    c(controls_needlag, controls_nolag), 
    response = "EMIE"
    ),
  data = joined_df
)
fs_gini <- summary(first_stage_gini)$fstatistic
p_fs_gini <- pf(fs_gini[1], fs_gini[2], fs_gini[3], lower.tail = FALSE)

# Optional rounding these numbers:
fs_consumption_value <- round(fs_consumption[1], 2)
fs_gini_value <- round(fs_gini[1], 2)
p_fs_consumption_fmt <- format.pval(p_fs_consumption, digits = 3)
p_fs_gini_fmt <- format.pval(p_fs_gini, digits = 3)




# Cluster SEs by state; IV and EMIE are relatively homogenous within a few states

vcov_first_stage_consumption <- vcovCL(first_stage_consumption, cluster = ~ state_20)

# coeftest(first_stage_consumption, vcov = vcov_first_stage_consumption)

# coeftest(first_stage_consumption)

vcov_model_consumption_iv <- vcovCL(model_consumption_iv, cluster = ~ state_20)

# coeftest(model_consumption_iv,vcov = vcov_model_consumption_iv)


vcov_model_gini_iv <- vcovCL(model_gini_iv, cluster = ~ state_20)

# coeftest(model_gini_iv, vcov = vcov_model_gini_iv)


# summary(model_consumption_iv, diagnostics = FALSE)
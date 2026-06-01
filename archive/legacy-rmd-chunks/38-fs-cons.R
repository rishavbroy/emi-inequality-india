# Mapping for cleaner covariate labels
var_labels <- c(
  
  "EMIE" = "EMI exposure (fitted)",
  "wavg_ling_degrees" = "Linguistic distance",
  "consumption_0708" = "Consumption (2007-08)",
  "gini_cons_0708" = "Gini of Consumption (2007-08)",
  "avg_IMR" = "Average IMR",
  "pct_urban" = "Pct. Urban (ref: Rural)",
  "avg_hh_size" = "Average HH size",
  "dependency_ratio" = "Dependency ratio × 100",
  "pct_fem_head" = "Pct. Female head",
  "pct_hindu" = "Pct. Hindu  (ref: Other)",
  "pct_muslim" = "Pct. Muslim",
  "pct_st" = "Pct. Scheduled Tribe (ref: Other)",
  "pct_sc" = "Pct. Scheduled Caste",
  "pct_obc" = "Pct. OBC",
  "pct_small_land" = "Pct. Small Land-owner (ref: No Land)",
  "pct_medium_land" = "Pct. Medium Land-owner",
  "pct_large_land" = "Pct. Large Land-owner",
  "pct_head_lit_to_primary" = "Pct. Head Educ., Lit.-Primary (ref: Illiterate)",
  "pct_head_secondary_plus" = "Pct. Head Educ., Secondary+",
  "(Intercept)" = "Intercept"
)


#### Test instrument's strength ####

# Run cluster-robust Wald F-test
V_cl <- vcovCL(first_stage_consumption, cluster = first_stage_consumption$state_20)

partial_F_test <- linearHypothesis(
  first_stage_consumption,
  "wavg_ling_degrees = 0",
  vcov. = V_cl,
  test = "F"
)

# Extract F-stat and p-val
partial_f <- partial_F_test[["F"]][2]
partial_p <- partial_F_test[["Pr(>F)"]][2]

# Create asterisks based on p-val
stars <- function(p) {
  if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Add custom row for Weak IV F-stat
custom_gof_row <- data.frame(
  term = "Instrument's F-Statistic",
  estimate = paste0(formatC(partial_f, digits = 2, format = "f"), stars(partial_p)),
  stringsAsFactors = FALSE
)


# Table with styling and stats
modelsummary(
  first_stage_consumption,
  coef_map = var_labels,
  vcov = vcov_first_stage_consumption,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "$R^2$", fmt = 3),
    list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
    list(raw = "sigma", clean = "Residual Std. Error",  fmt = 3),
    list(raw = "statistic", clean = "Model's F-Statistic", fmt = 2)
  ),
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  title = "First-Stage Regression: EMI Exposure on Linguistic Distance",
  output = "kableExtra",
  escape = FALSE,
  notes = list(
    "Standard errors clustered by state in parentheses."
  ),
  add_rows = custom_gof_row
) %>%
  kable_styling(latex_options = c(
    "hold_position", 
    "repeat_header", 
    "striped",
    "longtable"), 
    position = "center", 
    full_width = FALSE
    ) %>%
  add_header_above(c(" " = 1, "EMI Exposure" = 1))

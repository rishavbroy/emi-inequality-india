modelsummary(
  model_consumption_iv,
  coef_map = var_labels,
  vcov = vcov_model_consumption_iv,
  gof_map = list(
    list(raw = "nobs", clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "$R^2$", fmt = 3),
    list(raw = "adj.r.squared", clean = "Adjusted $R^2$", fmt = 3),
    list(raw = "sigma", clean = "Residual Std. Error", fmt = 3),
    list(raw = "waldtest", clean = "F-Statistic", fmt = 2)
  ),
  stars = c('*' = .05, '**' = .01, '***' = .001),
  fmt = 3,
  title = "Second-Stage Regression: Consumption Growth on EMIE (Fitted)",
  output = "kableExtra",
  escape = FALSE,
  notes = list(
    "Standard errors clustered by state in parentheses."
  ),
  add_rows = NULL
) %>%
  kable_styling(
    latex_options = c("hold_position", "repeat_header", "striped", "longtable"),
    position = "center",
    full_width = FALSE
  ) %>%
  add_header_above(c(" " = 1, "Consumption Growth" = 1))

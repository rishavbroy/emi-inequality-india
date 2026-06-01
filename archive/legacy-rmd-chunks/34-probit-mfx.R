# Print with kableExtra. Centered w/ booktabs style
kbl(
  reg_tbl_selection,
  format = "latex",
  booktabs = TRUE,
  longtable = TRUE,
  linesep = "",
  label = "probit-mfx",
  caption = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
  align = c("l", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position", "repeat_header", "striped"), # hold_position = Keep near chunk's position
    position = "center"
  ) %>%
  add_header_above(c(" " = 1, "Enrolled in School (1 = yes)" = 2)) %>% # Dep. var. label above numeric vars
  footnote(
    general = "Data from the 64th round of the NSS, ``Participation and Expenditure in Education'' in 2007–08. All standard errors are design-based (clustered and nested within strata).",
    threeparttable = TRUE,
    escape = FALSE
  )

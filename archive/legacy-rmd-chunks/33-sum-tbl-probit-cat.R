kbl(
  sel_cat_stats %>%
      select(Variable=label, Values, Mode, `% Mode`, `Least Freq.`, `% Least Freq.`, N),
  format = "latex",
  col.names = c("Variable", "Values", "Mode", "Pct. Mode", "Least Freq.", "Pct. Least Freq.", "N"),
  booktabs=TRUE, 
  longtable=TRUE,
  label = "sum-tbl-probit-cat", 
  caption="Summary Statistics for Enrollment Participation Model (Categorical Variables)",
  escape=FALSE
  ) %>%
  kable_styling(
    latex_options=c(
      "repeat_header",
      "scale_down",
      "striped"), 
    full_width=FALSE
  ) %>%
  footnote(
    general = paste(
      "`Values' = all possible values;",
      "`Mode' = most frequent value;",
      "`Pct. Mode' = Pct. of observations which take on the modal value;",
      "`Least Freq.' = least frequent value;",
      "`Pct. Least Freq.' = Pct. of observations which take on the least freq. value;",
      "`N' = number of observations.",
      collapse = " "
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>% 
  landscape() %>%
  # Set Variables column narrow, Values a bit wider, Mode smaller, etc.
  column_spec(1, width = "4cm") %>% # Variable
  column_spec(2, width = "6cm") %>% # Values
  column_spec(4, width = "2cm") %>% # Pct. Mode
  column_spec(6, width = "2cm") # Pct. Least Freq.
kbl(
  sel_quant_stats %>% 
      select(Variable=label, Min, `1Q`, Med, `3Q`, Max, Mean, SD, N),
  format = "latex",
  booktabs=TRUE, 
  longtable=TRUE, 
  label = "sum-tbl-probit-quant", 
  caption="Summary Statistics for Enrollment Participation Model (Numeric Variables)",
  escape=FALSE
  ) %>%
  kable_styling(
    latex_options=c(
      "repeat_header",
      "scale_down",
      "striped"), 
    full_width=FALSE
  ) %>%
  ## All rows are one group:
  pack_rows("District‐level aggregates:", 
            which(sel_quant_stats$var=="dmean_num_IS_EDU_FREE"), 
            nrow(sel_quant_stats)) %>%
  footnote(
    general = paste(
      "`Min.' = minimum;",
      "`1Q' = first quartile;",
      "`Med.' = median;",
      "`3Q' = third quartile;",
      "`Max.' = maximum;",
      "`Mean' = arithmetic mean;",
      "`SD' = standard deviation;",
      "`N' = number of observations",
      collapse = " "
    ),
    escape = FALSE,
    threeparttable = TRUE,
    footnote_as_chunk = TRUE
  ) %>% 
  landscape()
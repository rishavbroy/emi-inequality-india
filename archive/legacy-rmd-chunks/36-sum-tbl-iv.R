kbl(
  joined_stats %>% 
      select(Variable = label, Description = desc, Min, `1Q`, Med, `3Q`, Max, Mean, SD, N),
  booktabs=TRUE, 
  longtable=TRUE, # longtable=T so table flows across pages
  label = "sum-tbl-iv", 
  caption="Summary Statistics for 2SLS Model",
, 
  escape=FALSE, # escape=F so LaTeX labels can be used
  format = "latex"
  ) %>%
  kable_styling(
    latex_options=c(
      "repeat_header",
      "scale_down",
      "striped")
    , full_width=FALSE
    ) %>% # Header repeats on each page, table scaled down so its full width fits on each page
  ## pack_rows: compute the row indices from joined_stats
  pack_rows("From 2001:",       1,   1) %>%
  pack_rows("From 2007-08:",    2,  22) %>%
  pack_rows("From 2017-18:",   23,  26) %>%
  pack_rows("From 2007-08 to 2017-18:",26,  27) %>%
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
  landscape() %>%
  # Narrow columns
  column_spec(1, width = "4cm") %>%  # Variable
  column_spec(2, width = "6cm") # Description

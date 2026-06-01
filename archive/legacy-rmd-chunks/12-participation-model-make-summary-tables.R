## selection_df quant metadata:
sel_quant_meta <- tribble(
  ~var, ~label,              
  "AGE", "Age",
  "HH_SIZE", "Household size",
  "ENROLLMENT_COST", "Enrollment cost (Rs.)",
  # district‐level:
  "dmean_num_IS_EDU_FREE", "Educ. free available? (Yes = 1)",
  "dmean_num_TUTION_FEE_WAIVED", "Tuition waived?",
  "dmean_num_RECD_SCHOLARSHIP_STIPEND", "Scholarship/Stipend?",
  "dmean_num_RECD_TXT_BOOKS", "Textbooks received?",
  "dmean_num_RECD_STATIONERY", "Stationery received?",
  "dmean_num_MID_DAY_MEAL_ETC_RECD", "Mid-day meal or more received?"
)

## selection_df categorical metadata:
sel_cat_meta <- tribble(
  ~var, ~label,
  "SEX", "Sex",
  "RELIGION", "Religion",
  "SOCIAL_GROUP", "Social group",
  "SECTOR", "Urban",
  "DIST_FROM_NEAREST_PRIMARY_CLASS", "Distance of nearest primary class",
  "father_educ", "Father's education"
)

# Summarize selection_df quant vars
# Process data
sel_quant_stats <- selection_df %>%
  summarize(across(
    all_of(sel_quant_meta$var),
    list(
      Min = ~min(.x, na.rm = TRUE),
      `1Q` = ~quantile(.x, .25, na.rm = TRUE),
      Med = ~median(.x, na.rm = TRUE),
      `3Q` = ~quantile(.x, .75, na.rm = TRUE),
      Max = ~max(.x, na.rm = TRUE),
      Mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to  = c("var","stat"),
    # Only split off ONE of the 7 stats at the end
    names_pattern = "^(.*)_(Min|1Q|Med|3Q|Max|Mean|SD)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  left_join(sel_quant_meta, by = "var") %>%

# Add number of observation column
  left_join(
    selection_df %>%
    summarize(across(all_of(sel_quant_meta$var),
                     ~ sum(!is.na(.x)))) %>%
    pivot_longer(everything(),
                 names_to  = "var",
                 values_to = "N"), 
    by="var"
    ) %>% 
# Round and format: Set var as factor with intended order, then arrange
  mutate(
    var = factor(var, levels = sel_quant_meta$var),
    across(
      Min:SD,
      ~ case_when(
          var == "ENROLLMENT_COST" ~ comma(.x, accuracy = 0.01),
          TRUE ~ sprintf("%.2f", .x)
        )
    )
  ) %>% 
  arrange(var) %>% 
  select(var, label, N, Min, `1Q`, Med, `3Q`, Max, Mean, SD)



# Summarize selection_df cat vars

sel_cat_stats <- selection_df %>%
  select(all_of(sel_cat_meta$var)) %>%
  map_df(~ {
    freq <- table(.x)
    tibble(
      Values = paste(names(freq), collapse=", "),
      Mode = names(freq)[which.max(freq)],
      `% Mode` = round(max(freq)/sum(freq)*100,1),
      `Least Freq.` = names(freq)[which.min(freq)],
      `% Least Freq.`= round(min(freq)/sum(freq)*100,1)
    )
  }, .id="var") %>%
  left_join(sel_cat_meta, by="var") %>%
  # Add number of observations column
  left_join(
    selection_df %>%
    summarize(across(all_of(sel_cat_meta$var),
                     ~ sum(!is.na(.x)))) %>%
    pivot_longer(everything(),
                 names_to  = "var",
                 values_to = "N"), 
    by="var"
    ) %>% 
  mutate(var = factor(var, levels = sel_cat_meta$var)) %>% 
  arrange(var) %>% 
  select(var, label, N, Values, Mode, `% Mode`, `Least Freq.`, `% Least Freq.`)

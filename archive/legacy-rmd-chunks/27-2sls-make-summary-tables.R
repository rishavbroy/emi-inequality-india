## joined_df meta:
joined_meta <- tribble(
  ~var, ~label, ~desc,
  "EMIE", "EMIE", "EMI exposure",
  
  # 2007–08
  "wavg_ling_degrees", "Ling. Distance", "Average linguistic distance of mother tongue from Hindi",
  "npeople_0708", "Population", "Estimated via NSS sample weights",
  "consumption_0708", "Consumption", "Average household monthly consumption expenditures (Rs.)",
  "gini_cons_0708", "Gini of Consumption", "Gini coefficient of consumption",
  "pct_urban", "Pct. Urban", "Percentage of people in an urban area",
  "avg_hh_size", "Avg. HH Size", "Average household size",
  "dependency_ratio", "Dependency Ratio × 100", "Ratio of dependents (0-14, 65+) to labor force (15-64), × 100",
  "pct_fem_head", "Pct. Female Head", "Percentage of households with a female head",
  "pct_hindu", "Pct. Hindu", "Percentage of Hindus",
  "pct_muslim", "Pct. Muslim", "Percentage of Muslims",
  "pct_other_religion", "Pct. Other", "Percentage not Hindu/Muslim",
  "pct_st", "Pct. ST", "Scheduled Tribe",
  "pct_sc", "Pct. SC", "Scheduled Caste",
  "pct_obc", "Pct. OBC", "Other Backward Class",
  "pct_small_land", "Pct. Small Land-Owner", "Owns 0.005–0.40 hectares",
  "pct_medium_land", "Pct. Med. Land-Owner", "Owns 0.41–3.00 hectares",
  "pct_large_land", "Pct. Large Land-Owner", "Owns $\\geq$ 3.01 hectares",
  "pct_head_illiterate", "Pct. Head Educ., Illiterate", "Percentage of household heads with educ. level: illiterate",
  "pct_head_lit_to_primary", "Pct. Head Educ., Lit.-Primary", "Percentage of heads with educ. level: literate-primary",
  "pct_head_secondary_plus", "Pct. Head Educ., Secondary+", "Percentage of heads with educ. level: above secondary",
  "pct_pucca", "Pct. Pucca", "Percentage in pucca (permanent) homes",
  
  # 2017–18
  "npeople_1718", "Population", "Estimated via NSS sample weights",
  "consumption_1718", "Consumption", "Average household monthly consumption expenditures (Rs.)",
  "gini_cons_1718", "Gini of Consumption", "Gini coefficient of consumption",
  
  # Changes
  "consumption_pct_change", "$\\%\\Delta\\text{Consumption}$", "Percent change in consumption",
  "gini_change", "$\\Delta\\text{Gini}^{\\text{Consumption}}$", "Change in the Gini coefficient of consumption"
)


# Summarize joined_df
# Process data
joined_stats <- joined_df %>%
  st_drop_geometry() %>% 
  select(all_of(joined_meta$var)) %>%
  summarize(across(
    everything(),
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
    # Only split on the final underscore, pulling off one of 7 stat‐names
    names_pattern = "^(.*)_(Min|1Q|Med|3Q|Max|Mean|SD)$"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  left_join(joined_meta, by = "var") %>%
  arrange(match(var, joined_meta$var)) 

# Add number of observation column
joined_N <- joined_df %>%
  st_drop_geometry() %>%
  summarize(across(all_of(joined_meta$var),
                   ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything(),
               names_to  = "var",
               values_to = "N")
joined_stats <- joined_stats %>%
  left_join(joined_N, by="var")


# Round and format
joined_stats <- joined_stats %>%
  mutate(
    across(
      Min:SD, 
      ~ ifelse(
        var %in% c("npeople_0708", "npeople_1718"),
        # round to 0 decimals, then add commas
        comma(round(.x, 0)), 
        # otherwise, keep two decimals
        sprintf("%.2f", .x)
        )
      )
    )# %>% mutate(across(Min:SD, ~ signif(.x, 3))) # 3 significant digits


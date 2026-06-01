temp <- edu1718b3 %>%
  # Build district code, pick weights
  mutate(
    district_code_1718 = paste0(NSS_Region, District),
    weight = MULT_Combined
  ) %>%
  # One row per household
  distinct(HHID, .keep_all = TRUE) %>%
  # Strings into numbers
  mutate(
    Household_size = as.numeric(Household_size),
    HH_Con_exp_rs = as.numeric(HH_Con_exp_rs)
  ) %>%
  # Aggregate by district
  group_by(district_code_1718) %>%
  summarise(
    # Number of people
    npeople_1718 = sum(weight * Household_size, na.rm = TRUE),
    # Number of households
    nhouses_1718 = sum(weight, na.rm = TRUE),

    # Mean per‑capita consumption
    consumption_1718 = weighted.mean(
      x = HH_Con_exp_rs / Household_size,
      w = weight,
      na.rm = TRUE
    ),

    # Weighted Gini of per‑capita consumption
    gini_cons_1718 = Gini(
      x = HH_Con_exp_rs / Household_size,
      weights = weight
    )
  ) %>%
  ungroup()
# No households had an NA for consumption

df1718 <- right_join(districts_1718, temp, by = "district_code_1718")



# edu1718b3 %>% distinct(StateDistrict) %>% nrow() - df1718 %>% distinct(district_code_1718) %>% nrow()
# Number of districts did not change in this operation

missing_districts_1718 <- anti_join(districts_1718, df1718, by = "district_code_1718")
# Only Mumbai has no data on it.


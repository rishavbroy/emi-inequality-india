# EMIE: Percentage of enrolled kids in EMI (code "02"), weighted by child‐level weight
temp <- edu0708b5 %>%
  filter(AGE <= 19) %>%
  mutate(district_code_0708 = as.factor(district_code)) %>%
  group_by(district_code_0708) %>%
  arrange(district_code_0708) %>% 
  summarise(
    EMIE = 100*weighted.mean(
      x = (MEDIUM_INSTRUCTION == "02"),
      w = weight,
      na.rm = TRUE
    )
  )
# EMIE = Number of children aged 5-19 enrolled in EMI schools / Total number of children aged 5-19 enrolled in any school


df0708 <- right_join(districts_0708, temp, by = "district_code_0708")




# Household characteristics from edu0708b3. Weighted by household‐level weight
temp <- edu0708b3 %>%
  distinct(district_code, HHID, .keep_all = TRUE) %>%
  mutate(district_code_0708 = as.factor(district_code)) %>%
  group_by(district_code_0708) %>%
  summarise(
    # Total persons in pop.
    npeople_0708 = sum(weight * HH_SIZE, na.rm = TRUE),

    # Total households in pop.
    nhouses_0708 = sum(weight, na.rm = TRUE),

    # Mean per‑capita consumption, weighted
    consumption_0708 = weighted.mean(
      x = TOTAL / HH_SIZE,
      w = weight,
      na.rm = TRUE
    ),

    # Gini of per‑capita consumption, weighted
    gini_cons_0708 = Gini(
      x = TOTAL / HH_SIZE,
      weights = weight
    )
  ) %>%
  ungroup()

df0708 <- right_join(df0708, temp, by = "district_code_0708")




# Merge with IMR after it has been aggregated at the district level
temp <- selection_df %>% 
  group_by(district_code_0708) %>% 
  summarize(
    avg_IMR = weighted.mean(IMR, w = weight, na.rm = TRUE),
    .groups = "drop"
    )

df0708 <- left_join(df0708, temp, by = "district_code_0708")




# Demographic characteristics, from edu0708b4
# Collapse LAND_POSSESSED_CODE into four bins: no_land, small (0.005–0.20), medium (0.21–2.00), large (>2.00)
land_breaks <- c("01", c("02","03","04"), c("05","06","07"), c("08","10","11","12"))

temp <- edu0708b4 %>%
  mutate(
    # Recode land categories
    land_cat4 = case_when(
      LAND_POSSESSED_CODE == "01" ~ "no_land",
      LAND_POSSESSED_CODE %in% c("02","03","04") ~ "small",
      LAND_POSSESSED_CODE %in% c("05","06","07") ~ "medium",
      LAND_POSSESSED_CODE %in% c("08","10","11","12") ~ "large",
      TRUE ~ NA_character_
    ),
    # Numeric education
    edu_num = as.integer(na_if(as.character(EDUCATION_LEVEL), "")),
    # Dependent / working‐age flags
    is_dep   = if_else(AGE <= 14 | AGE >= 65, 1L, 0L),
    is_work  = if_else(AGE >= 15 & AGE <= 64, 1L, 0L)
  ) %>%
  group_by(district_code) %>%
  summarise(
    # Pct urban
    pct_urban = 100*sum(weight * (SECTOR == 2)) / sum(weight),
    
    # Avg household size
    avg_hh_size = sum(weight * HH_SIZE) / sum(weight),
    
    dependency_ratio = 100*sum(weight * is_dep) / sum(weight * is_work),
    
    # Female-headed households
    pct_fem_head = 100*sum(weight * (SEX == 2 & RELATION_TO_HEAD == 1)) / sum(weight),
    
    # Religion pcts
    pct_hindu = 100*sum(weight * (RELIGION == 1)) / sum(weight),
    pct_muslim = 100*sum(weight * (RELIGION == 2)) / sum(weight),
    pct_other_religion = 100*sum(weight * (RELIGION %in% c(3,4,5,6,7,8))) / sum(weight),
    
    # Caste pcts (ref: others)
    pct_st = 100*sum(weight * (SOCIAL_GROUP == "1")) / sum(weight),
    pct_sc = 100*sum(weight * (SOCIAL_GROUP == "2")) / sum(weight),
    pct_obc = 100*sum(weight * (SOCIAL_GROUP == "3")) / sum(weight),
,
    
    # Land possession pcts (ref: no_land)
    pct_small_land = 100*sum(weight * (land_cat4 == "small")) / sum(weight),
    pct_medium_land = 100*sum(weight * (land_cat4 == "medium")) / sum(weight),
    pct_large_land = 100*sum(weight * (land_cat4 == "large")) / sum(weight),
    
    # Education of head (ref: illiterate)
    total_heads = sum(weight * (RELATION_TO_HEAD == 1)),
    pct_head_illiterate = 100*sum(weight * (RELATION_TO_HEAD == 1 & edu_num == 1)) / total_heads,
    pct_head_lit_to_primary = 100*sum(weight * (RELATION_TO_HEAD == 1 & edu_num >= 2 & edu_num <= 7)) / total_heads,
    pct_head_secondary_plus = 100*sum(weight * (RELATION_TO_HEAD == 1 & edu_num >= 8)) / total_heads
  ) %>%
  ungroup() %>% 
    rename(district_code_0708 = district_code)


df0708 <- left_join(df0708, temp, by = "district_code_0708")




temp <- cons0708hhchar %>%
  # If multiple rows per household, keep one
  distinct(HH_ID, .keep_all = TRUE) %>% 
  group_by(District) %>% 
  # Indicator for if Type_of_structure == "1" (pucca)
  mutate(is_pucca = as.integer(Type_of_structure == "1")) %>%  
  # Weighted Percentage
  summarise(
    pct_pucca = 100*sum(Multiplier * is_pucca) / sum(Multiplier),
    .groups = "drop"
  ) %>% 
    rename(district_code_0708 = District)
  
df0708 <- left_join(df0708, temp, by = "district_code_0708")



#### Diagnose issues ####

# edu0708b5 %>% distinct(district_code) %>% nrow() - df0708 %>% distinct(district_code_0708) %>% nrow()
# Number of districts did not change in this operation

missing_districts_0708 <- anti_join(districts_0708, df0708, by = "district_code_0708")
# 7 districts not included. 
# Leh (Ladakh), Jammu and Kashmir: Explicitly excluded due to safety concerns
# Kargil, Jammu and Kashmir: Ditto
# Punch, Jammu and Kashmir: Doesn't have a war named after it like Kargil does, but likely ditto too: instability along Line of Control --> safety concerns, difficult logistics
# New Delhi, Delhi: Representative sampling tough given preponderance of bureaucrats, diplomats, other transient officials
# Central, Delhi: Combo of New Delhi's and Mumbai's reasons
# Mumbai, Maharastra: Rep. samp. tough given slums, rapid cyclical migration, tightly-secured gated/high-rise residential areas 
# Kanniyakumari, Tamil Nadu: Heavy tourist activity (at the southern tip of Indian peninsula) --> difficulties identifying residents, churn in who actually is a resident?
# Will assume sample weights have accounted for the exclusion of these districts

# Dotplot of the ratio (npeople_0708 / nhouses_0708)
plt_nkids_per_house <- ggplot(
  df0708 %>% mutate(
    ratio = npeople_0708 / nhouses_0708,
         diff  = npeople_0708 - nhouses_0708
    ),
  aes(x = district_code_0708, y = ratio)) +
  geom_point(size = 3) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(title = "Average Students Per Household, Per District",
       x = "District (sorted by state and within-state region)", y = "Average Students Per Household") +
  theme_minimal()
# Pretty consistent number of students per household across districts and states!

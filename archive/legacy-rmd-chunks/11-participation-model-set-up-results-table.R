# Define covariate labels, same order in coef_me, se_me, p_me:
labels <- c(
  "Age (years)",
  "Female (ref: Male)",
  "Household size",
  # Religion dummies (ref: Hindu)
  "Religion: Muslim (ref: Hindu)",
  "Religion: Christian",
  "Religion: Sikh",
  "Religion: Jain",
  "Religion: Buddhist",
  "Religion: Zoroastrian",
  "Religion: Other",
  # Social group dummies (ref: Other)
  "Social group: Scheduled Tribe (ref: Other)",
  "Social group: Scheduled Caste",
  "Social group: Other Backward Class",
  # sector
  "Urban (ref: Rural)",
  # distance dummies (ref: <1km)
  "Distance 1–2km (ref: <1km)",
  "Distance 2–3km",
  "Distance 3–5km",
  "Distance > 5km",
  # parental education levels (ref: Illiterate)
  "Father's educ.: Literate, no school (ref: Illiterate)",
  "Father's educ.: Literate, school < primary",
  "Father's educ.: Primary",
  "Father's educ.: Upper primary",
  "Father's educ.: Secondary",
  "Father's educ.: Higher secondary",
  "Father's educ.: Postsecondary+",
  # district‐level instruments (shares; ref = 0)
  "Educ. free available (ref: No)",
  "Tuition waiver received",
  "Scholarship/Stipend received",
  "Textbook(s) received",
  "Stationery received",
  "Mid-day meal, etc. received",
  # mean cost
  "Enrollment cost (Rs.)"
)



# Map (term, contrast) in mfx_all to label in labels
lookup <- tribble(
  ~term, ~contrast, ~Term,
  "AGE", "dY/dX", "Age (years)",
  "SEX", "Female - Male", "Female (ref: Male)",
  "HH_SIZE", "dY/dX", "Household size",

  # RELIGION (ref: Hindu)
  "RELIGION","Muslim - Hindu","Religion: Muslim (ref: Hindu)",
  "RELIGION","Christian - Hindu","Religion: Christian",
  "RELIGION","Sikh - Hindu","Religion: Sikh",
  "RELIGION","Jain - Hindu","Religion: Jain",
  "RELIGION","Buddhist - Hindu","Religion: Buddhist",
  "RELIGION","Zoroastrian - Hindu","Religion: Zoroastrian",
  "RELIGION","Other - Hindu","Religion: Other",

  # SOCIAL_GROUP (ref: Other) — match labels exactly
  "SOCIAL_GROUP","Scheduled Tribe - Other","Social group: Scheduled Tribe (ref: Other)",
  "SOCIAL_GROUP","Scheduled Caste - Other","Social group: Scheduled Caste",
  "SOCIAL_GROUP","Other Backward Class - Other","Social group: Other Backward Class",

  # SECTOR
  "SECTOR","Urban - Rural","Urban (ref: Rural)",

  # DIST_FROM_NEAREST_PRIMARY_CLASS (ref: d<1km)
  "DIST_FROM_NEAREST_PRIMARY_CLASS","1km <= d <2kms - d<1km","Distance 1–2km (ref: <1km)",
  "DIST_FROM_NEAREST_PRIMARY_CLASS","2kms<= d <3kms - d<1km","Distance 2–3km",
  "DIST_FROM_NEAREST_PRIMARY_CLASS","3kms <= d <5kms - d<1km","Distance 3–5km",
  "DIST_FROM_NEAREST_PRIMARY_CLASS","d>=5kms - d<1km","Distance > 5km",

  # father_educ (ref: Illiterate)
  "father_educ","Literate, no school - Illiterate","Father's educ.: Literate, no school (ref: Illiterate)",
  "father_educ","Literate, school < primary - Illiterate","Father's educ.: Literate, school < primary",
  "father_educ","Primary - Illiterate","Father's educ.: Primary",
  "father_educ","Upper primary - Illiterate","Father's educ.: Upper primary",
  "father_educ","Secondary - Illiterate","Father's educ.: Secondary",
  "father_educ","Higher secondary - Illiterate","Father's educ.: Higher secondary",
  "father_educ","Postsecondary+ - Illiterate","Father's educ.: Postsecondary+",

  # district-level means
  "dmean_num_IS_EDU_FREE","dY/dX","Educ. free available (ref: No)",
  "dmean_num_TUTION_FEE_WAIVED","dY/dX","Tuition waiver received",
  "dmean_num_RECD_SCHOLARSHIP_STIPEND","dY/dX","Scholarship/Stipend received",
  "dmean_num_RECD_TXT_BOOKS","dY/dX","Textbook(s) received",
  "dmean_num_RECD_STATIONERY","dY/dX","Stationery received",
  "dmean_num_MID_DAY_MEAL_ETC_RECD","dY/dX","Mid-day meal, etc. received",
  "dmean_num_ENROLLMENT_COST","dY/dX","Enrollment cost (Rs.)"
)




# Make sure it mapped correctly
mfx_df <- as.data.frame(mfx_all) %>%
  dplyr::left_join(lookup, by = c("term","contrast"))

# Catch unmapped rows
unmapped <- mfx_df %>% dplyr::filter(is.na(Term)) %>% dplyr::select(term, contrast) %>% unique()
if (nrow(unmapped) > 0) {
  warning("Unmapped (term, contrast) pairs found:\n",
          paste0(utils::capture.output(print(unmapped)), collapse = "\n"))
}



# Build df and add stars & parentheses:
reg_tbl_selection <- as.data.frame(mfx_df) %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    ),
    Estimate = sprintf("%.3f%s", estimate, stars),
    `Std. Error` = sprintf("(%.3f)", std.error),
    Term = factor(Term, levels = labels)
  ) %>%
  arrange(Term) %>% 
  select(Term, Estimate, `Std. Error`)
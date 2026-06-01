# Goal: Regress on variables which affect the probability one enrolls in or stays enrolled in school, but which do not otherwise affect district-level outcomes of interest (consumption, wages, etc.)


# Collect data on those aged 5-19 who are in school
selection_df <- edu0708b5 %>%
  filter(AGE <= 19) %>%
  mutate(
    PID = PID,
    district_code = district_code,
    enrolled = 1,
    weight = weight,
    FSU_SL_NO = FSU_SL_NO,
    HHID = HHID,
    STATE = STATE,
    STRATUM = STRATUM,
    SUB_STRATUM_NO = SUB_STRATUM_NO,
    IS_EDU_FREE = factor(
      IS_EDU_FREE, 
      levels = c(1,2), 
      labels = c("Yes","No")),
    TUTION_FEE_WAIVED = factor(
      if_else(TUTION_FEE_WAIVED %in% c(1,2), "Yes","No"), 
      levels = c("Yes","No")), # Note: 1 and 2 = waiver received. 3 = No waiver received. 5 NAs, labeled as No
    RECD_SCHOLARSHIP_STIPEND = factor(
      RECD_SCHOLARSHIP_STIPEND,
      levels = c(1,2),
      labels = c("Yes","No")), # No NAs
    RECD_TXT_BOOKS = factor(
      if_else(RECD_TXT_BOOKS %in% c(1,2),"Yes","No"), 
      levels = c("Yes","No")), # 80 NAs, labeled No
    RECD_STATIONERY = factor(
      if_else(RECD_STATIONERY %in% c(1,2),"Yes","No"), 
      levels = c("Yes","No")), # 151 NAs, labeled No
    MID_DAY_MEAL_ETC_RECD = factor(
      MID_DAY_MEAL_ETC_RECD, 
      levels = c(1,2), 
      labels = c("Yes","No")), # No NAs
    .keep = "none"
  )


# Merge with more data on those aged 5-19 who are in school
temp <- edu0708b6 %>%
  filter(AGE <= 19) %>%
  mutate(
    PID = PID,
    district_code = district_code,
    enrolled = 1,
    weight = weight,
    FSU_SL_NO = FSU_SL_NO,
    HHID = HHID,
    STATE = STATE, 
    STRATUM = STRATUM, 
    SUB_STRATUM_NO = SUB_STRATUM_NO
  ) %>%
  select(enrolled, weight, PID, district_code, TUTION_FEE, EXAMINATION_FEE, OTHER_FEES_PAYMENTS, BOOKS, STATIONERY, UNIFORM, TRANSPORT, FSU_SL_NO, HHID, STATE, STRATUM, SUB_STRATUM_NO)

selection_df <- full_join(selection_df, temp, by = join_by(PID, district_code, enrolled, weight, FSU_SL_NO, HHID, STATE, STRATUM, SUB_STRATUM_NO)) # Equivalent to excluding district_code, enrolled, and weight; doing this, however, prevents equivalent .x and .y columns from forming (e.g., enrolled.x and enrolled.y)


# Merge with data on all who are aged 5-19, including those who are not enrolled in school
temp <- edu0708b4 %>%
  filter(AGE >= 5, AGE <= 19) %>%
  mutate(
    AGE = AGE,
    SEX = factor(
      SEX,
      levels = c(1,2),
      labels = c("Male","Female")),
    HH_SIZE = HH_SIZE,
    RELIGION = factor(
      RELIGION, 
      levels = 1:8, 
      labels = c("Hindu","Muslim","Christian","Sikh","Jain","Buddhist","Zoroastrian","Other")),
    SOCIAL_GROUP = factor(
      SOCIAL_GROUP, 
      levels = c(1,2,3,9), 
      labels = c("Scheduled Tribe","Scheduled Caste","Other Backward Class","Other")),
    SECTOR = factor(
      SECTOR,
      levels = c(1, 2),
      labels = c("Rural","Urban")),
    DIST_FROM_NEAREST_PRIMARY_CLASS = factor(
      DIST_FROM_NEAREST_PRIMARY_CLASS,
      levels = 1:5,
      labels = c("d<1km",
                 "1km <= d <2kms",
                 "2kms<= d <3kms",
                 "3kms <= d <5kms",
                 "d>=5kms")),
    district_code = district_code,
    PID = PID,
    weight = weight,
    FSU_SL_NO = FSU_SL_NO,
    HHID = HHID,
    STATE = STATE, 
    STRATUM = STRATUM, 
    SUB_STRATUM_NO = SUB_STRATUM_NO,
    .keep = "none"
  )

selection_df <- full_join(selection_df, temp, by = join_by(PID, district_code, weight, FSU_SL_NO, HHID, STATE, STRATUM, SUB_STRATUM_NO))




#### Father's education proxy
# Merge with data on father's education
# @azam2013a, @munshi2006a, @singh2013: Father's education as proxy of ability. So use father's education for father_educ

# Father proxy: within-HH male and potential parent. Defined for each HHID in this priority order:
# Male head (RELATION_TO_HEAD==1 & SEX==1)
# Male married child of head (3 & 1) (common for married male to live with parents)
# Male spouse of head (2 & 1) ***OR MAYBE 5 & 1!!! CHANGE IN FUTURE. Misclassification error in data here.
# Male parent/parent-in-law of head (7 & 1)

# Build a household-level "father proxy" table
father_proxy <- edu0708b4 %>%
  mutate(
    educ_collapsed = fct_collapse(
      EDUCATION_LEVEL,
      Illiterate = "01",
      `Literate, no school` = c("02","03","04","05"),
      `Literate, school < primary` = "06",
      Primary = "07",
      `Upper primary` = "08",
      Secondary = "10",
      `Higher secondary` = c("11","12"),
      `Postsecondary+` = c("13","14"),
      .default = NA_character_
    )
  ) %>%
  mutate(
    # priority rank (lower = better)
    father_rank = case_when(
      RELATION_TO_HEAD == 1 & SEX == 1 ~ 1L, # Male head
      RELATION_TO_HEAD == 3 & SEX == 1 ~ 2L, # Male married child
#      RELATION_TO_HEAD == 2 & SEX == 1 ~ 3L, # Male spouse of head; average age of this 11.3, or 11.0 after filtering for female heads. Misclassification error!
      RELATION_TO_HEAD == 7 & SEX == 1 ~ 4L, # Male parent/parent-in-law of head
      TRUE ~ NA_integer_ # Have to specify all are ints
    )
  ) %>%
  group_by(HHID) %>%
  arrange(father_rank, .by_group = TRUE) %>%
  slice(1L) %>% # Best available candidate
  ungroup() %>%
  mutate(
    HHID,
    father_educ = factor(educ_collapsed,
                           levels = c("Illiterate","Literate, no school","Literate, school < primary","Primary","Upper primary","Secondary","Higher secondary","Postsecondary+")),
    .keep = "none"
  )

# Join to selection_df (adds the same parental_educ to all kids in HH)
selection_df <- selection_df %>% left_join(father_proxy, by = "HHID")




#### Enrollment and NA cost removal

# Create "enrolled" variable
selection_df <- selection_df %>%
  rename(district_code_0708 = district_code) %>% 
  mutate(
    # any child not in edu0708b5/b6 gets enrolled=0
    enrolled = if_else(is.na(enrolled), 0, enrolled),
    enrolled = factor(enrolled, levels = 0:1, labels = c("No", "Yes")),
    district_code_0708 = as.factor(district_code_0708)
  )


# Create enrollment cost. 
# Use documentation to remove some NAs (else many NAs would come from e.g., kids with RECD_TXT_BOOKS=="Yes" but an NA in BOOKS)
selection_df <- selection_df %>% 
  mutate(
    # Remove some NAs: Either condition below implies free tuition, per NSS documentation
    TUTION_FEE = if_else(
      is.na(TUTION_FEE) & (IS_EDU_FREE == "Yes" | TUTION_FEE_WAIVED == "Yes"), 
      0, TUTION_FEE
      ),
    # Textbooks received -> books cost 0 if missing
    BOOKS = if_else(
      is.na(BOOKS) & RECD_TXT_BOOKS == "Yes",
      0, BOOKS
      ),
    # Stationery received -> stationery cost 0 if missing
    STATIONERY = if_else(
      is.na(STATIONERY) & RECD_STATIONERY == "Yes",
      0, STATIONERY
      ),
    ENROLLMENT_COST = TUTION_FEE + # Same spelling as in data
                      EXAMINATION_FEE +
                      OTHER_FEES_PAYMENTS +
                      BOOKS +
                      STATIONERY +
                      UNIFORM +
                      TRANSPORT
  )



# Explicitly set reference level for all factors
# (Note that the 1st level when defining the variables is by default the reference e.g., "Hindu" was already the reference level for RELIGION):
selection_df <- selection_df %>%
  mutate(
    RELIGION = relevel(RELIGION, ref = "Hindu"),
    SOCIAL_GROUP = relevel(SOCIAL_GROUP, ref = "Other"),
    DIST_FROM_NEAREST_PRIMARY_CLASS = 
      relevel(DIST_FROM_NEAREST_PRIMARY_CLASS, ref = "d<1km"),
    father_educ = relevel(father_educ, ref = "Illiterate")
  )
# Confirm
# levels(selection_df$RELIGION)
# levels(selection_df$SOCIAL_GROUP)
# levels(selection_df$DIST_FROM_NEAREST_PRIMARY_CLASS)
# levels(selection_df$father_educ)



#### District-level aggregates for imputation ####

# Many relevant variables are only defined for enrolled kids. In lieu of using multiple imputation to assign them for not-enrolled kids, we use district-level aggregates of these variables as instruments for each child in the district. The exclusion restriction of the Heckman correction holds for the aggregates as it does for the original variables.

enrolled_only_vars <- c(
  "IS_EDU_FREE","TUTION_FEE_WAIVED","RECD_SCHOLARSHIP_STIPEND",
  "RECD_TXT_BOOKS","RECD_STATIONERY","MID_DAY_MEAL_ETC_RECD",
  "ENROLLMENT_COST"
)

# Ensure binary factors, ENROLLMENT_COST are numeric
temp <- selection_df %>%
  mutate(
    across(all_of(enrolled_only_vars), 
           ~case_when(
             # for factors that are Yes/No:
             is.factor(.x) ~ as.numeric(.x == "Yes"),
             # for cost, should already be numeric:
             TRUE ~ as.numeric(.x)
             ),
      .names="num_{col}")
    )

# Take their weighted district-level means
district_agg <- temp %>%
  filter(enrolled == "Yes") %>% 
  group_by(district_code_0708) %>%
  summarize(across(starts_with("num_"), # the numeric versions
                   ~ {
                     val <- weighted.mean(.x, w=weight, na.rm=TRUE) 
                     if (is.nan(val)) NA_real_ else val # Convert NaN (from dividing by 0, no one in district has it) to NA
                   },
                   .names = "dmean_{.col}"),
            .groups="drop"
            )

# For future: Use regional and state level if district level does not exist

# Bring those means in without overwriting
selection_df <- selection_df %>%
  left_join(district_agg, by="district_code_0708")

# May be better to give unenrolled kids the weighted average enrollment cost of enrolled kids who are a) in the same district, b) with the same values for all the probit_controls variables, except for c) AGE, where we include kids up to two years younger and two years older? May end up with far more missing values, however.


# Merge state names, region names
selection_df <- selection_df %>% left_join(
  districts_0708 %>% mutate(district_code_0708 = as.character(district_code_0708)), 
  by = "district_code_0708")


# Build regressor lists
probit_controls   <- c("AGE",
                       "SEX",
                       "HH_SIZE",
                       "RELIGION",
                       "SOCIAL_GROUP",
                       "SECTOR")

probit_excl_restr_all_kids <- c("DIST_FROM_NEAREST_PRIMARY_CLASS",
                       "father_educ")

probit_excl_restr_enrolled_only <- c(
  "dmean_num_IS_EDU_FREE","dmean_num_TUTION_FEE_WAIVED","dmean_num_RECD_SCHOLARSHIP_STIPEND",
  "dmean_num_RECD_TXT_BOOKS","dmean_num_RECD_STATIONERY","dmean_num_MID_DAY_MEAL_ETC_RECD",
  "dmean_num_ENROLLMENT_COST"
)


probit_vars <- c(probit_controls, probit_excl_restr_all_kids, probit_excl_restr_enrolled_only)

# Create formula from regressor vectors
f_probit <- reformulate(
  probit_vars,
  response = "enrolled"
  )
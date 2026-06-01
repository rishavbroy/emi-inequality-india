#### All variables: Variables/Regions with NAs ####


# Variables with NAs
# Only variables in the probit
counts <- selection_df %>%
  summarise(across(all_of(probit_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NA_count") %>%
  filter(NA_count >= 0) %>%
  arrange(desc(NA_count))

total <- selection_df %>%
  mutate(any_NA_probit = !complete.cases(across(all_of(probit_vars)))) %>%
  summarise(n = sum(any_NA_probit)) %>%
  pull(n)

sel_NA_vars <- bind_rows(
  counts,
  tribble(~variable, ~NA_count,
          "Total probit-relevant with NA", total,
          "Total probit-relevant with no NA", selection_df %>% 
            select(all_of(probit_vars)) %>%
            drop_na() %>% nrow())
  )


# Regions with the _most_ NAs

temp <- selection_df %>%
  mutate(
    any_na_row = !complete.cases(across(all_of(probit_vars))),  # ANY NA in the whole row (strict)
    across(
      all_of(
        c("dmean_num_ENROLLMENT_COST",
          "father_educ",
          "DIST_FROM_NEAREST_PRIMARY_CLASS"
          )
        ),
      ~ as.integer(is.na(.)),
      .names = "miss_{col}"),
    # Demographic shares
    is_urban = as.integer(SECTOR == "Urban"),
    is_female = as.integer(SEX == "Female"),
    is_hindu = as.integer(RELIGION == "Hindu"),
    is_muslim = as.integer(RELIGION == "Muslim"),
    is_st_sc_obc = as.integer(SOCIAL_GROUP %in% c("Scheduled Tribe","Scheduled Caste","Other Backward Class"))
  )

sel_NA_regions_cost <- temp %>% 
  group_by(state_0708, region_0708) %>%
  summarise(
    n = n(),
    pct_any_na = mean(any_na_row, na.rm = TRUE),
    pct_miss_dmean_num_ENROLLMENT_COST = mean(miss_dmean_num_ENROLLMENT_COST, na.rm = TRUE),
    pct_miss_DIST_FROM_NEAREST_PRIMARY = mean(miss_DIST_FROM_NEAREST_PRIMARY_CLASS, na.rm = TRUE),
    pct_miss_father_educ = mean(miss_father_educ, na.rm = TRUE),
    pct_urban = mean(is_urban, na.rm = TRUE),
    pct_female = mean(is_female, na.rm = TRUE),
    pct_hindu = mean(is_hindu, na.rm = TRUE),
    pct_muslim = mean(is_muslim, na.rm = TRUE),
    pct_st_sc_obc = mean(is_st_sc_obc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_miss_dmean_num_ENROLLMENT_COST)) %>%
  slice_head(n = 20) %>%
  mutate(across(starts_with("pct_"), ~round(100*.x, 2)))  # to percentages

# Case study:
# selection_df %>%
#   filter(
#     state_0708=="Rajasthan" &
#       region_0708=="Southern"
#     ) %>%
#   filter(if_any(everything(),is.na)) %>%
#   select(where(anyNA)) %>%
#   summarise(across(everything(), ~sum(is.na(.)))) %>%
#   View
# selection_df %>%
#   filter(
#     state_0708=="Rajasthan" &
#       region_0708=="Southern"
#     ) %>%
#   filter(if_any(everything(),is.na)) %>%
#   select(where(anyNA)) %>%
#   View
# Southern region of Rajasthan: People with one missing cost variable often have ones which aren't missing. The person as a whole wasn't excluded from the data.
# Explanations: Surveyor messed up writing some costs but not others. Or surveyor meant to put a 0.  

sel_NA_regions_dist <- temp %>% 
  group_by(state_0708, region_0708) %>%
  summarise(
    n = n(),
    pct_any_na = mean(any_na_row, na.rm = TRUE),
    pct_miss_dmean_num_ENROLLMENT_COST = mean(miss_dmean_num_ENROLLMENT_COST, na.rm = TRUE),
    pct_miss_DIST_FROM_NEAREST_PRIMARY = mean(miss_DIST_FROM_NEAREST_PRIMARY_CLASS, na.rm = TRUE),
    pct_miss_father_educ = mean(miss_father_educ, na.rm = TRUE),
    pct_urban = mean(is_urban, na.rm = TRUE),
    pct_female = mean(is_female, na.rm = TRUE),
    pct_hindu = mean(is_hindu, na.rm = TRUE),
    pct_muslim = mean(is_muslim, na.rm = TRUE),
    pct_st_sc_obc = mean(is_st_sc_obc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_miss_DIST_FROM_NEAREST_PRIMARY)) %>%
  slice_head(n = 20) %>%
  mutate(across(starts_with("pct_"), ~round(100*.x, 2)))

sel_NA_regions_father <- temp %>% 
  group_by(state_0708, region_0708) %>%
  summarise(
    n = n(),
    pct_any_na = mean(any_na_row, na.rm = TRUE),
    pct_miss_dmean_num_ENROLLMENT_COST = mean(miss_dmean_num_ENROLLMENT_COST, na.rm = TRUE),
    pct_miss_DIST_FROM_NEAREST_PRIMARY = mean(miss_DIST_FROM_NEAREST_PRIMARY_CLASS, na.rm = TRUE),
    pct_miss_father_educ = mean(miss_father_educ, na.rm = TRUE),
    pct_urban = mean(is_urban, na.rm = TRUE),
    pct_female = mean(is_female, na.rm = TRUE),
    pct_hindu = mean(is_hindu, na.rm = TRUE),
    pct_muslim = mean(is_muslim, na.rm = TRUE),
    pct_st_sc_obc = mean(is_st_sc_obc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_miss_father_educ)) %>%
  slice_head(n = 20) %>%
  mutate(across(starts_with("pct_"), ~round(100*.x, 2)))

# Potential chi-square tests for independence
# # ANY NA vs. state
# chisq.test(table(temp$any_na_row, temp$state_0708))
# # ANY NA vs region
# chisq.test(table(temp$any_na_row, temp$region_0708))





# Define all variables

# Missing variables defined only for enrolled
miss_vars_enrolled <- c("TUTION_FEE","EXAMINATION_FEE","OTHER_FEES_PAYMENTS", "BOOKS","STATIONERY","UNIFORM","TRANSPORT") # ENROLLMENT_COST excluded

# Missing variables defined for all
miss_vars_all <- c("DIST_FROM_NEAREST_PRIMARY_CLASS",
                  "dmean_num_ENROLLMENT_COST",
                  "father_educ")

# Variables I'm most worried about explaining these misses
miss_vars <- c(miss_vars_all, miss_vars_enrolled)

group_vars <- c("SECTOR","SEX","RELIGION","SOCIAL_GROUP", "state_0708")
cts_vars <- c("AGE","HH_SIZE")




#### Correlation matrix ####

# For all observations
# Make new df with missingness indicator vars
temp1 <- selection_df %>% 
  mutate(
    across(all_of(miss_vars_all), 
           ~as.integer(is.na(.)),
           .names = "miss_{.col}"),
    .keep = "none"
  )
# Expand factors to set of dummy variables
temp2 <- model.matrix(~.-1, data = selection_df %>% select(all_of(group_vars))) # Do we need to substract by 1?
# Compute correlation matrix
sel_corr_mat_all <- cor(
  cbind(
    temp1, 
    selection_df %>% select(all_of(cts_vars)), 
    temp2
    ),
  use = "pairwise.complete.obs" # Corr found for complete pairs. Allows for NA entries. Pearson method.
)

# For enrolled observations
# Make new df with missingness indicator vars
temp1 <- selection_df %>% 
  filter(enrolled=="Yes") %>% 
  mutate(
    across(all_of(miss_vars_enrolled), 
           ~as.integer(is.na(.)),
           .names = "miss_{.col}"),
    .keep = "none"
  )
# Expand factors to set of dummy variables
temp2 <- model.matrix(~.-1, data = selection_df %>% filter(enrolled=="Yes") %>% select(all_of(group_vars))) # Do we need to substract by 1?
# Compute correlation matrix
sel_corr_mat_enrolled <- cor(
  cbind(
    temp1, 
    selection_df %>% filter(enrolled=="Yes") %>% select(all_of(cts_vars)), 
    temp2
    ),
  use = "pairwise.complete.obs" # Corr found for complete pairs. Allows for NA entries. Pearson method.
)




#### Logistic per missing variable (parallelized) ####
# One logistic per missing variable. Summarize coefficient significant, overall fit

check_missing_logit_parallel <- function(df, miss_vars, covars, method_p = "BH") {
  # RHS of regression
  rhs <- paste(covars, collapse = " + ") 
  
  # Fitting function for binary outcome is.na(m) per missing var m
  fit_one <- function(m) {
    f <- as.formula(paste0("is.na(", m, ") ~ ", rhs))
    fit <- glm(f, data = df, family = binomial)
    # Model-level pseudo-R2 calculated once
    pseudoR2 <- 1 - fit$deviance / fit$null.deviance
    gl <- glance(fit) # Just in case; for AIC, BIC, etc.
    # Tidy up coefficients (return glm as tidy dfs of coefs), attach metadata (missing var's name)
    tidy(fit) %>%
      mutate(
        missing_var = m,
        pseudoR2 = pseudoR2
      )
  }
  
  # Parallelization
  # Cluster with parLapply() if Windows. (**I HAVE NOT CONFIRMED THAT THIS CODE WORKS.**)
  out_list <- if (.Platform$OS.type == "windows") {
    cl <- makeCluster(max(1, detectCores() - 1))
    on.exit(stopCluster(cl), add = TRUE) # Shut down cluster even if errors occur
    parLapply(cl, miss_vars, fit_one)
  } 
  # Fork with mclapply() if not
  else {
    mclapply(miss_vars, fit_one, mc.cores = max(1, detectCores() - 1))
  } # Returns list of tidy dfs, one per missing var
  
  out <- bind_rows(out_list) # Stack the dfs
  out$p_adj <- p.adjust(out$p.value, method = method_p) # Adjust p-values to control for multiple comparisons problem
  out
}

covars <- c("SECTOR","SEX","AGE","HH_SIZE",
            "RELIGION","SOCIAL_GROUP",
            "state_0708")



# Logit results for missing variables defined for all
logit_missing_results_all <- check_missing_logit_parallel(selection_df, miss_vars_all, covars, method_p = "BH") # "BH" = Benjamini-Hochberg control on false discovery rate. Independence or positive dependence assumptions weaken to arbitrary dependence for "BY", but by being more conservative, its power also weakens (more p-vals above 0.05).

sel_logit_NA_all <- logit_missing_results_all %>%
  group_by(missing_var) %>% # Use name from function
  summarise(
    n_sig = sum(p_adj < 0.05 & term != "(Intercept)"),
    pseudoR2 = max(pseudoR2, na.rm = TRUE) # Model-level metric
  ) %>%
  arrange(desc(pseudoR2))
# So n_sig = # of predictors which survive FDR at 0.05 significance (i.e., # of covariates associated with missingness)

# Predictors of missingness for missing vars in probit (dmean_num_ENROLLMENT_COST and DIST_FROM_NEAREST_PRIMARY_CLASS)
sel_predlogit_NA_all <- logit_missing_results_all %>% 
  filter(
    missing_var %in% miss_vars,
    term != "(Intercept)",
    p_adj < 0.05) %>% 
  mutate(predictor = term) %>%
  select(missing_var, predictor, estimate, p_adj) %>% 
  arrange(missing_var, p_adj, desc(abs(estimate)))

# Plot results: variables with most non-random missingness
plt_sel_logit_NA_all <- logit_missing_results_all %>%
  group_by(missing_var) %>%
  summarise(pseudoR2 = max(pseudoR2, na.rm = TRUE)) %>%
  ggplot(aes(reorder(missing_var, pseudoR2), pseudoR2)) +
  geom_col() +
  coord_flip() +
  labs(y = "Pseudo-R^2 (predictability of missingness)",
       x = "Variable",
       title = "How structured is missingness?")



# Logit results for missing variables defined for enrolled
logit_missing_results_enrolled <- check_missing_logit_parallel(selection_df, miss_vars_enrolled, covars, method_p = "BH") # "BH" = Benjamini-Hochberg control on false discovery rate. Independence or positive dependence assumptions weaken to arbitrary dependence for "BY", but by being more conservative, its power also weakens (more p-vals above 0.05).

sel_logit_NA_enrolled <- logit_missing_results_enrolled %>%
  group_by(missing_var) %>% # Use name from function
  summarise(
    n_sig = sum(p_adj < 0.05 & term != "(Intercept)"),
    pseudoR2 = max(pseudoR2, na.rm = TRUE) # Model-level metric
  ) %>%
  arrange(desc(pseudoR2))
# So n_sig = # of predictors which survive FDR at 0.05 significance (i.e., # of covariates associated with missingness)

# Predictors of missingness for missing vars in probit (dmean_num_ENROLLMENT_COST and DIST_FROM_NEAREST_PRIMARY_CLASS)
sel_predlogit_NA_enrolled <- logit_missing_results_enrolled %>% 
  filter(
    missing_var %in% miss_vars,
    term != "(Intercept)",
    p_adj < 0.05) %>% 
  mutate(predictor = term) %>%
  select(missing_var, predictor, estimate, p_adj) %>% 
  arrange(missing_var, p_adj, desc(abs(estimate)))

# Plot results: variables with most non-random missingness
plt_sel_logit_NA_enrolled <- logit_missing_results_enrolled %>%
  group_by(missing_var) %>%
  summarise(pseudoR2 = max(pseudoR2, na.rm = TRUE)) %>%
  ggplot(aes(reorder(missing_var, pseudoR2), pseudoR2)) +
  geom_col() +
  coord_flip() +
  labs(y = "Pseudo-R^2 (predictability of missingness)",
       x = "Variable",
       title = "How structured is missingness?")


# Potential tests for later: Multivariate logistic regression; variable-by-variable chi-squared, t-tests

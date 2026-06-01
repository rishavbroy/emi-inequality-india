# Handling lonely PSUs ("variance formula  gives 0/0 if stratum contains only one sampling unit", `?svyCprod`)
options(survey.lonely.psu = "average")

# Options: ""fail" to give an error, "remove" or "certainty" to give a variance contribution of 0 for the stratum, "adjust" to center the stratum at the grand mean rather than the stratum mean, and "average" to assign strata with one PSU the average variance contribution from strata with more than one PSU."
# ""adjust" is conservative, and it would often be better to combine strata in some intelligent way."
# "properties of "average" have not been investigated thoroughly, but it may be useful when the lonely PSUs are due to a few strata having PSUs missing completely at random."
# ""remove" and "certainty" options give the same result" 
# "but "certainty" is intended for situations where there is only one PSU in the population stratum, which is sampled with certainty (also called ‘self-representing’ PSUs or strata). With "certainty" no warning is generated for strata with only one PSU. Ordinarily, svydesign will detect certainty PSUs, making this option unnecessary."

# Went with average on assumption svydesign would've otherwise detected that stratum should have only one PSU

# Ex: The stratum corresponding to the "Jhelam Valley"  district (a beauiful, hilly area known for tourism, earthquakes, and proximity to the conflict-ridden Line of Control between Pakistan and India) in Jammu and Kashmir has only one PSU in the data.


# Create weight survey design model
selection_design <- svydesign(
  
  ids = ~FSU_SL_NO, 
  # Sampling occured in two stages: the first-stage units (FSU) were rural villages/urban blocks (FSU_SL_NO), and the ultimate stage units (USUs) were households (HHID)
  
  strata = ~interaction(
    as.character(STATE),
    as.character(STRATUM),
    as.character(SUB_STRATUM_NO),
    drop = TRUE # Nothing was dropped; kept just in case
    ), 
  # Sampling stratified into state, stratum (district), and substratum (rural/urban, with multiple urban substrata if population dense enough)
# ?svydesign: "For multistage sampling the id argument should specify a formula with the cluster identifiers at each stage."

  weights = ~weight,
  data = selection_df,
  
  nest = TRUE # Clusters (PSUs) are nested within strata (states*strata*substrata)
)

# Estimate weighted model
model_probit_selection <- svyglm(
  f_probit,
  design = selection_design,
  family = binomial(link = "probit")
) # Drops rows with an NA in a probit variable; specious!



# Derive IMR
# For now, calculate IMR for all rows. Don't drop rows with NAs in unrelated columns:

# Rows with no NA under probit variable
needed_vars <- all.vars(f_probit) # = c(probit_vars, "enrolled") 

# Take only complete cases of variables passed to svydesign
comp_cases <- complete.cases(selection_df[, needed_vars, drop = FALSE])

# Predict only on complete rows; NA for rest
lp <- rep(NA_real_, nrow(selection_df)) # full-length placeholders
lp[comp_cases] <- predict(model_probit_selection,
                         newdata = selection_df[comp_cases, , drop=FALSE],
                         type = "link") # "link" means coefs = linear predictors \eta. "response" would make coefs = probabilities \Phi(\eta).

# Build IMR
phi_lp <- dnorm(lp)
Phi_lp <- pnorm(lp)

# Remaining NAs could be from dividing by 0. 
# Ensure against this by keeping Phi_lp \in (0,1) strictly
Phi_lp <- pmin(pmax(Phi_lp, .Machine$double.eps), 1 - .Machine$double.eps)
# .Machine$double.eps = smallest floating-point (x > 0) s.t. (1 + x != 1) = 2.220446e-16 on my device

selection_df <- selection_df %>%
  mutate(
    IMR = case_when(
      enrolled == "Yes" ~ phi_lp / Phi_lp,
      enrolled == "No"  ~ phi_lp / (1 - Phi_lp),
      TRUE ~ NA_real_ # NA if missing predictor
      )
  )
# Note: Each NA will be stripped from IMR column when aggregating it later

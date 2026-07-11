# Missingness Diagnostics


## Legacy comments

### Legacy Chunk 8: missingness checks

### All variables: Variables/Regions with NAs

Variables with NAs Only variables in the probit

Regions with the *most* NAs

Demographic shares

Case study:

``` r
selection_df %>%
  filter(
    state_0708=="Rajasthan" &
      region_0708=="Southern"
    ) %>%
  filter(if_any(everything(),is.na)) %>%
  select(where(anyNA)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  View
selection_df %>%
  filter(
    state_0708=="Rajasthan" &
      region_0708=="Southern"
    ) %>%
  filter(if_any(everything(),is.na)) %>%
  select(where(anyNA)) %>%
  View
Southern region of Rajasthan: People with one missing cost variable often have ones which aren't missing. The person as a whole wasn't excluded from the data.
Explanations: Surveyor messed up writing some costs but not others. Or surveyor meant to put a 0.  
```

Potential chi-square tests for independence ANY NA vs. state

``` r
chisq.test(table(temp$any_na_row, temp$state_0708))
ANY NA vs region
chisq.test(table(temp$any_na_row, temp$region_0708))
```

Define all variables

Missing variables defined only for enrolled

Missing variables defined for all

Variables I’m most worried about explaining these misses

### Correlation matrix

For all observations Make new df with missingness indicator vars Expand
factors to set of dummy variables Compute correlation matrix

For enrolled observations Make new df with missingness indicator vars
Expand factors to set of dummy variables Compute correlation matrix

### Logistic per missing variable (parallelized)

One logistic per missing variable. Summarize coefficient significant,
overall fit

RHS of regression

Fitting function for binary outcome is.na(m) per missing var m
Model-level pseudo-R2 calculated once

``` r
Tidy up coefficients (return glm as tidy dfs of coefs), attach metadata (missing var's name)
```

Parallelization

``` r
Cluster with parLapply() if Windows. (**I HAVE NOT CONFIRMED THAT THIS CODE WORKS.**)
Fork with mclapply() if not
```

Logit results for missing variables defined for all

``` r
So n_sig = # of predictors which survive FDR at 0.05 significance (i.e., # of covariates associated with missingness)
```

``` r
Predictors of missingness for missing vars in probit (dmean_num_ENROLLMENT_COST and DIST_FROM_NEAREST_PRIMARY_CLASS)
```

Plot results: variables with most non-random missingness

Logit results for missing variables defined for enrolled

``` r
So n_sig = # of predictors which survive FDR at 0.05 significance (i.e., # of covariates associated with missingness)
```

``` r
Predictors of missingness for missing vars in probit (dmean_num_ENROLLMENT_COST and DIST_FROM_NEAREST_PRIMARY_CLASS)
```

Plot results: variables with most non-random missingness

Potential tests for later: Multivariate logistic regression;
variable-by-variable chi-squared, t-tests

**Deviation note.** The prose and commented exploratory code above are
rendered from the legacy comments, with comment markers removed and
code-like comment blocks fenced. The tables below replace manually
inspected objects and plots with current target outputs. The known
`glm.fit: fitted probabilities numerically 0 or 1 occurred` warning is
intentionally left in the target metadata and review archive because it
is a diagnostic warning, not a public-build failure.

## Current targets-backed results

| missing_var                      | n_missing | pct_missing |
|:---------------------------------|----------:|------------:|
| enrolled                         |         0 |       0.000 |
| AGE                              |         0 |       0.000 |
| SEX                              |         0 |       0.000 |
| HH_SIZE                          |         0 |       0.000 |
| RELIGION                         |         0 |       0.000 |
| SOCIAL_GROUP                     |         0 |       0.000 |
| SECTOR                           |         0 |       0.000 |
| state_0708                       |         0 |       0.000 |
| DIST_FROM_NEAREST_PRIMARY_CLASS  |       312 |       0.002 |
| dmean_num_ENROLLMENT_COST        |      7109 |       0.056 |
| father_educ                      |      5205 |       0.041 |
| TUTION_FEE                       |     39994 |       0.314 |
| EXAMINATION_FEE                  |     68331 |       0.537 |
| OTHER_FEES_PAYMENTS              |     71365 |       0.561 |
| BOOKS                            |     38637 |       0.304 |
| STATIONERY                       |     38189 |       0.300 |
| UNIFORM                          |     65463 |       0.514 |
| TRANSPORT                        |    111204 |       0.874 |
| Total probit-relevant with NA    |    120380 |       0.946 |
| Total probit-relevant with no NA |      6866 |       0.054 |

Current missingness counts

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Lakshadweep | all_regions_available_in_state_only_input | 256 | 100.00 | 100.00 | 0.00 | 6.64 | 67.19 | 53.91 | 0.78 | 98.44 | 98.83 | state_only_fallback |
| Arunachal Pradesh | all_regions_available_in_state_only_input | 1485 | 97.64 | 43.77 | 2.49 | 3.43 | 35.22 | 50.98 | 23.03 | 1.68 | 75.15 | state_only_fallback |
| Jharkhand | all_regions_available_in_state_only_input | 3143 | 95.10 | 23.93 | 0.00 | 4.71 | 25.64 | 54.53 | 76.74 | 13.39 | 86.89 | state_only_fallback |
| Bihar | all_regions_available_in_state_only_input | 11185 | 98.39 | 23.86 | 0.78 | 4.53 | 17.47 | 56.43 | 83.77 | 16.04 | 85.28 | state_only_fallback |
| Mizoram | all_regions_available_in_state_only_input | 1533 | 94.13 | 19.24 | 0.00 | 4.04 | 57.21 | 53.62 | 0.46 | 0.26 | 99.54 | state_only_fallback |
| Delhi | all_regions_available_in_state_only_input | 1310 | 90.61 | 14.27 | 0.00 | 4.96 | 88.40 | 56.49 | 79.62 | 15.80 | 47.56 | state_only_fallback |
| Rajasthan | all_regions_available_in_state_only_input | 6676 | 96.49 | 9.57 | 0.00 | 4.75 | 29.21 | 53.86 | 86.31 | 11.70 | 79.07 | state_only_fallback |
| Gujrat | all_regions_available_in_state_only_input | 5289 | 96.62 | 8.75 | 0.00 | 4.18 | 41.44 | 53.28 | 85.65 | 13.06 | 71.79 | state_only_fallback |
| Assam | all_regions_available_in_state_only_input | 2855 | 93.70 | 7.81 | 0.00 | 3.47 | 28.97 | 55.66 | 69.14 | 28.09 | 48.20 | state_only_fallback |
| Uttaranchal | all_regions_available_in_state_only_input | 1843 | 93.38 | 5.86 | 0.81 | 3.64 | 34.13 | 51.33 | 77.65 | 21.32 | 48.51 | state_only_fallback |
| Orissa | all_regions_available_in_state_only_input | 4989 | 95.79 | 4.33 | 0.00 | 4.19 | 22.57 | 51.77 | 95.75 | 2.75 | 79.88 | state_only_fallback |
| Chhattisgarh | all_regions_available_in_state_only_input | 2550 | 94.31 | 4.04 | 0.08 | 4.86 | 30.00 | 52.82 | 95.53 | 1.96 | 90.51 | state_only_fallback |
| Himachal Pradesh | all_regions_available_in_state_only_input | 1847 | 91.07 | 3.84 | 0.00 | 5.25 | 21.12 | 54.20 | 92.69 | 3.09 | 45.97 | state_only_fallback |
| Madhya Pradesh | all_regions_available_in_state_only_input | 7841 | 94.31 | 1.86 | 0.34 | 3.90 | 33.30 | 54.92 | 89.49 | 9.41 | 80.18 | state_only_fallback |
| Karnataka | all_regions_available_in_state_only_input | 4807 | 95.92 | 1.79 | 0.00 | 3.08 | 38.86 | 52.07 | 81.19 | 16.06 | 66.82 | state_only_fallback |
| Uttar Pradesh | all_regions_available_in_state_only_input | 17239 | 95.98 | 1.33 | 0.06 | 5.73 | 26.35 | 54.04 | 76.38 | 23.15 | 80.17 | state_only_fallback |
| Jammu & Kashmir | all_regions_available_in_state_only_input | 2135 | 89.65 | 0.75 | 0.00 | 4.87 | 38.50 | 52.60 | 35.78 | 62.39 | 27.87 | state_only_fallback |
| Andaman & Nicober | all_regions_available_in_state_only_input | 456 | 82.89 | 0.00 | 0.00 | 3.29 | 42.11 | 50.66 | 67.98 | 13.38 | 23.03 | state_only_fallback |
| Andhra Pardesh | all_regions_available_in_state_only_input | 7215 | 97.30 | 0.00 | 0.00 | 3.13 | 33.75 | 50.66 | 87.29 | 10.64 | 73.49 | state_only_fallback |
| Chandigarh | all_regions_available_in_state_only_input | 317 | 86.75 | 0.00 | 0.00 | 5.99 | 75.71 | 58.36 | 79.81 | 5.36 | 37.54 | state_only_fallback |

Regions with the most cost-variable missingness

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Manipur | all_regions_available_in_state_only_input | 2358 | 91.43 | 0.00 | 3.35 | 4.66 | 28.33 | 54.66 | 50.42 | 9.29 | 85.88 | state_only_fallback |
| Arunachal Pradesh | all_regions_available_in_state_only_input | 1485 | 97.64 | 43.77 | 2.49 | 3.43 | 35.22 | 50.98 | 23.03 | 1.68 | 75.15 | state_only_fallback |
| Meghalaya | all_regions_available_in_state_only_input | 1984 | 95.51 | 0.00 | 1.21 | 3.18 | 23.03 | 49.34 | 6.30 | 3.93 | 92.04 | state_only_fallback |
| Tripura | all_regions_available_in_state_only_input | 2647 | 96.49 | 0.00 | 1.13 | 3.74 | 18.02 | 52.81 | 84.21 | 12.05 | 70.04 | state_only_fallback |
| Uttaranchal | all_regions_available_in_state_only_input | 1843 | 93.38 | 5.86 | 0.81 | 3.64 | 34.13 | 51.33 | 77.65 | 21.32 | 48.51 | state_only_fallback |
| Bihar | all_regions_available_in_state_only_input | 11185 | 98.39 | 23.86 | 0.78 | 4.53 | 17.47 | 56.43 | 83.77 | 16.04 | 85.28 | state_only_fallback |
| Madhya Pradesh | all_regions_available_in_state_only_input | 7841 | 94.31 | 1.86 | 0.34 | 3.90 | 33.30 | 54.92 | 89.49 | 9.41 | 80.18 | state_only_fallback |
| Chhattisgarh | all_regions_available_in_state_only_input | 2550 | 94.31 | 4.04 | 0.08 | 4.86 | 30.00 | 52.82 | 95.53 | 1.96 | 90.51 | state_only_fallback |
| Uttar Pradesh | all_regions_available_in_state_only_input | 17239 | 95.98 | 1.33 | 0.06 | 5.73 | 26.35 | 54.04 | 76.38 | 23.15 | 80.17 | state_only_fallback |
| Andaman & Nicober | all_regions_available_in_state_only_input | 456 | 82.89 | 0.00 | 0.00 | 3.29 | 42.11 | 50.66 | 67.98 | 13.38 | 23.03 | state_only_fallback |
| Andhra Pardesh | all_regions_available_in_state_only_input | 7215 | 97.30 | 0.00 | 0.00 | 3.13 | 33.75 | 50.66 | 87.29 | 10.64 | 73.49 | state_only_fallback |
| Assam | all_regions_available_in_state_only_input | 2855 | 93.70 | 7.81 | 0.00 | 3.47 | 28.97 | 55.66 | 69.14 | 28.09 | 48.20 | state_only_fallback |
| Chandigarh | all_regions_available_in_state_only_input | 317 | 86.75 | 0.00 | 0.00 | 5.99 | 75.71 | 58.36 | 79.81 | 5.36 | 37.54 | state_only_fallback |
| Dadra & Nagar Haveli | all_regions_available_in_state_only_input | 277 | 97.11 | 0.00 | 0.00 | 0.36 | 42.96 | 64.26 | 97.83 | 0.36 | 76.90 | state_only_fallback |
| Daman & Diu | all_regions_available_in_state_only_input | 279 | 96.77 | 0.00 | 0.00 | 2.87 | 48.39 | 60.22 | 88.53 | 9.32 | 68.82 | state_only_fallback |
| Delhi | all_regions_available_in_state_only_input | 1310 | 90.61 | 14.27 | 0.00 | 4.96 | 88.40 | 56.49 | 79.62 | 15.80 | 47.56 | state_only_fallback |
| Goa | all_regions_available_in_state_only_input | 308 | 94.16 | 0.00 | 0.00 | 2.92 | 57.47 | 53.90 | 70.78 | 14.29 | 24.03 | state_only_fallback |
| Gujrat | all_regions_available_in_state_only_input | 5289 | 96.62 | 8.75 | 0.00 | 4.18 | 41.44 | 53.28 | 85.65 | 13.06 | 71.79 | state_only_fallback |
| Haryana | all_regions_available_in_state_only_input | 2502 | 89.01 | 0.00 | 0.00 | 2.88 | 35.77 | 56.08 | 85.49 | 7.15 | 57.87 | state_only_fallback |
| Himachal Pradesh | all_regions_available_in_state_only_input | 1847 | 91.07 | 3.84 | 0.00 | 5.25 | 21.12 | 54.20 | 92.69 | 3.09 | 45.97 | state_only_fallback |

Regions with the most distance-to-school missingness

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Sikkim | all_regions_available_in_state_only_input | 1410 | 97.02 | 0.00 | 0.00 | 7.59 | 14.18 | 52.84 | 63.05 | 1.49 | 85.18 | state_only_fallback |
| Lakshadweep | all_regions_available_in_state_only_input | 256 | 100.00 | 100.00 | 0.00 | 6.64 | 67.19 | 53.91 | 0.78 | 98.44 | 98.83 | state_only_fallback |
| Chandigarh | all_regions_available_in_state_only_input | 317 | 86.75 | 0.00 | 0.00 | 5.99 | 75.71 | 58.36 | 79.81 | 5.36 | 37.54 | state_only_fallback |
| Uttar Pradesh | all_regions_available_in_state_only_input | 17239 | 95.98 | 1.33 | 0.06 | 5.73 | 26.35 | 54.04 | 76.38 | 23.15 | 80.17 | state_only_fallback |
| Himachal Pradesh | all_regions_available_in_state_only_input | 1847 | 91.07 | 3.84 | 0.00 | 5.25 | 21.12 | 54.20 | 92.69 | 3.09 | 45.97 | state_only_fallback |
| Delhi | all_regions_available_in_state_only_input | 1310 | 90.61 | 14.27 | 0.00 | 4.96 | 88.40 | 56.49 | 79.62 | 15.80 | 47.56 | state_only_fallback |
| Punjab | all_regions_available_in_state_only_input | 3192 | 88.38 | 0.00 | 0.00 | 4.92 | 40.76 | 55.11 | 39.85 | 2.13 | 53.63 | state_only_fallback |
| Jammu & Kashmir | all_regions_available_in_state_only_input | 2135 | 89.65 | 0.75 | 0.00 | 4.87 | 38.50 | 52.60 | 35.78 | 62.39 | 27.87 | state_only_fallback |
| Chhattisgarh | all_regions_available_in_state_only_input | 2550 | 94.31 | 4.04 | 0.08 | 4.86 | 30.00 | 52.82 | 95.53 | 1.96 | 90.51 | state_only_fallback |
| Rajasthan | all_regions_available_in_state_only_input | 6676 | 96.49 | 9.57 | 0.00 | 4.75 | 29.21 | 53.86 | 86.31 | 11.70 | 79.07 | state_only_fallback |
| Jharkhand | all_regions_available_in_state_only_input | 3143 | 95.10 | 23.93 | 0.00 | 4.71 | 25.64 | 54.53 | 76.74 | 13.39 | 86.89 | state_only_fallback |
| Manipur | all_regions_available_in_state_only_input | 2358 | 91.43 | 0.00 | 3.35 | 4.66 | 28.33 | 54.66 | 50.42 | 9.29 | 85.88 | state_only_fallback |
| Bihar | all_regions_available_in_state_only_input | 11185 | 98.39 | 23.86 | 0.78 | 4.53 | 17.47 | 56.43 | 83.77 | 16.04 | 85.28 | state_only_fallback |
| Orissa | all_regions_available_in_state_only_input | 4989 | 95.79 | 4.33 | 0.00 | 4.19 | 22.57 | 51.77 | 95.75 | 2.75 | 79.88 | state_only_fallback |
| Gujrat | all_regions_available_in_state_only_input | 5289 | 96.62 | 8.75 | 0.00 | 4.18 | 41.44 | 53.28 | 85.65 | 13.06 | 71.79 | state_only_fallback |
| Mizoram | all_regions_available_in_state_only_input | 1533 | 94.13 | 19.24 | 0.00 | 4.04 | 57.21 | 53.62 | 0.46 | 0.26 | 99.54 | state_only_fallback |
| Madhya Pradesh | all_regions_available_in_state_only_input | 7841 | 94.31 | 1.86 | 0.34 | 3.90 | 33.30 | 54.92 | 89.49 | 9.41 | 80.18 | state_only_fallback |
| Tripura | all_regions_available_in_state_only_input | 2647 | 96.49 | 0.00 | 1.13 | 3.74 | 18.02 | 52.81 | 84.21 | 12.05 | 70.04 | state_only_fallback |
| Uttaranchal | all_regions_available_in_state_only_input | 1843 | 93.38 | 5.86 | 0.81 | 3.64 | 34.13 | 51.33 | 77.65 | 21.32 | 48.51 | state_only_fallback |
| Assam | all_regions_available_in_state_only_input | 2855 | 93.70 | 7.81 | 0.00 | 3.47 | 28.97 | 55.66 | 69.14 | 28.09 | 48.20 | state_only_fallback |

Regions with the most father-education missingness

| missing_var                     | n_sig | pseudoR2 |
|:--------------------------------|------:|---------:|
| dmean_num_ENROLLMENT_COST       |     9 |    0.310 |
| DIST_FROM_NEAREST_PRIMARY_CLASS |     3 |    0.246 |
| EXAMINATION_FEE                 |    37 |    0.164 |
| UNIFORM                         |    34 |    0.161 |
| TRANSPORT                       |    38 |    0.151 |
| OTHER_FEES_PAYMENTS             |    39 |    0.134 |
| TUTION_FEE                      |    20 |    0.056 |
| STATIONERY                      |     3 |    0.039 |
| BOOKS                           |     4 |    0.027 |
| father_educ                     |    11 |    0.023 |

Logit summaries for structured missingness

| diagnostic | legacy_status |
|:---|:---|
| rajasthan_southern_case_study | commented diagnostic View() code preserved as documented note |
| chi_square_any_na_by_state | commented diagnostic test not run by default |
| chi_square_any_na_by_region | commented diagnostic test not run by default |
| parallel_missingness_logit | ported using lapply for reproducible targets execution; legacy mclapply/parLapply choice documented |

Legacy missingness notes retained as diagnostics

# Missingness Diagnostics


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

#### All variables: Variables/Regions with NAs

Variables with NAs. Only variables in the probit. Regions with the
*most* NAs.

#### Rajasthan/Southern case study

The legacy case study inspected Rajasthan’s Southern region and
concluded: Southern region of Rajasthan: People with one missing cost
variable often have ones which aren’t missing. The person as a whole
wasn’t excluded from the data. Explanations: Surveyor messed up writing
some costs but not others. Or surveyor meant to put a 0.

Potential chi-square tests for independence: ANY NA vs. state; ANY NA vs
region. The target output now renders those tests when the active
cleaned data expose enough variation.

#### Correlation matrix

For all observations: make new data frame with missingness indicator
variables, expand factors to dummy variables, and compute the
correlation matrix using pairwise-complete observations. For enrolled
observations: repeat the same logic for missing variables defined only
for enrolled children.

#### Logistic per missing variable (parallelized)

One logistic per missing variable. Summarize coefficient significance
and overall fit. So `n_sig` = number of predictors which survive FDR at
0.05 significance (i.e., number of covariates associated with
missingness).

``` r
analysis_deviation_note("The rendered note preserves the legacy diagnostic order and prose while replacing interactive View() calls and platform-specific parallelization with target-backed case-study summaries, chi-square outputs, CSVs, heatmaps, and pseudo-R-squared figures.")
```

**Deviation note.** The rendered note preserves the legacy diagnostic
order and prose while replacing interactive View() calls and
platform-specific parallelization with target-backed case-study
summaries, chi-square outputs, CSVs, heatmaps, and pseudo-R-squared
figures.

``` r
missing_counts <- analysis_target_csv("diag_ext_missingness", "missingness_counts.csv")
regional_cost <- analysis_target_csv("diag_ext_missingness", "regional_missingness_cost.csv")
regional_distance <- analysis_target_csv("diag_ext_missingness", "regional_missingness_distance.csv")
regional_father <- analysis_target_csv("diag_ext_missingness", "regional_missingness_father_education.csv")
logit_summary <- analysis_target_csv("diag_ext_missingness", "missingness_logit_summary.csv")
case_study <- analysis_target_csv("diag_ext_missingness", "missingness_rajasthan_southern_case_study.csv")
chi_square <- analysis_target_csv("diag_ext_missingness", "missingness_chi_square_tests.csv")
legacy_notes <- analysis_target_csv("diag_ext_missingness", "missingness_legacy_notes.csv")
corr_all_pairs <- analysis_target_csv("diag_ext_missingness", "missingness_correlation_all_top_pairs.csv")
corr_enrolled_pairs <- analysis_target_csv("diag_ext_missingness", "missingness_correlation_enrolled_top_pairs.csv")
```

The current active data contain 12,283 probit-model rows with at least
one missing value and 114,898 probit-model rows with no missing value.
Enrolled-only expenditure fields are diagnosed separately rather than
included in this probit-model total.

``` r
missing_counts[order(-missing_counts$n_missing), c("missing_var", "n_missing", "pct_missing"), drop = FALSE]
```

                           missing_var n_missing pct_missing
    14   Total probit-model with no NA    114898 0.903421108
    13      Total probit-model with NA     12283 0.096578892
    11       dmean_num_ENROLLMENT_COST      7109 0.055896714
    12                     father_educ      5203 0.040910199
    10 DIST_FROM_NEAREST_PRIMARY_CLASS       312 0.002453197
    1                         enrolled         0 0.000000000
    2                              AGE         0 0.000000000
    3                              SEX         0 0.000000000
    4                          HH_SIZE         0 0.000000000
    5                         RELIGION         0 0.000000000
    6                     SOCIAL_GROUP         0 0.000000000
    7                           SECTOR         0 0.000000000
    8                       state_0708         0 0.000000000
    9                      region_0708         0 0.000000000

``` r
analysis_table(missing_counts, "Current missingness counts")
```

| missing_var                     | n_missing | pct_missing |
|:--------------------------------|----------:|------------:|
| enrolled                        |         0 |       0.000 |
| AGE                             |         0 |       0.000 |
| SEX                             |         0 |       0.000 |
| HH_SIZE                         |         0 |       0.000 |
| RELIGION                        |         0 |       0.000 |
| SOCIAL_GROUP                    |         0 |       0.000 |
| SECTOR                          |         0 |       0.000 |
| state_0708                      |         0 |       0.000 |
| region_0708                     |         0 |       0.000 |
| DIST_FROM_NEAREST_PRIMARY_CLASS |       312 |       0.002 |
| dmean_num_ENROLLMENT_COST       |      7109 |       0.056 |
| father_educ                     |      5203 |       0.041 |
| Total probit-model with NA      |     12283 |       0.097 |
| Total probit-model with no NA   |    114898 |       0.903 |

Current missingness counts

``` r
analysis_table(regional_cost, "Regions with the most cost-variable missingness", max_rows = 20)
```

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Lakshadweep | Lakshadweep | 256 | 100.00 | 100.00 | 0.00 | 6.64 | 67.19 | 53.91 | 0.78 | 98.44 | 98.83 | state_region |
| Rajasthan | Southern | 767 | 47.20 | 44.98 | 0.00 | 3.26 | 27.64 | 52.41 | 89.83 | 9.65 | 79.79 | state_region |
| Arunachal Pradesh | Arunachal Pradesh | 1485 | 46.40 | 43.77 | 2.49 | 3.43 | 35.22 | 50.98 | 23.03 | 1.68 | 75.15 | state_region |
| Jharkhand | Hazaribagh Plateau | 1850 | 43.46 | 40.65 | 0.00 | 4.97 | 23.73 | 53.41 | 81.03 | 15.41 | 86.43 | state_region |
| Gujrat | Saurashtra | 1487 | 27.77 | 25.69 | 0.00 | 3.16 | 39.48 | 53.80 | 86.01 | 13.32 | 74.11 | state_region |
| Bihar | Northern | 6808 | 29.32 | 24.93 | 1.16 | 4.04 | 14.39 | 57.56 | 79.33 | 20.51 | 85.90 | state_region |
| Chhattisgarh | Southern Chhattishgarh | 441 | 28.80 | 23.36 | 0.45 | 8.16 | 23.13 | 51.47 | 96.83 | 2.27 | 92.29 | state_region |
| Bihar | Central | 4376 | 27.10 | 22.21 | 0.18 | 5.30 | 22.23 | 54.68 | 90.68 | 9.10 | 84.35 | state_region |
| Karnataka | Coastal & Ghats | 393 | 23.66 | 21.88 | 0.00 | 1.78 | 35.62 | 54.45 | 72.26 | 23.92 | 49.36 | state_region |
| Assam | Cachar Plain | 515 | 25.63 | 21.75 | 0.00 | 4.47 | 28.54 | 66.02 | 65.05 | 25.05 | 57.09 | state_region |
| Mizoram | Mizoram | 1533 | 22.44 | 19.24 | 0.00 | 4.04 | 57.21 | 53.62 | 0.46 | 0.26 | 99.54 | state_region |
| Delhi | Delhi | 1310 | 18.63 | 14.27 | 0.00 | 4.96 | 88.40 | 56.49 | 79.62 | 15.80 | 47.56 | state_region |
| Rajasthan | North-Eastern | 2287 | 18.71 | 12.86 | 0.00 | 6.52 | 29.12 | 54.48 | 86.27 | 11.89 | 78.18 | state_region |
| Orissa | Southern | 1682 | 15.46 | 12.84 | 0.00 | 3.33 | 26.10 | 53.63 | 96.79 | 0.59 | 84.84 | state_region |
| Madhya Pradesh | Northern | 1140 | 16.32 | 12.81 | 0.18 | 4.12 | 31.40 | 57.98 | 90.18 | 9.21 | 69.74 | state_region |
| Assam | Plains Western | 927 | 14.67 | 11.97 | 0.00 | 3.99 | 26.75 | 52.75 | 49.95 | 49.51 | 33.01 | state_region |
| Uttar Pradesh | Central | 2524 | 13.15 | 9.07 | 0.00 | 4.52 | 30.71 | 53.72 | 77.81 | 21.79 | 76.31 | state_region |
| Himachal Pradesh | Trans Himalayan & Southern | 922 | 13.77 | 7.70 | 0.00 | 6.29 | 20.61 | 55.42 | 87.74 | 4.88 | 43.49 | state_region |
| Uttaranchal | Uttaranchal | 1843 | 9.50 | 5.86 | 0.81 | 3.64 | 34.13 | 51.33 | 77.65 | 21.32 | 48.51 | state_region |
| Jammu & Kashmir | Outer Hills | 327 | 7.03 | 4.89 | 0.00 | 2.14 | 28.44 | 52.91 | 75.84 | 23.85 | 29.97 | state_region |

Regions with the most cost-variable missingness

``` r
analysis_table(regional_distance, "Regions with the most distance-to-school missingness", max_rows = 20)
```

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Manipur | Plains | 1493 | 10.05 | 0.00 | 4.49 | 5.63 | 42.73 | 55.26 | 74.82 | 14.53 | 83.66 | state_region |
| Arunachal Pradesh | Arunachal Pradesh | 1485 | 46.40 | 43.77 | 2.49 | 3.43 | 35.22 | 50.98 | 23.03 | 1.68 | 75.15 | state_region |
| Manipur | Hills | 865 | 4.39 | 0.00 | 1.39 | 3.01 | 3.47 | 53.64 | 8.32 | 0.23 | 89.71 | state_region |
| Meghalaya | Meghalaya | 1984 | 4.28 | 0.00 | 1.21 | 3.18 | 23.03 | 49.34 | 6.30 | 3.93 | 92.04 | state_region |
| Bihar | Northern | 6808 | 29.32 | 24.93 | 1.16 | 4.04 | 14.39 | 57.56 | 79.33 | 20.51 | 85.90 | state_region |
| Madhya Pradesh | Malwa | 1930 | 5.85 | 0.00 | 1.14 | 4.72 | 34.46 | 53.21 | 88.60 | 9.53 | 80.47 | state_region |
| Tripura | Tripura | 2647 | 4.80 | 0.00 | 1.13 | 3.74 | 18.02 | 52.81 | 84.21 | 12.05 | 70.04 | state_region |
| Uttaranchal | Uttaranchal | 1843 | 9.50 | 5.86 | 0.81 | 3.64 | 34.13 | 51.33 | 77.65 | 21.32 | 48.51 | state_region |
| Chhattisgarh | Southern Chhattishgarh | 441 | 28.80 | 23.36 | 0.45 | 8.16 | 23.13 | 51.47 | 96.83 | 2.27 | 92.29 | state_region |
| Madhya Pradesh | Central | 1053 | 2.85 | 0.00 | 0.28 | 2.56 | 41.41 | 55.27 | 81.29 | 17.76 | 73.60 | state_region |
| Bihar | Central | 4376 | 27.10 | 22.21 | 0.18 | 5.30 | 22.23 | 54.68 | 90.68 | 9.10 | 84.35 | state_region |
| Madhya Pradesh | Northern | 1140 | 16.32 | 12.81 | 0.18 | 4.12 | 31.40 | 57.98 | 90.18 | 9.21 | 69.74 | state_region |
| Uttar Pradesh | Eastern | 6952 | 6.49 | 0.00 | 0.16 | 6.36 | 22.38 | 53.74 | 80.51 | 19.30 | 83.14 | state_region |
| Andaman & Nicober | Andaman & Nicobar Islands | 453 | 3.31 | 0.00 | 0.00 | 3.31 | 42.16 | 50.77 | 67.99 | 13.25 | 22.52 | state_region |
| Andhra Pardesh | Costal Northern | 1507 | 3.92 | 0.00 | 0.00 | 3.92 | 30.92 | 50.17 | 93.36 | 3.52 | 77.84 | state_region |
| Andhra Pardesh | Costal Southern | 1241 | 2.50 | 0.00 | 0.00 | 2.50 | 31.59 | 51.25 | 92.26 | 6.61 | 61.64 | state_region |
| Andhra Pardesh | Inland North Eastern | 1122 | 2.76 | 0.00 | 0.00 | 2.76 | 22.99 | 51.52 | 92.96 | 4.63 | 86.72 | state_region |
| Andhra Pardesh | Inland North Western | 1914 | 2.93 | 0.00 | 0.00 | 2.93 | 45.51 | 49.58 | 81.92 | 16.25 | 76.12 | state_region |
| Andhra Pardesh | Inland Southern | 1425 | 3.37 | 0.00 | 0.00 | 3.37 | 31.09 | 51.37 | 79.23 | 18.95 | 65.40 | state_region |
| Assam | Cachar Plain | 515 | 25.63 | 21.75 | 0.00 | 4.47 | 28.54 | 66.02 | 65.05 | 25.05 | 57.09 | state_region |

Regions with the most distance-to-school missingness

``` r
analysis_table(regional_father, "Regions with the most father-education missingness", max_rows = 20)
```

| state_0708 | region_0708 | n | pct_any_na | miss_dmean_num_ENROLLMENT_COST | miss_DIST_FROM_NEAREST_PRIMARY_CLASS | miss_father_educ | is_urban | is_female | is_hindu | is_muslim | is_st_sc_obc | region_diagnostic_level |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Chhattisgarh | Southern Chhattishgarh | 441 | 28.80 | 23.36 | 0.45 | 8.16 | 23.13 | 51.47 | 96.83 | 2.27 | 92.29 | state_region |
| Sikkim | Sikkim | 1409 | 7.59 | 0.00 | 0.00 | 7.59 | 14.19 | 52.87 | 63.02 | 1.49 | 85.17 | state_region |
| Lakshadweep | Lakshadweep | 256 | 100.00 | 100.00 | 0.00 | 6.64 | 67.19 | 53.91 | 0.78 | 98.44 | 98.83 | state_region |
| Rajasthan | North-Eastern | 2287 | 18.71 | 12.86 | 0.00 | 6.52 | 29.12 | 54.48 | 86.27 | 11.89 | 78.18 | state_region |
| Uttar Pradesh | Eastern | 6952 | 6.49 | 0.00 | 0.16 | 6.36 | 22.38 | 53.74 | 80.51 | 19.30 | 83.14 | state_region |
| Himachal Pradesh | Trans Himalayan & Southern | 922 | 13.77 | 7.70 | 0.00 | 6.29 | 20.61 | 55.42 | 87.74 | 4.88 | 43.49 | state_region |
| Chandigarh | Chandigarh | 316 | 6.01 | 0.00 | 0.00 | 6.01 | 75.63 | 58.54 | 79.75 | 5.38 | 37.66 | state_region |
| Jammu & Kashmir | Jhelam Valley | 1210 | 5.95 | 0.00 | 0.00 | 5.95 | 34.38 | 51.74 | 0.41 | 99.42 | 20.91 | state_region |
| Uttar Pradesh | Southern Upper Ganga Plains | 3990 | 5.76 | 0.00 | 0.00 | 5.76 | 26.42 | 56.12 | 80.20 | 18.97 | 77.89 | state_region |
| Manipur | Plains | 1493 | 10.05 | 0.00 | 4.49 | 5.63 | 42.73 | 55.26 | 74.82 | 14.53 | 83.66 | state_region |
| Punjab | Southern | 1796 | 5.46 | 0.00 | 0.00 | 5.46 | 43.71 | 55.96 | 33.91 | 3.34 | 50.56 | state_region |
| Uttar Pradesh | Southern | 937 | 5.44 | 0.00 | 0.00 | 5.44 | 32.66 | 49.73 | 90.50 | 9.18 | 82.28 | state_region |
| Madhya Pradesh | South | 1211 | 5.37 | 0.00 | 0.00 | 5.37 | 30.88 | 53.26 | 91.74 | 7.02 | 91.58 | state_region |
| Bihar | Central | 4376 | 27.10 | 22.21 | 0.18 | 5.30 | 22.23 | 54.68 | 90.68 | 9.10 | 84.35 | state_region |
| Uttar Pradesh | Northern Upper Ganga Plains | 2832 | 5.30 | 0.00 | 0.00 | 5.30 | 29.98 | 53.53 | 54.98 | 44.24 | 78.88 | state_region |
| Rajasthan | South-Eastern | 725 | 5.10 | 0.00 | 0.00 | 5.10 | 32.83 | 54.76 | 91.45 | 8.41 | 79.59 | state_region |
| Jharkhand | Hazaribagh Plateau | 1850 | 43.46 | 40.65 | 0.00 | 4.97 | 23.73 | 53.41 | 81.03 | 15.41 | 86.43 | state_region |
| Delhi | Delhi | 1310 | 18.63 | 14.27 | 0.00 | 4.96 | 88.40 | 56.49 | 79.62 | 15.80 | 47.56 | state_region |
| Gujrat | Kachchh | 223 | 4.93 | 0.00 | 0.00 | 4.93 | 37.22 | 47.09 | 69.51 | 30.49 | 83.86 | state_region |
| Orissa | Northern | 1520 | 4.87 | 0.00 | 0.00 | 4.87 | 21.97 | 50.53 | 96.78 | 1.18 | 88.82 | state_region |

Regions with the most father-education missingness

``` r
analysis_table(case_study, "Current Rajasthan/Southern case-study analog")
```

| case_scope | n_rows | n_rows_with_any_missing | n_rows_with_partial_missing | n_missing_cells | n_observed_cells_in_rows_with_missing | interpretation |
|:---|---:|---:|---:|---:|---:|:---|
| Rajasthan / Southern | 767 | 763 | 763 | 3426 | 4204 | Current analog of the legacy Rajasthan/Southern View() check: rows with partial cost-variable missingness preserve the legacy concern that a child was not necessarily excluded wholesale when one cost field was missing. |

Current Rajasthan/Southern case-study analog

``` r
analysis_table(chi_square, "Current chi-square tests for ANY probit-model NA")
```

| test | status | statistic | parameter | p.value | n | method |
|:---|:---|---:|---:|---:|---:|:---|
| any_probit_model_na_by_state | estimated | 13626.17 | 34 | 0 | 127181 | Pearson’s Chi-squared test |
| any_probit_model_na_by_region | estimated | 13491.22 | 65 | 0 | 127181 | Pearson’s Chi-squared test |

Current chi-square tests for ANY probit-model NA

The current analog of the legacy correlation-matrix work writes both CSV
matrices and rendered heatmaps as target outputs. The legacy
logit-screen plots are likewise rendered from the current target output
rather than copied from the old notebook.

``` r
analysis_image("diag_ext_missingness", "missingness_correlation_all.png", "Missingness correlation heatmap for all observations")
```

![Missingness correlation heatmap for all
observations](../../outputs/diagnostics/extended/missingness/missingness_correlation_all.png)

``` r
analysis_image("diag_ext_missingness", "missingness_correlation_enrolled.png", "Missingness correlation heatmap for enrolled observations")
```

![Missingness correlation heatmap for enrolled
observations](../../outputs/diagnostics/extended/missingness/missingness_correlation_enrolled.png)

``` r
analysis_image("diag_ext_missingness", "missingness_logit_pseudo_r2.png", "Pseudo-R-squared bar chart for missingness logit screens")
```

![Pseudo-R-squared bar chart for missingness logit
screens](../../outputs/diagnostics/extended/missingness/missingness_logit_pseudo_r2.png)

``` r
analysis_table(corr_all_pairs, "Largest absolute missingness correlations: all observations", max_rows = 20)
```

| var1 | var2 | correlation | abs_correlation |
|:---|:---|:---|:---|
| SECTORRural | SECTORUrban | -1 | 1 |
| RELIGIONHindu | RELIGIONMuslim | -0.730352767628187 | 0.730352767628187 |
| RELIGIONChristian | state_0708Punjab | 0.657341197117487 | 0.657341197117487 |
| RELIGIONJain | SOCIAL_GROUPScheduled Tribe | 0.478890744033803 | 0.478890744033803 |
| RELIGIONHindu | RELIGIONJain | -0.438594959728884 | 0.438594959728884 |
| RELIGIONJain | state_0708Nagaland | 0.410788553426636 | 0.410788553426636 |
| RELIGIONJain | state_0708Meghalaya | 0.407619693413785 | 0.407619693413785 |
| RELIGIONSikh | state_0708Arunachal Pradesh | 0.403422327157635 | 0.403422327157635 |
| RELIGIONJain | state_0708Mizoram | 0.403016862876936 | 0.403016862876936 |
| SOCIAL_GROUPOther Backward Class | SOCIAL_GROUPScheduled Caste | -0.385554428806168 | 0.385554428806168 |
| SOCIAL_GROUPOther Backward Class | SOCIAL_GROUPScheduled Tribe | -0.327857495572207 | 0.327857495572207 |
| SOCIAL_GROUPScheduled Tribe | state_0708Meghalaya | 0.276285643539746 | 0.276285643539746 |
| SOCIAL_GROUPScheduled Tribe | state_0708Mizoram | 0.268661463763534 | 0.268661463763534 |
| SOCIAL_GROUPScheduled Tribe | state_0708Nagaland | 0.253500621882801 | 0.253500621882801 |
| miss_dmean_num_ENROLLMENT_COST | state_0708Bihar | 0.247015617170005 | 0.247015617170005 |
| RELIGIONChristian | RELIGIONHindu | -0.234642951092739 | 0.234642951092739 |
| RELIGIONHindu | state_0708Meghalaya | -0.200563619530547 | 0.200563619530547 |
| RELIGIONHindu | SOCIAL_GROUPScheduled Caste | 0.19562252770786 | 0.19562252770786 |
| SOCIAL_GROUPScheduled Caste | SOCIAL_GROUPScheduled Tribe | -0.193977258221556 | 0.193977258221556 |
| RELIGIONHindu | state_0708Mizoram | -0.190920293394014 | 0.190920293394014 |
| Table truncated in rendered note; full CSV has 50 rows. |  |  |  |

Largest absolute missingness correlations: all observations

``` r
analysis_table(corr_enrolled_pairs, "Largest absolute missingness correlations: enrolled observations", max_rows = 20)
```

| var1 | var2 | correlation | abs_correlation |
|:---|:---|:---|:---|
| SECTORRural | SECTORUrban | -1 | 1 |
| RELIGIONHindu | RELIGIONMuslim | -0.692373844680057 | 0.692373844680057 |
| RELIGIONChristian | state_0708Punjab | 0.643957482896051 | 0.643957482896051 |
| RELIGIONJain | SOCIAL_GROUPScheduled Tribe | 0.503881729855742 | 0.503881729855742 |
| RELIGIONHindu | RELIGIONJain | -0.480085894082433 | 0.480085894082433 |
| RELIGIONSikh | state_0708Arunachal Pradesh | 0.436236456878389 | 0.436236456878389 |
| RELIGIONJain | state_0708Nagaland | 0.418594905643235 | 0.418594905643235 |
| RELIGIONJain | state_0708Mizoram | 0.418386800302075 | 0.418386800302075 |
| RELIGIONJain | state_0708Meghalaya | 0.388727714617899 | 0.388727714617899 |
| SOCIAL_GROUPOther Backward Class | SOCIAL_GROUPScheduled Caste | -0.365222771040763 | 0.365222771040763 |
| SOCIAL_GROUPOther Backward Class | SOCIAL_GROUPScheduled Tribe | -0.319645591856161 | 0.319645591856161 |
| miss_UNIFORM | state_0708Bihar | 0.311114762730919 | 0.311114762730919 |
| SOCIAL_GROUPScheduled Tribe | state_0708Mizoram | 0.29539205542993 | 0.29539205542993 |
| SOCIAL_GROUPScheduled Tribe | state_0708Meghalaya | 0.278814711348354 | 0.278814711348354 |
| SOCIAL_GROUPScheduled Tribe | state_0708Nagaland | 0.277244351558723 | 0.277244351558723 |
| miss_TRANSPORT | AGE | -0.249900889040538 | 0.249900889040538 |
| RELIGIONHindu | SOCIAL_GROUPScheduled Tribe | -0.246513278856422 | 0.246513278856422 |
| RELIGIONChristian | RELIGIONHindu | -0.244761311354973 | 0.244761311354973 |
| miss_EXAMINATION_FEE | AGE | -0.215715449233446 | 0.215715449233446 |
| RELIGIONHindu | state_0708Mizoram | -0.213467911757855 | 0.213467911757855 |
| Table truncated in rendered note; full CSV has 50 rows. |  |  |  |

Largest absolute missingness correlations: enrolled observations

The current analog of the legacy one-logit-per-missing-variable screen
is summarized below. `n_sig` counts covariates that survive
Benjamini-Hochberg/FDR adjustment at the target’s configured threshold.

``` r
analysis_table(logit_summary, "Logit summaries for structured missingness")
```

| missing_var                     | n_sig | pseudoR2 |
|:--------------------------------|------:|---------:|
| dmean_num_ENROLLMENT_COST       |     9 |    0.310 |
| DIST_FROM_NEAREST_PRIMARY_CLASS |     3 |    0.246 |
| EXAMINATION_FEE                 |    37 |    0.164 |
| UNIFORM                         |    33 |    0.161 |
| TRANSPORT                       |    37 |    0.151 |
| OTHER_FEES_PAYMENTS             |    39 |    0.134 |
| TUTION_FEE                      |     5 |    0.056 |
| STATIONERY                      |     3 |    0.039 |
| BOOKS                           |     4 |    0.027 |
| father_educ                     |    11 |    0.023 |

Logit summaries for structured missingness

``` r
analysis_table(legacy_notes, "Legacy missingness notes retained as diagnostics")
```

| diagnostic | legacy_status |
|:---|:---|
| rajasthan_southern_case_study | commented diagnostic View() code preserved as documented note |
| chi_square_any_na_by_state | commented diagnostic test not run by default |
| chi_square_any_na_by_region | commented diagnostic test not run by default |
| parallel_missingness_logit | ported using lapply for reproducible targets execution; legacy mclapply/parLapply choice documented |

Legacy missingness notes retained as diagnostics

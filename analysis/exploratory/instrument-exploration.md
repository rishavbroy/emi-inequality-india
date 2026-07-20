# Instrument Exploration


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Preliminary test of IV strength: Dotplot of EMIE values by
`district_code`. These are the current target-backed instrument-strength
plots for the legacy exploratory check.

EMIE has three peaks: in Jammu and Kashmir; in Sikkim, Arunachal
Pradesh, Nagaland, Manipur, Mizoran, Tripura, maybe Meghalaya; and in
Andhra Pradesh, Karnataka, Goa, Lakshadweep, Kerala, Tamil Nadu,
Pondicheri, and Andaman & Nicobar! The regions which historically were
the furthest from Hindi! The legacy prose used a 0-1 scale: many
districts in the second group seem to have EMIE around 1, and the range
of EMIE outside peaks is between 0.4 and 0.1. The current target output
stores the same exposure as a 0-100 percentage scale, so those legacy
reference values correspond to about 100 and 40-10. Justification for
looking at smaller units of analysis?

Process the dataframe and compute the weighted average `ling_distance`
for each `(state, district)` group. Group by state and district to
ensure that same district names in different states are treated
separately. For each group, keep only the top three rows by `spkr_tot`.
Create the `ling_degrees` column based on the mother tongue values and
@shastry2012a’s 0-5 measure of degrees of linguistic distance. Calculate
the weighted average linguistic distance for each group.

``` r
analysis_deviation_note("The active note renders the current EMIE dotplot target and IV-panel diagnostics, while preserving the legacy interpretation of the peaks as prose rather than copied static output. EMIE scale language is minimally updated because current outputs use a 0-100 percentage scale while the legacy comments described the same concept on a 0-1 scale.")
```

**Deviation note.** The active note renders the current EMIE dotplot
target and IV-panel diagnostics, while preserving the legacy
interpretation of the peaks as prose rather than copied static output.
EMIE scale language is minimally updated because current outputs use a
0-100 percentage scale while the legacy comments described the same
concept on a 0-1 scale.

``` r
iv_match <- read_analysis_csv("diagnostics", "public", "iv_panel_match_summary.csv")
iv_state <- read_analysis_csv("diagnostics", "public", "iv_panel_state_summary.csv")
iv_rows <- read_analysis_csv("diagnostics", "public", "iv_summary_keyed_rows.csv")
iv_dotplot <- analysis_target_csv("diag_ext_instrument_exploration", "instrument_strength_dotplot_data.csv")
iv_notes <- analysis_target_csv("diag_ext_instrument_exploration", "instrument_exploration_legacy_notes.csv")
```

The current analog of the legacy
`ggplot(..., aes(x = district_code_0708, y = EMIE, color = district_prefix)) + geom_point()`
check uses 482 active district-panel rows.

``` r
instrument_dotplot_rows <- iv_dotplot[, intersect(c("district_order", "district_code", "state", "district", "EMIE", "wavg_ling_degrees", "state_prefix"), names(iv_dotplot)), drop = FALSE]
emie_scale_summary <- data.frame(
  current_scale = "0-100 percentage scale",
  min_EMIE = min(iv_dotplot$EMIE, na.rm = TRUE),
  median_EMIE = stats::median(iv_dotplot$EMIE, na.rm = TRUE),
  max_EMIE = max(iv_dotplot$EMIE, na.rm = TRUE),
  legacy_scale_note = "legacy prose values 1 and 0.4-0.1 correspond to 100 and 40-10 on the current scale"
)
analysis_table(emie_scale_summary, "Current EMIE scale check")
```

| current_scale | min_EMIE | median_EMIE | max_EMIE | legacy_scale_note |
|:---|---:|---:|---:|:---|
| 0-100 percentage scale | 0 | 8.544 | 100 | legacy prose values 1 and 0.4-0.1 correspond to 100 and 40-10 on the current scale |

Current EMIE scale check

``` r
analysis_table(instrument_dotplot_rows, "Current EMIE-by-district dotplot data", max_rows = 30)
```

| district_order | district_code | state | district | EMIE | wavg_ling_degrees | state_prefix |
|:---|:---|:---|:---|:---|:---|:---|
| 1 | 1113 | Jammu and Kashmir | Jammu | 55.9373478876879 | 4.10472406351803 | 1 |
| 2 | 1114 | Jammu and Kashmir | Kathua | 44.1686433398463 | 4.47509586442914 | 1 |
| 3 | 1209 | Jammu and Kashmir | Doda | 100 | 2.80978436598451 | 1 |
| 4 | 1210 | Jammu and Kashmir | Udhampur | 100 | 3.89551856807832 | 1 |
| 5 | 1301 | Jammu and Kashmir | Kupwara | 35.3856881455635 | 3.03277502974141 | 1 |
| 6 | 1303 | Jammu and Kashmir | Srinagar | 85.077782419044 | 3.68960681829044 | 1 |
| 7 | 1305 | Jammu and Kashmir | Pulwama | 90.3661939188381 | 3.77674113819634 | 1 |
| 8 | 1306 | Jammu and Kashmir | Anantnag | 85.3633724175985 | 3.53093067678782 | 1 |
| 9 | 2102 | Himachal Pradesh | Kangra | 23.0964013292776 | 0.0511254204156725 | 2 |
| 10 | 2104 | Himachal Pradesh | Kullu | 2.16035948115958 | 1.93095942551585 | 2 |
| 11 | 2105 | Himachal Pradesh | Mandi | 13.1015338642775 | 0.0223237688187754 | 2 |
| 12 | 2106 | Himachal Pradesh | Hamirpur | 16.4639133923992 | 0.0486081824586379 | 2 |
| 13 | 2107 | Himachal Pradesh | Una | 14.6258166762162 | 0.226245237822633 | 2 |
| 14 | 2201 | Himachal Pradesh | Chamba | 11.6242446239474 | 0.11429443132082 | 2 |
| 15 | 2208 | Himachal Pradesh | Bilaspur | 16.8181705232738 | 0.559565764910242 | 2 |
| 16 | 2209 | Himachal Pradesh | Solan | 17.8837636459365 | 0.187361916175475 | 2 |
| 17 | 2210 | Himachal Pradesh | Sirmaur | 17.7238270927478 | 0.0939557639834032 | 2 |
| 18 | 2211 | Himachal Pradesh | Shimla | 41.7306051009988 | 0.163964926837157 | 2 |
| 19 | 2212 | Himachal Pradesh | Kinnaur | 5.15399235250357 | 4.43059659051503 | 2 |
| 20 | 3101 | Punjab | Gurdaspur | 34.4063148539354 | 0.978981869361082 | 3 |
| 21 | 3102 | Punjab | Amritsar | 32.9079601779732 | 0.95674459915501 | 3 |
| 22 | 3103 | Punjab | Kapurthala | 43.9577396007221 | 0.936608167186548 | 3 |
| 23 | 3104 | Punjab | Jalandhar | 39.7958708417419 | 0.918986609063724 | 3 |
| 24 | 3105 | Punjab | Hoshiarpur | 14.6471262784023 | 0.952338796474347 | 3 |
| 25 | 3106 | Punjab | Nawanshahr | 39.073688061125 | 0.966297335658519 | 3 |
| 26 | 3107 | Punjab | Rupnagar | 37.1008378309431 | 0.903243078071297 | 3 |
| 27 | 3208 | Punjab | Fatehgarh Sahib | 26.1747306686159 | 0.935136464132648 | 3 |
| 28 | 3209 | Punjab | Ludhiana | 28.2453355709974 | 0.824173864209363 | 3 |
| 29 | 3210 | Punjab | Moga | 13.648210472959 | 0.974371536656354 | 3 |
| 30 | 3211 | Punjab | Firozpur | 26.704471925114 | 0.883867895594906 | 3 |
| Table truncated in rendered note; full CSV has 482 rows. |  |  |  |  |  |  |

Current EMIE-by-district dotplot data

``` r
analysis_image("diag_ext_instrument_exploration", "emie_by_district_dotplot.png", "Current EMIE-by-district exploratory dotplot")
```

![Current EMIE-by-district exploratory
dotplot](../../outputs/diagnostics/extended/instrument_exploration/emie_by_district_dotplot.png)

``` r
analysis_table(iv_match, "Current IV-panel match summary")
```

| .matched_2001 | .matched_2007 | .matched_2017 | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|
| TRUE | TRUE | TRUE | 482 | 18.973 | 2.013 | 1700682 | 850.209 | 60.264 |

Current IV-panel match summary

``` r
analysis_table(iv_state, "Current IV-panel state summary", max_rows = 30)
```

| state | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|:---|:---|:---|:---|
| Andhra Pradesh | 11 | 23.6439637582659 | 4.73743652602561 | 3565363.77909091 | 858.689956070715 | 47.5979753626463 |
| Arunachal Pradesh | 13 | 95.0157164422056 | 4.6472947467219 | 76279.7523076923 | 906.331252948571 | 61.0798056117889 |
| Assam | 20 | 4.86270886719062 | 3.07232885430867 | 1147553.4215 | 801.401484912825 | 57.1510363005785 |
| Bihar | 34 | 3.50846184019296 | 0.737018650699089 | 1981686.42544118 | 602.665280317918 | 84.8703028532982 |
| Chandigarh | 1 | 58.3318564651552 | 0.28986403537111 | 837516.24 | 2923.13632837881 | 36.7830442420546 |
| Chhattisgarh | 13 | 3.36951744303005 | 0.591175404554623 | 1626488.72307692 | 588.203351810108 | 57.6972398610473 |
| Goa | 2 | 62.2874081579816 | 2 | 698016.255 | 1427.58012410047 | 35.7209708999027 |
| Gujarat | 16 | 4.43578715526743 | 1.27061155203841 | 1824361.856875 | 994.490052799496 | 49.8057766561105 |
| Haryana | 19 | 20.170112602026 | 0.126072481752038 | 1142009.75631579 | 1056.35388556882 | 53.1686777255239 |
| Himachal Pradesh | 11 | 16.3984207347944 | 0.711727402615791 | 564285.269545455 | 1133.8732701979 | 54.6766535198146 |
| Jammu and Kashmir | 8 | 74.5373785160723 | 3.66439706562825 | 789480.136875 | 959.896462458858 | 51.214728623363 |
| Jharkhand | 14 | 5.8563339754116 | 1.02388506566692 | 1454029.05785714 | 671.740893305257 | 71.0616880045961 |
| Karnataka | 16 | 14.0909379283582 | 4.3457335311984 | 1430983.1028125 | 831.971046438564 | 50.7501404266105 |
| Kerala | 14 | 43.4497857986853 | 4.99840467097412 | 2129849.7675 | 1206.28675381536 | 48.0392820929302 |
| Lakshadweep | 1 | 32.4512530246271 | 5 | 57165.375 | 1535.93748293089 | 47.3127675768181 |
| Madhya Pradesh | 42 | 6.24753868906339 | 0.573042893579146 | 1314874.12678571 | 653.244273348503 | 63.8122407105126 |
| Maharashtra | 29 | 11.4830410179425 | 1.9522032273927 | 2841016.72137931 | 889.465463343876 | 52.3245091519967 |
| Manipur | 8 | 63.66891346626 | 4.98569769201453 | 227265.8075 | 870.903630397952 | 49.6512299191417 |
| Meghalaya | 6 | 63.742626422972 | 4.85968082738316 | 344808.693333333 | 941.600385292216 | 64.6291546365286 |
| Mizoram | 7 | 49.8826321870426 | 4.70967863891981 | 81444.7335714286 | 1150.98277228436 | 62.853899929227 |
| Nagaland | 8 | 99.7003154183167 | 4.74309513846195 | 118572.34625 | 1235.27747750873 | 41.5966283338895 |
| Odisha | 23 | 7.07565238642496 | 3.11402429671511 | 1244504.71978261 | 619.200310362183 | 52.6753535020323 |
| Puducherry | 4 | 53.7704148434607 | 4.97028095100957 | 207733.7475 | 1333.56765439781 | 44.4357277807352 |
| Punjab | 15 | 30.8110849018846 | 0.932472555964024 | 1503356.93366667 | 1237.25188924923 | 49.5760252064197 |
| Rajasthan | 27 | 5.38824408462306 | 0.369378246090727 | 1861619.5487037 | 818.178496919533 | 68.8613716717374 |
| Tamil Nadu | 27 | 22.6959611165765 | 4.98566486057399 | 2169276.71055556 | 976.785681331315 | 45.8385046057772 |
| Telangana | 8 | 28.758822497895 | 4.01688425665652 | 2923324.4025 | 991.440731095913 | 46.7537202937939 |
| Tripura | 4 | 3.2389135004614 | 3.58325060830235 | 883489.59875 | 789.774624079622 | 50.5183950560216 |
| Uttar Pradesh | 62 | 5.68861643327111 | 0.0179480043464079 | 2518112.50258065 | 700.163262177421 | 76.9319368762913 |
| Uttarakhand | 11 | 14.6636310855265 | 0.0624887381832764 | 589312.001363636 | 924.281162141684 | 66.6122123387273 |
| Table truncated in rendered note; full CSV has 31 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | var | label | N | Min | 1Q | Med | 3Q | Max | Mean | SD | desc |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| From 2001 | wavg_ling_degrees | wavg_ling_degrees | Ling. Distance | 482 | 0.00 | 0.04 | 1.44 | 4.02 | 5.00 | 2.01 | 1.98 | Average linguistic distance of mother tongue from Hindi |
| From 2007-08 | EMIE | EMIE | EMIE | 482 | 0.00 | 2.02 | 8.54 | 22.89 | 100.00 | 18.97 | 25.27 | EMI exposure |
| From 2007-08 | npeople_0708 | npeople_0708 | Population | 482 | 12,285 | 823,676 | 1,396,516 | 2,317,118 | 9,922,640 | 1,700,682 | 1,307,716 | Estimated via NSS sample weights |
| From 2007-08 | consumption_0708 | consumption_0708 | Consumption | 482 | 330.09 | 626.88 | 768.60 | 999.13 | 2923.14 | 850.21 | 319.75 | Average household monthly consumption expenditures (Rs.) |
| From 2007-08 | gini_cons_0708 | gini_cons_0708 | Gini of Consumption | 482 | 0.06 | 0.22 | 0.26 | 0.30 | 0.56 | 0.26 | 0.07 | Gini coefficient of consumption |
| From 2007-08 | pct_urban | pct_urban | Pct. Urban | 482 | 0.00 | 9.25 | 16.59 | 30.31 | 100.00 | 22.00 | 18.21 | Percentage of people in an urban area |
| From 2007-08 | avg_hh_size | avg_hh_size | Avg. HH Size | 482 | 3.15 | 4.27 | 4.70 | 5.12 | 6.44 | 4.68 | 0.65 | Average household size |
| From 2007-08 | dependency_ratio | dependency_ratio | Dependency Ratio × 100 | 482 | 23.69 | 48.43 | 57.43 | 71.22 | 110.65 | 60.26 | 15.45 | Ratio of dependents (0-14, 65+) to labor force (15-64), × 100 |
| From 2007-08 | pct_fem_head | pct_fem_head | Pct. Female Head | 482 | 50.44 | 85.57 | 90.22 | 93.95 | 100.00 | 88.84 | 7.09 | Percentage of households with a female head |
| From 2007-08 | pct_hindu | pct_hindu | Pct. Hindu | 482 | 0.00 | 72.83 | 88.62 | 95.14 | 100.00 | 77.60 | 27.27 | Percentage of Hindus |
| From 2007-08 | pct_muslim | pct_muslim | Pct. Muslim | 482 | 0.00 | 1.67 | 5.70 | 12.20 | 100.00 | 10.26 | 15.13 | Percentage of Muslims |
| From 2007-08 | pct_other_religion | pct_other_religion | Pct. Other | 482 | 0.00 | 0.00 | 1.21 | 7.29 | 100.00 | 12.14 | 26.20 | Percentage not Hindu/Muslim |
| From 2007-08 | pct_st | pct_st | Pct. ST | 482 | 0.00 | 0.00 | 2.88 | 19.26 | 100.00 | 17.03 | 27.84 | Scheduled Tribe |
| From 2007-08 | pct_sc | pct_sc | Pct. SC | 482 | 0.00 | 9.97 | 17.67 | 25.78 | 46.72 | 17.77 | 10.88 | Scheduled Caste |
| From 2007-08 | pct_obc | pct_obc | Pct. OBC | 482 | 0.00 | 18.24 | 41.83 | 57.45 | 96.60 | 38.74 | 23.76 | Other Backward Class |
| From 2007-08 | pct_small_land | pct_small_land | Pct. Small Land-Owner | 482 | 5.03 | 33.21 | 47.07 | 60.52 | 94.20 | 47.22 | 18.29 | Owns 0.005–0.40 hectares |
| From 2007-08 | pct_medium_land | pct_medium_land | Pct. Med. Land-Owner | 482 | 0.00 | 17.39 | 28.41 | 40.77 | 90.73 | 30.30 | 16.86 | Owns 0.41–3.00 hectares |
| From 2007-08 | pct_large_land | pct_large_land | Pct. Large Land-Owner | 482 | 0.00 | 0.00 | 1.30 | 4.73 | 34.94 | 3.35 | 4.96 | Owns $\geq$ 3.01 hectares |
| From 2007-08 | pct_head_illiterate | pct_head_illiterate | Pct. Head Educ., Illiterate | 482 | 0.00 | 24.07 | 34.35 | 46.16 | 78.68 | 34.90 | 15.88 | Percentage of household heads with educ. level: illiterate |
| From 2007-08 | pct_head_lit_to_primary | pct_head_lit_to_primary | Pct. Head Educ., Lit.-Primary | 482 | 3.28 | 19.67 | 26.46 | 33.77 | 77.63 | 27.76 | 11.05 | Percentage of heads with educ. level: literate-primary |
| From 2007-08 | pct_head_secondary_plus | pct_head_secondary_plus | Pct. Head Educ., Secondary+ | 482 | 0.58 | 26.90 | 35.20 | 46.43 | 79.43 | 37.34 | 14.40 | Percentage of heads with educ. level: above secondary |
| From 2007-08 | pct_pucca | pct_pucca | Pct. Pucca | 482 | 0.00 | 27.51 | 55.63 | 80.92 | 100.00 | 53.95 | 29.42 | Percentage in pucca (permanent) homes |
| From 2017-18 | npeople_1718 | npeople_1718 | Population | 482 | 30,094 | 807,081 | 1,481,289 | 2,342,633 | 12,274,837 | 1,788,321 | 1,476,278 | Estimated via NSS sample weights |
| From 2017-18 | consumption_1718 | consumption_1718 | Consumption | 482 | 850.53 | 1544.12 | 2038.39 | 2618.70 | 6764.46 | 2214.82 | 902.80 | Average household monthly consumption expenditures (Rs.) |
| From 2017-18 | gini_cons_1718 | gini_cons_1718 | Gini of Consumption | 482 | 0.11 | 0.20 | 0.24 | 0.29 | 0.55 | 0.25 | 0.07 | Gini coefficient of consumption |
| From 2007-08 to 2017-18 | consumption_pct_change | consumption_pct_change | Percent change in consumption | 482 | 12.25 | 124.22 | 157.62 | 192.18 | 446.24 | 164.61 | 61.56 | Percent change in consumption |
| From 2007-08 to 2017-18 | gini_change | gini_change | Change in Gini of consumption | 482 | -0.30 | -0.07 | -0.02 | 0.03 | 0.29 | -0.02 | 0.08 | Change in the Gini coefficient of consumption |

Current keyed IV summary rows

``` r
analysis_table(iv_notes, "Legacy instrument-exploration notes retained as target output")
```

| diagnostic | legacy_note | current_status |
|:---|:---|:---|
| emie_dotplot | Dotplot of EMIE values by district_code. | rendered from active district_panel as a target-backed figure |
| legacy_peak_comment | EMIE had visible peaks in Jammu and Kashmir; in several Northeast states; and in southern/coastal districts historically furthest from Hindi. | use current dotplot/table rather than the legacy hard-coded visual impression |
| smaller_units_question | Many districts outside peaks had low EMIE values; legacy comments asked whether smaller units of analysis would be useful. | retained as exploratory rationale, not a final-paper claim |
| district_count_check | Legacy code checked that the number of districts did not change while constructing weighted linguistic distance. | final panel match summaries are rendered in this analysis note |

Legacy instrument-exploration notes retained as target output

# Instrument Exploration


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Legacy prose retained with current results

Preliminary test of IV strength: Dotplot of EMIE values by
`district_code`.

EMIE has three peaks: in Jammu and Kashmir; in Sikkim, Arunachal
Pradesh, Nagaland, Manipur, Mizoran, Tripura, maybe Meghalaya; and in
Andhra Pradesh, Karnataka, Goa, Lakshadweep, Kerala, Tamil Nadu,
Pondicheri, and Andaman & Nicobar! The regions which historically were
the furthest from Hindi! Many districts in the second group seem to have
EMIE around 1, and the range of EMIE outside peaks is between 0.4 and
0.1. Justification for looking at smaller units of analysis?

Process the dataframe and compute the weighted average `ling_distance`
for each `(state, district)` group. Group by state and district to
ensure that same district names in different states are treated
separately. For each group, keep only the top three rows by `spkr_tot`.
Create the `ling_degrees` column based on the mother tongue values and
@shastry2012a’s 0-5 measure of degrees of linguistic distance. Calculate
the weighted average linguistic distance for each group.

``` r
analysis_deviation_note("The active note renders the current EMIE dotplot target and IV-panel diagnostics, while preserving the legacy interpretation of the peaks as prose rather than copied static output.")
```

**Deviation note.** The active note renders the current EMIE dotplot
target and IV-panel diagnostics, while preserving the legacy
interpretation of the peaks as prose rather than copied static output.

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
| TRUE | TRUE | TRUE | 482 | 18.973 | 2.013 | 1700680 | 850.209 | 60.264 |

Current IV-panel match summary

``` r
analysis_table(iv_state, "Current IV-panel state summary", max_rows = 30)
```

| state | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|:---|:---|:---|:---|
| Andhra Pradesh | 11 | 23.644 | 4.73744 | 3565360 | 858.69 | 47.598 |
| Arunachal Pradesh | 13 | 95.0157 | 4.64729 | 76279.8 | 906.331 | 61.0798 |
| Assam | 20 | 4.86271 | 3.07233 | 1147550 | 801.401 | 57.151 |
| Bihar | 34 | 3.50846 | 0.737019 | 1981690 | 602.665 | 84.8703 |
| Chandigarh | 1 | 58.3319 | 0.289864 | 837516 | 2923.14 | 36.783 |
| Chhattisgarh | 13 | 3.36952 | 0.591175 | 1626490 | 588.203 | 57.6972 |
| Goa | 2 | 62.2874 | 2 | 698016 | 1427.58 | 35.721 |
| Gujarat | 16 | 4.43579 | 1.27061 | 1824360 | 994.49 | 49.8058 |
| Haryana | 19 | 20.1701 | 0.126072 | 1142010 | 1056.35 | 53.1687 |
| Himachal Pradesh | 11 | 16.3984 | 0.711727 | 564285 | 1133.87 | 54.6767 |
| Jammu and Kashmir | 8 | 74.5374 | 3.6644 | 789480 | 959.896 | 51.2147 |
| Jharkhand | 14 | 5.85633 | 1.02389 | 1454030 | 671.741 | 71.0617 |
| Karnataka | 16 | 14.0909 | 4.34573 | 1430980 | 831.971 | 50.7501 |
| Kerala | 14 | 43.4498 | 4.9984 | 2129850 | 1206.29 | 48.0393 |
| Lakshadweep | 1 | 32.4513 | 5 | 57165.4 | 1535.94 | 47.3128 |
| Madhya Pradesh | 42 | 6.24754 | 0.573043 | 1314870 | 653.244 | 63.8122 |
| Maharashtra | 29 | 11.483 | 1.9522 | 2841020 | 889.465 | 52.3245 |
| Manipur | 8 | 63.6689 | 4.9857 | 227266 | 870.904 | 49.6512 |
| Meghalaya | 6 | 63.7426 | 4.85968 | 344809 | 941.6 | 64.6292 |
| Mizoram | 7 | 49.8826 | 4.70968 | 81444.7 | 1150.98 | 62.8539 |
| Nagaland | 8 | 99.7003 | 4.7431 | 118572 | 1235.28 | 41.5966 |
| Odisha | 23 | 7.07565 | 3.11402 | 1244500 | 619.2 | 52.6754 |
| Puducherry | 4 | 53.7704 | 4.97028 | 207734 | 1333.57 | 44.4357 |
| Punjab | 15 | 30.8111 | 0.932473 | 1503360 | 1237.25 | 49.576 |
| Rajasthan | 27 | 5.38824 | 0.369378 | 1861620 | 818.178 | 68.8614 |
| Tamil Nadu | 27 | 22.696 | 4.98566 | 2169280 | 976.786 | 45.8385 |
| Telangana | 8 | 28.7588 | 4.01688 | 2923320 | 991.441 | 46.7537 |
| Tripura | 4 | 3.23891 | 3.58325 | 883490 | 789.775 | 50.5184 |
| Uttar Pradesh | 62 | 5.68862 | 0.017948 | 2518110 | 700.163 | 76.9319 |
| Uttarakhand | 11 | 14.6636 | 0.0624887 | 589312 | 924.281 | 66.6122 |
| Table truncated in rendered note; full CSV has 31 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | Variable | Description | Min | 1Q | Med | 3Q | Max | Mean | SD | N |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|
| 2001 | wavg_ling_degrees | Ling. Distance | Average linguistic distance of mother tongue from Hindi | 0.00 | 0.04 | 1.44 | 4.02 | 5.00 | 2.01 | 1.98 | 482 |
| 2007-08 | EMIE | EMIE | EMI exposure | 0.00 | 2.02 | 8.54 | 22.89 | 100.00 | 18.97 | 25.27 | 482 |
| 2007-08 | npeople_0708 | Population | Estimated via NSS sample weights | 12,285 | 823,676 | 1,396,516 | 2,317,118 | 9,922,640 | 1,700,682 | 1,307,716 | 482 |
| 2007-08 | consumption_0708 | Consumption | Average household monthly consumption expenditures (Rs.) | 330.09 | 626.88 | 768.60 | 999.13 | 2923.14 | 850.21 | 319.75 | 482 |
| 2007-08 | gini_cons_0708 | Gini of Consumption | Gini coefficient of consumption | 0.06 | 0.22 | 0.26 | 0.30 | 0.56 | 0.26 | 0.07 | 482 |
| 2007-08 | Pct. Urban | Pct. Urban | Percentage of people in an urban area | 0.00 | 8.60 | 15.69 | 28.13 | 100.00 | 21.09 | 17.91 | 482 |
| 2007-08 | Avg. HH Size | Avg. HH Size | Average household size | 3.85 | 5.10 | 5.60 | 6.23 | 8.94 | 5.66 | 0.85 | 482 |
| 2007-08 | dependency_ratio | Dependency Ratio × 100 | Ratio of dependents (0-14, 65+) to labor force (15-64), × 100 | 23.69 | 48.43 | 57.43 | 71.22 | 110.65 | 60.26 | 15.45 | 482 |
| 2007-08 | pct_fem_head | Pct. Female Head | Percentage of households with a female head | 9.81 | 17.56 | 19.04 | 20.89 | 28.98 | 19.29 | 2.64 | 482 |
| 2007-08 | Pct. Hindu | Pct. Hindu | Percentage of Hindus | 0.00 | 71.62 | 87.94 | 95.10 | 100.00 | 76.78 | 27.73 | 482 |
| 2007-08 | Pct. Muslim | Pct. Muslim | Percentage of Muslims | 0.00 | 1.55 | 6.14 | 13.64 | 100.00 | 11.15 | 16.00 | 482 |
| 2007-08 | Pct. Other | Pct. Other | Percentage not Hindu/Muslim | 0.00 | 0.00 | 1.02 | 6.65 | 100.00 | 12.07 | 26.56 | 482 |
| 2007-08 | Pct. ST | Pct. ST | Scheduled Tribe | 0.00 | 0.00 | 2.93 | 18.06 | 100.00 | 16.97 | 28.12 | 482 |
| 2007-08 | Pct. SC | Pct. SC | Scheduled Caste | 0.00 | 9.45 | 17.68 | 25.55 | 46.42 | 17.61 | 10.86 | 482 |
| 2007-08 | Pct. OBC | Pct. OBC | Other Backward Class | 0.00 | 18.15 | 43.12 | 59.18 | 96.69 | 39.41 | 24.28 | 482 |
| 2007-08 | Pct. Small Land-Owner | Pct. Small Land-Owner | Owns 0.005–0.40 hectares | 4.89 | 32.36 | 45.80 | 59.45 | 95.61 | 46.49 | 18.52 | 482 |
| 2007-08 | Pct. Med. Land-Owner | Pct. Med. Land-Owner | Owns 0.41–3.00 hectares | 0.00 | 19.92 | 31.71 | 44.77 | 94.64 | 33.25 | 17.70 | 482 |
| 2007-08 | Pct. Large Land-Owner | Pct. Large Land-Owner | Owns $\geq$ 3.01 hectares | 0.00 | 0.00 | 1.67 | 6.11 | 43.73 | 4.20 | 6.07 | 482 |
| 2007-08 | Pct. Head Educ., Illiterate | Pct. Head Educ., Illiterate | Percentage of household heads with educ. level: illiterate | 0.00 | 23.95 | 34.35 | 46.16 | 78.68 | 34.88 | 15.87 | 482 |
| 2007-08 | Pct. Head Educ., Lit.-Primary | Pct. Head Educ., Lit.-Primary | Percentage of heads with educ. level: literate-primary | 3.28 | 19.67 | 26.46 | 33.77 | 77.63 | 27.74 | 11.02 | 482 |
| 2007-08 | Pct. Head Educ., Secondary+ | Pct. Head Educ., Secondary+ | Percentage of heads with educ. level: above secondary | 0.58 | 26.90 | 35.20 | 46.43 | 79.43 | 37.32 | 14.40 | 482 |
| 2007-08 | Pct. Pucca | Pct. Pucca | Percentage in pucca (permanent) homes | 0.00 | 27.51 | 55.63 | 80.92 | 100.00 | 53.95 | 29.42 | 482 |
| 2017-18 | npeople_1718 | Population | Estimated via NSS sample weights | 30,094 | 807,081 | 1,481,289 | 2,342,633 | 12,274,837 | 1,788,321 | 1,476,278 | 482 |
| 2017-18 | consumption_1718 | Consumption | Average household monthly consumption expenditures (Rs.) | 850.53 | 1544.12 | 2038.39 | 2618.70 | 6764.46 | 2214.82 | 902.80 | 482 |
| 2017-18 | gini_cons_1718 | Gini of Consumption | Gini coefficient of consumption | 0.11 | 0.20 | 0.24 | 0.29 | 0.55 | 0.25 | 0.07 | 482 |
| 2007-08 to 2017-18 | consumption_pct_change | Percent change in consumption | Percent change in consumption | 12.25 | 124.22 | 157.62 | 192.18 | 446.24 | 164.61 | 61.56 | 482 |
| 2007-08 to 2017-18 | Change in Gini of consumption | Change in Gini of consumption | Change in the Gini coefficient of consumption | -0.30 | -0.07 | -0.02 | 0.03 | 0.29 | -0.02 | 0.08 | 482 |

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

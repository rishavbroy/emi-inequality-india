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
check uses 573 active district-panel rows.

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
| 0-100 percentage scale | 0 | 8.355 | 100 | legacy prose values 1 and 0.4-0.1 correspond to 100 and 40-10 on the current scale |

Current EMIE scale check

``` r
analysis_table(instrument_dotplot_rows, "Current EMIE-by-district dotplot data", max_rows = 30)
```

| district_order | district_code | state | district | EMIE | wavg_ling_degrees | state_prefix |
|:---|:---|:---|:---|:---|:---|:---|
| 1 | 1113 | Jammu & Kashmir | Jammu | 55.9373478876879 | 4.10472406351803 | 1 |
| 2 | 1114 | Jammu & Kashmir | Kathus | 44.1686433398463 | 4.47509586442914 | 1 |
| 3 | 1209 | Jammu & Kashmir | Doda | 100 | 2.80978436598451 | 1 |
| 4 | 1210 | Jammu & Kashmir | Udhampur | 100 | 3.89551856807832 | 1 |
| 5 | 1212 | Jammu & Kashmir | Rajauri | 19.1220027592448 | 0.480199300877297 | 1 |
| 6 | 1301 | Jammu & Kashmir | Kupwara | 35.3856881455635 | 3.03277502974141 | 1 |
| 7 | 1302 | Jammu & Kashmir | Baramula | 50.1462484103989 | 3.54776816308471 | 1 |
| 8 | 1303 | Jammu & Kashmir | Srinagar | 85.077782419044 | 3.68960681829044 | 1 |
| 9 | 1304 | Jammu & Kashmir | Badgam | 63.7323207233014 | 3.84258254745327 | 1 |
| 10 | 1305 | Jammu & Kashmir | Pulwama | 90.3661939188381 | 3.77674113819634 | 1 |
| 11 | 1306 | Jammu & Kashmir | Anantnag | 85.3633724175985 | 3.53093067678782 | 1 |
| 12 | 2102 | Himachal Pradesh | Kangra | 23.0964013292776 | 0.0511254204156725 | 2 |
| 13 | 2104 | Himachal Pradesh | Kullu | 2.16035948115958 | 1.93095942551585 | 2 |
| 14 | 2105 | Himachal Pradesh | Mandi | 13.1015338642775 | 0.0223237688187754 | 2 |
| 15 | 2106 | Himachal Pradesh | Hamirpur | 16.4639133923992 | 0.0486081824586379 | 2 |
| 16 | 2107 | Himachal Pradesh | Una | 14.6258166762162 | 0.226245237822633 | 2 |
| 17 | 2201 | Himachal Pradesh | Chamba | 11.6242446239474 | 0.11429443132082 | 2 |
| 18 | 2203 | Himachal Pradesh | Lahul & Spiti | 6.31027748728387 | 4.18331171638565 | 2 |
| 19 | 2208 | Himachal Pradesh | Bilaspur | 16.8181705232738 | 0.559565764910242 | 2 |
| 20 | 2209 | Himachal Pradesh | Solan | 17.8837636459365 | 0.187361916175475 | 2 |
| 21 | 2210 | Himachal Pradesh | Sirmapur | 17.7238270927478 | 0.0939557639834032 | 2 |
| 22 | 2211 | Himachal Pradesh | Shimla | 41.7306051009988 | 0.163964926837157 | 2 |
| 23 | 2212 | Himachal Pradesh | Kinnaur | 5.15399235250357 | 4.43059659051503 | 2 |
| 24 | 3101 | Punjab | Gurdaspur | 34.4063148539354 | 0.978981869361082 | 3 |
| 25 | 3102 | Punjab | Amritsar | 32.9079601779732 | 0.95674459915501 | 3 |
| 26 | 3103 | Punjab | Kapurthala | 43.9577396007221 | 0.936608167186548 | 3 |
| 27 | 3104 | Punjab | Jalandhar | 39.7958708417419 | 0.918986609063724 | 3 |
| 28 | 3106 | Punjab | Nawanshahr | 39.073688061125 | 0.966297335658519 | 3 |
| 29 | 3107 | Punjab | Rupnagar | 37.1008378309431 | 0.903243078071297 | 3 |
| 30 | 3208 | Punjab | Fatehgarh Sahib | 26.1747306686159 | 0.935136464132648 | 3 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |  |

Current EMIE-by-district dotplot data

``` r
analysis_image("diag_ext_instrument_exploration", "emie_by_district_dotplot.png", "Current EMIE-by-district exploratory dotplot")
```

![Current EMIE-by-district exploratory
dotplot](../../outputs/diagnostics/extended/instrument_exploration/emie_by_district_dotplot.png)

``` r
analysis_table(iv_match, "Current IV-panel match summary")
```

| n_rows |
|-------:|
|    573 |

Current IV-panel match summary

``` r
analysis_table(iv_state, "Current IV-panel state summary", max_rows = 30)
```

| state | n_rows | mean_EMIE | mean_wavg_ling_degrees | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|:---|:---|:---|:---|
| Andaman & Nicobar Islands | 2 | 28.9337363688688 | 3.555900590902 | 114699.0625 | 1876.7627890843 | 39.9965239648858 |
| Andhra Pradesh | 13 | 24.4802289280157 | 4.68834038721984 | 3406138.48653846 | 865.403384123275 | 46.8784465840865 |
| Arunachal Pradesh | 13 | 95.0157164422056 | 4.6472947467219 | 76279.7523076923 | 906.331252948571 | 61.0798056117889 |
| Assam | 22 | 5.3275500840982 | 3.13755149089939 | 1070667.90886364 | 803.234906099672 | 58.0450186320867 |
| Bihar | 37 | 3.38601239299322 | 0.6784276793522 | 2052743.65324324 | 599.798542271148 | 84.7022490469553 |
| Chandigarh | 1 | 58.3318564651552 | 0.28986403537111 | 837516.24 | 2923.13632837881 | 36.7830442420546 |
| Chhattisgarh | 16 | 3.2273017372452 | 0.825123850855922 | 1455306.785 | 561.201507459237 | 59.4295625174172 |
| Dadra & Nagar Haveli | 1 | 5.45838437776184 | 2.85327585515039 | 204832.69 | 1005.32744354631 | 55.2790526851351 |
| Daman & Diu | 2 | 22.0521469070064 | 0.900756561674745 | 69690.93 | 1505.79610737489 | 51.6253036614033 |
| Delhi | 7 | 32.8114108555629 | 0.0800754690740823 | 1802076.03428571 | 1377.86559751405 | 51.9338015790853 |
| Goa | 2 | 62.2874081579816 | 2 | 698016.255 | 1427.58012410047 | 35.7209708999027 |
| Gujarat | 25 | 4.42406172844767 | 1.39840030769189 | 2038854.0708 | 950.628629395387 | 53.6880918495208 |
| Haryana | 19 | 20.170112602026 | 0.126072481752038 | 1142009.75631579 | 1056.35388556882 | 53.1686777255239 |
| Himachal Pradesh | 12 | 15.5577421308352 | 1.00102609542995 | 519885.804583333 | 1116.2584647989 | 53.7694561219673 |
| Jammu & Kashmir | 11 | 66.2999636383203 | 3.38052059422194 | 735654.508181818 | 961.24108632186 | 52.1863851221851 |
| Jharkhand | 18 | 6.48227701539392 | 1.35216904499632 | 1395211.16555556 | 691.596072974349 | 67.2780276872427 |
| Karnataka | 27 | 16.0376594835309 | 4.38737818600313 | 1844944.73574074 | 859.871286034099 | 49.7504147543725 |
| Kerala | 14 | 43.4497857986853 | 4.99840467097412 | 2129849.7675 | 1206.28675381536 | 48.0392820929302 |
| Lakshadweep | 1 | 32.4512530246271 | 5 | 57165.375 | 1535.93748293089 | 47.3127675768181 |
| Madhya Pradesh | 44 | 6.54482431274942 | 0.624772804479957 | 1333711.54318182 | 655.499190713482 | 63.7789027840119 |
| Maharashtra | 33 | 10.5268081957616 | 1.93039803292807 | 2757972.46242424 | 882.321778049425 | 52.2534100830016 |
| Manipur | 9 | 67.5987809704474 | 4.98728683734625 | 220217.051666667 | 843.622256256101 | 50.0165633266468 |
| Meghalaya | 7 | 64.4926264200829 | 4.77483391291109 | 325151.725 | 946.017920296636 | 63.5951262270377 |
| Mizoram | 8 | 51.9549916295876 | 4.74053974211294 | 104185.15875 | 1198.66428882497 | 61.7142451234391 |
| Nagaland | 8 | 99.7003154183167 | 4.74309513846195 | 118572.34625 | 1235.27747750873 | 41.5966283338895 |
| Odisha | 29 | 6.83880120777512 | 3.08393406232459 | 1205721.18155172 | 606.805301034576 | 53.2755205864839 |
| Puducherry | 4 | 53.7704148434607 | 4.97028095100957 | 207733.7475 | 1333.56765439781 | 44.4357277807352 |
| Punjab | 16 | 30.8770654405604 | 0.932931207974935 | 1452066.4490625 | 1223.43785027676 | 50.1406819071539 |
| Rajasthan | 27 | 4.93754820082534 | 0.360060963705563 | 1755742.01925926 | 805.797817032219 | 70.0862133128767 |
| Sikkim | 4 | 99.6822052368016 | 4.78724906042476 | 129015.62625 | 906.254711120454 | 55.1816987031961 |
| Table truncated in rendered note; full CSV has 36 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | var | label | N | Min | 1Q | Med | 3Q | Max | Mean | SD | desc |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| From 2001 | wavg_ling_degrees | wavg_ling_degrees | Ling. Distance | 573 | 0.00 | 0.05 | 1.70 | 4.13 | 5.00 | 2.09 | 1.96 | Average linguistic distance of mother tongue from Hindi |
| From 2007-08 | EMIE | EMIE | EMIE | 573 | 0.00 | 2.14 | 8.36 | 23.10 | 100.00 | 19.04 | 25.37 | EMI exposure |
| From 2007-08 | npeople_0708 | npeople_0708 | Population | 573 | 12,285 | 773,322 | 1,400,593 | 2,319,260 | 9,922,640 | 1,712,255 | 1,409,894 | Estimated via NSS sample weights |
| From 2007-08 | consumption_0708 | consumption_0708 | Consumption | 573 | 330.09 | 625.37 | 771.51 | 997.67 | 2923.14 | 855.39 | 328.58 | Average household monthly consumption expenditures (Rs.) |
| From 2007-08 | gini_cons_0708 | gini_cons_0708 | Gini of Consumption | 573 | 0.06 | 0.22 | 0.26 | 0.30 | 0.56 | 0.26 | 0.07 | Gini coefficient of consumption |
| From 2007-08 | pct_urban | pct_urban | Pct. Urban | 573 | 0.00 | 8.82 | 16.44 | 29.86 | 100.00 | 22.12 | 19.12 | Percentage of people in an urban area |
| From 2007-08 | avg_hh_size | avg_hh_size | Avg. HH Size | 573 | 3.15 | 4.24 | 4.67 | 5.12 | 6.49 | 4.67 | 0.66 | Average household size |
| From 2007-08 | dependency_ratio | dependency_ratio | Dependency Ratio × 100 | 573 | 23.69 | 48.26 | 56.69 | 70.87 | 110.65 | 59.72 | 15.27 | Ratio of dependents (0-14, 65+) to labor force (15-64), × 100 |
| From 2007-08 | pct_fem_head | pct_fem_head | Pct. Female Head | 573 | 50.44 | 85.56 | 90.09 | 93.72 | 100.00 | 88.93 | 6.86 | Percentage of households with a female head |
| From 2007-08 | pct_hindu | pct_hindu | Pct. Hindu | 573 | 0.00 | 74.19 | 88.54 | 94.79 | 100.00 | 77.89 | 26.75 | Percentage of Hindus |
| From 2007-08 | pct_muslim | pct_muslim | Pct. Muslim | 573 | 0.00 | 1.66 | 5.80 | 12.21 | 100.00 | 10.28 | 15.22 | Percentage of Muslims |
| From 2007-08 | pct_other_religion | pct_other_religion | Pct. Other | 573 | 0.00 | 0.00 | 1.15 | 7.04 | 100.00 | 11.83 | 25.53 | Percentage not Hindu/Muslim |
| From 2007-08 | pct_st | pct_st | Pct. ST | 573 | 0.00 | 0.08 | 3.27 | 21.16 | 100.00 | 17.29 | 27.70 | Scheduled Tribe |
| From 2007-08 | pct_sc | pct_sc | Pct. SC | 573 | 0.00 | 9.16 | 17.69 | 25.34 | 63.90 | 17.69 | 11.29 | Scheduled Caste |
| From 2007-08 | pct_obc | pct_obc | Pct. OBC | 573 | 0.00 | 16.06 | 40.87 | 57.27 | 96.60 | 37.93 | 23.88 | Other Backward Class |
| From 2007-08 | pct_small_land | pct_small_land | Pct. Small Land-Owner | 573 | 1.48 | 31.81 | 46.61 | 60.61 | 94.20 | 46.83 | 18.39 | Owns 0.005–0.40 hectares |
| From 2007-08 | pct_medium_land | pct_medium_land | Pct. Med. Land-Owner | 573 | 0.00 | 17.24 | 28.41 | 39.71 | 90.73 | 29.87 | 17.08 | Owns 0.41–3.00 hectares |
| From 2007-08 | pct_large_land | pct_large_land | Pct. Large Land-Owner | 573 | 0.00 | 0.00 | 1.22 | 4.32 | 34.94 | 3.13 | 4.67 | Owns $\geq$ 3.01 hectares |
| From 2007-08 | pct_head_illiterate | pct_head_illiterate | Pct. Head Educ., Illiterate | 573 | 0.00 | 23.83 | 34.38 | 46.54 | 78.68 | 34.85 | 15.94 | Percentage of household heads with educ. level: illiterate |
| From 2007-08 | pct_head_lit_to_primary | pct_head_lit_to_primary | Pct. Head Educ., Lit.-Primary | 573 | 3.28 | 19.76 | 26.44 | 33.86 | 77.63 | 27.90 | 11.11 | Percentage of heads with educ. level: literate-primary |
| From 2007-08 | pct_head_secondary_plus | pct_head_secondary_plus | Pct. Head Educ., Secondary+ | 573 | 0.58 | 26.84 | 34.86 | 46.02 | 81.49 | 37.24 | 14.53 | Percentage of heads with educ. level: above secondary |
| From 2007-08 | pct_pucca | pct_pucca | Pct. Pucca | 573 | 0.00 | 27.75 | 55.76 | 80.68 | 100.00 | 54.28 | 29.35 | Percentage in pucca (permanent) homes |
| From 2017-18 | npeople_1718 | npeople_1718 | Population | 573 | 18,689 | 860,228 | 1,559,713 | 2,488,207 | 12,274,837 | 1,889,535 | 1,599,341 | Estimated via NSS sample weights |
| From 2017-18 | consumption_1718 | consumption_1718 | Consumption | 573 | 850.53 | 1543.87 | 2045.35 | 2611.09 | 11031.46 | 2239.13 | 975.12 | Average household monthly consumption expenditures (Rs.) |
| From 2017-18 | gini_cons_1718 | gini_cons_1718 | Gini of Consumption | 573 | 0.11 | 0.20 | 0.24 | 0.29 | 0.57 | 0.25 | 0.07 | Gini coefficient of consumption |
| From 2007-08 to 2017-18 | consumption_pct_change | consumption_pct_change | Percent change in consumption | 573 | 12.25 | 124.11 | 158.71 | 193.14 | 542.70 | 165.15 | 61.48 | Percent change in consumption |
| From 2007-08 to 2017-18 | gini_change | gini_change | Change in Gini of consumption | 573 | -0.30 | -0.06 | -0.02 | 0.03 | 0.29 | -0.02 | 0.07 | Change in the Gini coefficient of consumption |

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

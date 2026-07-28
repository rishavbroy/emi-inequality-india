# Instrument Exploration


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Current instrument and treatment diagnostics

The active Phase 1 construction uses the full mutually exclusive Census
2001 C-16 mother-tongue distribution. Language-group subtotal rows are
removed before aggregation, linguistic distances are attached from the
documented Shastry concordance, and unmapped speaker mass remains
visible. The preferred exploratory scalar is
`ling_distance_nonzero_mean`, the speaker-weighted mean distance among
mapped languages with distance greater than zero. Hindi and Urdu are
treated as distance zero and their shares are reported separately.

The preferred exploratory treatment is `emi_exposure_all_children_0708`,
the survey-weighted share of all children ages 5-19 who are both
enrolled and studying in English medium. The historical `EMIE` field
remains a compatibility measure of EMI among enrolled children and is
shown below only in the legacy dotplot section.

The Phase 2 and Phase 3 tables diagnose how the preferred exploratory
first stage changes with six-region fixed effects, state fixed effects,
main and expanded Census control sets, sequential thematic blocks,
VIF/GVIF, state deletion, and district influence. These diagnostics do
not change the public IV specification.

``` r
analysis_deviation_note("The active note now describes the full-distribution linguistic-distance construction and all-child EMI exposure used by the Phase 2-3 diagnostics. The historical top-three/EMIE dotplot is retained below as an explicitly labeled compatibility check rather than as the current instrument definition.")
```

**Deviation note.** The active note now describes the full-distribution
linguistic-distance construction and all-child EMI exposure used by the
Phase 2-3 diagnostics. The historical top-three/EMIE dotplot is retained
below as an explicitly labeled compatibility check rather than as the
current instrument definition.

``` r
iv_match <- read_analysis_csv("diagnostics", "public", "iv_panel_match_summary.csv")
iv_state <- read_analysis_csv("diagnostics", "public", "iv_panel_state_summary.csv")
iv_rows <- read_analysis_csv("diagnostics", "public", "iv_summary_keyed_rows.csv")
iv_dotplot <- analysis_target_csv("diag_ext_instrument_exploration", "instrument_strength_dotplot_data.csv")
iv_notes <- analysis_target_csv("diag_ext_instrument_exploration", "instrument_exploration_legacy_notes.csv")
first_stage_absorption <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_absorption_ladder.csv")
first_stage_support <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_absorption_common_support.csv")
first_stage_state_ranges <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_state_residual_ranges.csv")
first_stage_state_deletion <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_state_deletion.csv")
first_stage_district_influence <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_district_influence.csv")
first_stage_vif <- analysis_target_csv("diag_ext_first_stage_absorption", "first_stage_vif.csv")
alternative_distance_summary <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "alternative_distance_first_stage_summary.csv")
alternative_distance_coefficients <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "alternative_distance_first_stage_coefficients.csv")
alternative_distance_support <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "alternative_distance_first_stage_common_support.csv")
mapping_coverage_sensitivity <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "alternative_distance_mapping_coverage_sensitivity.csv")
distance4_languages <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "distance4_language_decomposition.csv")
unmapped_languages <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "unmapped_language_decomposition.csv")
distance4_leave_one_out <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "distance4_leave_one_language_out.csv")
weak_iv_outcomes <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "alternative_distance_weak_iv_outcomes.csv")
```

## Legacy top-three and EMI-among-enrolled compatibility check

The current analog of the historical
`ggplot(..., aes(x = district_code_0708, y = EMIE, color = district_prefix)) + geom_point()`
check uses 573 active district-panel rows. It is retained to make
earlier drafts reproducible, not as the preferred Phase 1 definition.

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
| 1 | 1113 | Jammu & Kashmir | Jammu | 55.9373478876879 | 1.65578136240385 | 1 |
| 2 | 1114 | Jammu & Kashmir | Kathus | 44.1686433398463 | 0.935837739720622 | 1 |
| 3 | 1209 | Jammu & Kashmir | Doda | 100 | 2.78172518661874 | 1 |
| 4 | 1210 | Jammu & Kashmir | Udhampur | 100 | 1.69984110612438 | 1 |
| 5 | 1212 | Jammu & Kashmir | Rajauri | 19.1220027592448 | 0.129961284174162 | 1 |
| 6 | 1301 | Jammu & Kashmir | Kupwara | 35.3856881455635 | 3.11420512358161 | 1 |
| 7 | 1302 | Jammu & Kashmir | Baramula | 50.1462484103989 | 3.56535491706951 | 1 |
| 8 | 1303 | Jammu & Kashmir | Srinagar | 85.077782419044 | 3.75747237390968 | 1 |
| 9 | 1304 | Jammu & Kashmir | Badgam | 63.7323207233014 | 3.90722054947655 | 1 |
| 10 | 1305 | Jammu & Kashmir | Pulwama | 90.3661939188381 | 3.83193189231301 | 1 |
| 11 | 1306 | Jammu & Kashmir | Anantnag | 85.3633724175985 | 3.60288643704987 | 1 |
| 12 | 2102 | Himachal Pradesh | Kangra | 23.0964013292776 | 0 | 2 |
| 13 | 2104 | Himachal Pradesh | Kullu | 2.16035948115958 | 0.522908063394834 | 2 |
| 14 | 2105 | Himachal Pradesh | Mandi | 13.1015338642775 | 0 | 2 |
| 15 | 2106 | Himachal Pradesh | Hamirpur | 16.4639133923992 | 0 | 2 |
| 16 | 2107 | Himachal Pradesh | Una | 14.6258166762162 | 0.170456319147959 | 2 |
| 17 | 2201 | Himachal Pradesh | Chamba | 11.6242446239474 | 0 | 2 |
| 18 | 2203 | Himachal Pradesh | Lahul & Spiti | 6.31027748728387 | 4.54177723124717 | 2 |
| 19 | 2208 | Himachal Pradesh | Bilaspur | 16.8181705232738 | 0.556767522862926 | 2 |
| 20 | 2209 | Himachal Pradesh | Solan | 17.8837636459365 | 0.0460922743841225 | 2 |
| 21 | 2210 | Himachal Pradesh | Sirmapur | 17.7238270927478 | 0 | 2 |
| 22 | 2211 | Himachal Pradesh | Shimla | 41.7306051009988 | 0.0176638462227771 | 2 |
| 23 | 2212 | Himachal Pradesh | Kinnaur | 5.15399235250357 | 2.24336178283547 | 2 |
| 24 | 3101 | Punjab | Gurdaspur | 34.4063148539354 | 0.962116231873792 | 3 |
| 25 | 3102 | Punjab | Amritsar | 32.9079601779732 | 0.958142335350402 | 3 |
| 26 | 3103 | Punjab | Kapurthala | 43.9577396007221 | 0.939462953235953 | 3 |
| 27 | 3104 | Punjab | Jalandhar | 39.7958708417419 | 0.922250230613504 | 3 |
| 28 | 3106 | Punjab | Nawanshahr | 39.073688061125 | 0.968443709289662 | 3 |
| 29 | 3107 | Punjab | Rupnagar | 37.1008378309431 | 0.897755374045511 | 3 |
| 30 | 3208 | Punjab | Fatehgarh Sahib | 26.1747306686159 | 0.935960701335744 | 3 |
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
| Andaman & Nicobar Islands | 2 | 28.9337363688688 | 3.18043264711969 | 114699.0625 | 1716.59345353575 | 39.9965239648858 |
| Andhra Pradesh | 13 | 24.4802289280157 | 4.65706249020543 | 3406138.48653846 | 806.038478947683 | 46.8784465840865 |
| Arunachal Pradesh | 13 | 95.0157164422056 | 3.82326257358283 | 76279.7523076923 | 821.925150923097 | 61.0798056117889 |
| Assam | 22 | 5.3275500840982 | 3.00248252326762 | 1070667.90886364 | 762.436038575419 | 58.0450186320867 |
| Bihar | 37 | 3.38601239299322 | 0.383274378072781 | 2051830.32918919 | 559.316304711329 | 84.7022490469553 |
| Chandigarh | 1 | 58.3318564651552 | 0.300674842660398 | 837516.24 | 2237.62405162436 | 36.7830442420546 |
| Chhattisgarh | 16 | 3.2273017372452 | 0.783474808060863 | 1455306.785 | 519.343724642298 | 59.4295625174172 |
| Dadra & Nagar Haveli | 1 | 5.45838437776184 | 0.969649054764959 | 204832.69 | 878.890652903109 | 55.2790526851351 |
| Daman & Diu | 2 | 22.0521469070064 | 0.903703932246381 | 69690.93 | 1391.7018118573 | 51.6253036614033 |
| Delhi | 7 | 32.8114108555629 | 0.0755856765623703 | 1802076.03428571 | 1274.45536408576 | 51.9338015790853 |
| Goa | 2 | 62.2874081579816 | 2.20277678783885 | 698016.255 | 1277.6412702353 | 35.7209708999027 |
| Gujarat | 25 | 4.42406172844767 | 1.06293855831635 | 2038854.0708 | 858.757699979292 | 53.6880918495208 |
| Haryana | 19 | 20.170112602026 | 0.113451216367665 | 1142009.75631579 | 974.810008859313 | 53.1686777255239 |
| Himachal Pradesh | 12 | 15.5577421308352 | 0.674918920007938 | 519885.804583333 | 932.211852792387 | 53.7694561219673 |
| Jammu & Kashmir | 11 | 66.2999636383203 | 2.63474708840382 | 735654.508181818 | 917.231422452719 | 52.1863851221851 |
| Jharkhand | 18 | 6.48227701539392 | 1.42000990212625 | 1395211.16555556 | 632.040527265219 | 67.2780276872427 |
| Karnataka | 27 | 16.0376594835309 | 4.37519387186768 | 1844944.73574074 | 753.91460740765 | 49.7504147543725 |
| Kerala | 14 | 43.4497857986853 | 4.99645393900026 | 2129849.7675 | 1061.81342032626 | 48.0392820929302 |
| Lakshadweep | 1 | 32.4512530246271 | 4.9822009569378 | 57165.375 | 1258.92789866243 | 47.3127675768181 |
| Madhya Pradesh | 44 | 6.54482431274942 | 0.125803453485055 | 1333711.54318182 | 598.39039835291 | 63.7789027840119 |
| Maharashtra | 33 | 10.5268081957616 | 1.76886396762953 | 2757972.46242424 | 790.791751568391 | 52.2534100830016 |
| Manipur | 9 | 67.5987809704474 | 4.86239997201659 | 220217.051666667 | 819.916168102988 | 50.0165633266468 |
| Meghalaya | 7 | 64.4926264200829 | 4.76974083074187 | 325151.725 | 889.075788565463 | 63.5951262270377 |
| Mizoram | 8 | 51.9549916295876 | 4.69164932951452 | 104185.15875 | 1154.1292730263 | 61.7142451234391 |
| Nagaland | 8 | 99.7003154183167 | 3.73615265860392 | 118572.34625 | 1183.08745343988 | 41.5966283338895 |
| Odisha | 29 | 6.83880120777512 | 3.07904877017756 | 1205721.18155172 | 530.726549256539 | 53.2755205864839 |
| Puducherry | 4 | 53.7704148434607 | 4.95950894257703 | 207733.7475 | 1172.67173441507 | 44.4357277807352 |
| Punjab | 16 | 30.8770654405604 | 0.932128859706136 | 1452066.4490625 | 1116.02721196292 | 50.1406819071539 |
| Rajasthan | 27 | 4.93754820082534 | 0.0361302136656299 | 1755742.01925926 | 730.020097099085 | 70.0862133128767 |
| Sikkim | 4 | 99.6822052368016 | 3.56386538281542 | 129015.62625 | 773.645294969817 | 55.1816987031961 |
| Table truncated in rendered note; full CSV has 36 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | var | label | N | Min | 1Q | Med | 3Q | Max | Mean | SD | desc |
|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Treatment and instrument | wavg_ling_degrees | wavg_ling_degrees | Linguistic distance | 573 | 0.00 | 0.00 | 1.22 | 3.72 | 5.00 | 1.92 | 1.92 | Population-weighted linguistic distance of district mother tongues from Hindi |
| Treatment and instrument | EMIE | EMIE | EMI exposure | 573 | 0.00 | 2.14 | 8.36 | 23.10 | 100.00 | 19.04 | 25.37 | Share of school-going children enrolled in English-medium instruction |
| Consumption outcomes | real_consumption_0708 | real_consumption_0708 | Real consumption, 2007-08 | 573 | 439.46 | 826.48 | 986.00 | 1179.91 | 2945.06 | 1035.78 | 295.72 | Person-weighted monthly consumption in common prices |
| Consumption outcomes | real_consumption_1718 | real_consumption_1718 | Real consumption, 2017-18 | 573 | 561.61 | 996.59 | 1174.29 | 1418.34 | 4170.07 | 1261.98 | 398.55 | Person-weighted monthly consumption in common prices |
| Consumption outcomes | real_log_consumption_change | real_log_consumption_change | Real log consumption change | 573 | -0.86 | 0.07 | 0.20 | 0.34 | 0.98 | 0.19 | 0.22 | Log real consumption in 2017-18 minus log real consumption in 2007-08 |
| Census 2001 controls | log_population_2001 | log_population_2001 | Log population | 573 | 10.35 | 13.61 | 14.20 | 14.68 | 16.08 | 14.00 | 1.02 | Natural log of Census 2001 district population |
| Census 2001 controls | urban_share_2001 | urban_share_2001 | Urban population share | 573 | 0.00 | 10.03 | 18.15 | 29.73 | 100.00 | 23.27 | 19.11 | Urban population as a percentage of total population |
| Census 2001 controls | adult_secondary_plus_share_2001 | adult_secondary_plus_share_2001 | Secondary-plus share, age 7+ | 573 | 4.40 | 11.39 | 15.62 | 21.08 | 43.67 | 17.09 | 7.63 | Population age 7 and above with matric or higher attainment |
| Census 2001 controls | sc_share_2001 | sc_share_2001 | Scheduled Caste share | 573 | 0.00 | 8.13 | 15.64 | 19.98 | 50.11 | 14.71 | 8.64 | Scheduled Caste population as a percentage of total population |
| Census 2001 controls | st_share_2001 | st_share_2001 | Scheduled Tribe share | 573 | 0.00 | 0.15 | 3.49 | 18.27 | 98.09 | 16.16 | 25.85 | Scheduled Tribe population as a percentage of total population |
| Census 2001 controls | muslim_share_2001 | muslim_share_2001 | Muslim share | 573 | 0.09 | 2.59 | 7.21 | 13.51 | 98.49 | 11.40 | 14.81 | Muslim population as a percentage of total population |
| Census 2001 controls | agricultural_worker_share_2001 | agricultural_worker_share_2001 | Agricultural worker share | 573 | 0.00 | 50.56 | 66.32 | 75.37 | 90.24 | 60.53 | 20.40 | Cultivators and agricultural labourers as a percentage of workers |
| Census 2001 controls | dependency_ratio_2001 | dependency_ratio_2001 | Dependency ratio | 573 | 35.46 | 58.79 | 69.09 | 80.75 | 100.75 | 69.39 | 13.29 | Population age 0-14 and 65+ as a percentage of population age 15-64 |
| Census 2001 controls | electricity_access_share_2001 | electricity_access_share_2001 | Electricity access share | 573 | 3.09 | 28.36 | 60.14 | 78.40 | 99.71 | 54.44 | 28.01 | Households using electricity for lighting as a percentage of households |
| Census 2001 controls | log_population_density_2001 | log_population_density_2001 | Log population density | 573 | 0.88 | 5.23 | 5.76 | 6.41 | 10.82 | 5.77 | 1.16 | Natural log of persons per square kilometre |

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

``` r
analysis_table(first_stage_support, "Fixed common support for first-stage absorption diagnostics")
```

| treatment | instrument | n | n_states | n_regions |
|:---|:---|---:|---:|---:|
| emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 573 | 35 | 6 |

Fixed common support for first-stage absorption diagnostics

``` r
analysis_table(first_stage_absorption, "First-stage absorption ladder", max_rows = 30)
```

| specification_id | specification | sequence | treatment | instrument | fixed_effect | control_blocks | n_controls | estimate | std.error | statistic | p.value | excluded_instrument_f | partial_r_squared | residual_instrument_sd | residual_treatment_sd | residual_correlation | instrument_variance_remaining | n | n_states | n_regions | status | reason |
|:---|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| instrument_only | Instrument only | 1 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none |  | 0 | 5.440 | 1.756 | 3.097 | 0.002 | 9.591 | 0.122 | 1.307 | 20.353 | 0.349 | 1.000 | 573 | 35 | 6 | estimated | NA |
| region_fe | Six-region fixed effects | 2 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region |  | 0 | 5.056 | 3.153 | 1.604 | 0.109 | 2.572 | 0.057 | 0.750 | 15.938 | 0.238 | 0.329 | 573 | 35 | 6 | estimated | NA |
| state_fe | State fixed effects | 3 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state |  | 0 | -0.332 | 0.677 | -0.491 | 0.624 | 0.241 | 0.000 | 0.534 | 8.448 | -0.021 | 0.167 | 573 | 35 | 6 | estimated | NA |
| census_controls | Main Census controls | 4 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 4.637 | 1.201 | 3.861 | 0.000 | 14.904 | 0.130 | 1.104 | 14.193 | 0.361 | 0.713 | 573 | 35 | 6 | estimated | NA |
| region_fe_census_controls | Six-region fixed effects + main Census controls | 5 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 3.628 | 2.199 | 1.650 | 0.099 | 2.723 | 0.034 | 0.639 | 12.553 | 0.185 | 0.239 | 573 | 35 | 6 | estimated | NA |
| state_fe_census_controls | State fixed effects + main Census controls | 6 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 0.514 | 0.592 | 0.869 | 0.385 | 0.755 | 0.001 | 0.496 | 6.627 | 0.039 | 0.144 | 573 | 35 | 6 | estimated | NA |
| expanded_controls | Expanded Census controls | 7 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.303 | 1.151 | 4.607 | 0.000 | 21.220 | 0.174 | 1.081 | 13.753 | 0.417 | 0.684 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_controls | Six-region fixed effects + expanded Census controls | 8 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 4.939 | 2.056 | 2.403 | 0.017 | 5.774 | 0.064 | 0.621 | 12.149 | 0.253 | 0.226 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.641 | 0.551 | 1.162 | 0.246 | 1.349 | 0.002 | 0.489 | 6.583 | 0.048 | 0.140 | 573 | 35 | 6 | estimated | NA |
| region_fe_main_without_human_capital | Six-region FE + main controls without human capital | 10 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 4.458 | 2.137 | 2.086 | 0.037 | 4.352 | 0.048 | 0.645 | 13.186 | 0.218 | 0.244 | 573 | 35 | 6 | estimated | NA |
| state_fe_main_without_human_capital | State FE + main controls without human capital | 11 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 0.950 | 0.614 | 1.547 | 0.123 | 2.392 | 0.005 | 0.501 | 6.813 | 0.070 | 0.147 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_without_human_capital | Six-region FE + expanded controls without human capital | 12 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 5.019 | 2.072 | 2.422 | 0.016 | 5.866 | 0.062 | 0.640 | 12.893 | 0.249 | 0.240 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_without_human_capital | State FE + expanded controls without human capital | 13 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 1.055 | 0.596 | 1.769 | 0.077 | 3.130 | 0.006 | 0.497 | 6.804 | 0.077 | 0.145 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_scale_geography | Six-region FE + through basic scale geography | 14 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography | 3 | 4.475 | 2.658 | 1.683 | 0.093 | 2.834 | 0.049 | 0.694 | 14.080 | 0.220 | 0.282 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_social_composition | Six-region FE + through social composition | 15 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition | 6 | 3.486 | 2.119 | 1.645 | 0.100 | 2.707 | 0.027 | 0.654 | 13.784 | 0.165 | 0.250 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_human_capital | Six-region FE + through human capital | 16 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital | 8 | 3.285 | 2.025 | 1.622 | 0.105 | 2.631 | 0.029 | 0.653 | 12.689 | 0.169 | 0.249 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_demography | Six-region FE + through demography | 17 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography | 9 | 3.330 | 2.079 | 1.602 | 0.110 | 2.566 | 0.029 | 0.648 | 12.689 | 0.170 | 0.246 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_economic_structure | Six-region FE + through economic structure | 18 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 3.996 | 2.126 | 1.879 | 0.061 | 3.531 | 0.040 | 0.629 | 12.601 | 0.199 | 0.231 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_development | Six-region FE + through basic development | 19 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 4.939 | 2.056 | 2.403 | 0.017 | 5.774 | 0.064 | 0.621 | 12.149 | 0.253 | 0.226 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_scale_geography | State FE + through basic scale geography | 20 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography | 3 | 1.193 | 0.548 | 2.177 | 0.030 | 4.739 | 0.008 | 0.508 | 6.995 | 0.087 | 0.151 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_social_composition | State FE + through social composition | 21 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition | 6 | 1.086 | 0.582 | 1.868 | 0.062 | 3.488 | 0.006 | 0.504 | 6.968 | 0.079 | 0.148 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_human_capital | State FE + through human capital | 22 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital | 8 | 0.565 | 0.567 | 0.997 | 0.319 | 0.994 | 0.002 | 0.500 | 6.615 | 0.043 | 0.146 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_demography | State FE + through demography | 23 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography | 9 | 0.593 | 0.566 | 1.048 | 0.295 | 1.098 | 0.002 | 0.499 | 6.605 | 0.045 | 0.146 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_economic_structure | State FE + through economic structure | 24 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 0.706 | 0.570 | 1.239 | 0.216 | 1.535 | 0.003 | 0.493 | 6.590 | 0.053 | 0.142 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_development | State FE + through basic development | 25 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.641 | 0.551 | 1.162 | 0.246 | 1.349 | 0.002 | 0.489 | 6.583 | 0.048 | 0.140 | 573 | 35 | 6 | estimated | NA |

First-stage absorption ladder

``` r
analysis_table(alternative_distance_support, "Fixed support for alternative linguistic-distance first stages")
```

| treatment                      |   n | n_states | n_regions |
|:-------------------------------|----:|---------:|----------:|
| emi_exposure_all_children_0708 | 573 |       35 |         6 |

Fixed support for alternative linguistic-distance first stages

``` r
analysis_table(
  alternative_distance_summary[alternative_distance_summary$adjustment_id %in% c("region_expanded", "state_expanded"), ],
  "Alternative linguistic-distance first stages: preferred adjustment sets",
  max_rows = 30
)
```

| specification_id | sequence | adjustment_id | adjustment | construction_id | construction | fixed_effect | excluded_instruments | included_language_controls | n_excluded_instruments | joint_excluded_f | joint_excluded_p | partial_r_squared | n | n_states | n_regions |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|
| region_expanded\_\_nonzero_mean | 17 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 5.774 | 0.017 | 0.064 | 573 | 35 | 6 |
| region_expanded\_\_distant_share | 18 | region_expanded | Six-region FE + expanded controls | distant_share | Share speaking languages at distance three or higher | region | ling_share_distance_ge3 |  | 1 | 0.029 | 0.866 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_top3_legacy | 19 | region_expanded | Six-region FE + expanded controls | top3_legacy | Legacy top-three weighted mean | region | ling_distance_top3_legacy |  | 1 | 8.130 | 0.005 | 0.072 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu | 20 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | region | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 6.564 | 0.011 | 0.076 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu_separate | 21 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | region | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 5.720 | 0.017 | 0.071 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all | 22 | region_expanded | Six-region FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 12.440 | 0.000 | 0.193 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all_unmapped | 23 | region_expanded | Six-region FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 21.404 | 0.000 | 0.241 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_mapped | 24 | region_expanded | Six-region FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | region | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 19.168 | 0.000 | 0.246 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean | 33 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.349 | 0.246 | 0.002 | 573 | 35 | 6 |
| state_expanded\_\_distant_share | 34 | state_expanded | State FE + expanded controls | distant_share | Share speaking languages at distance three or higher | state | ling_share_distance_ge3 |  | 1 | 0.497 | 0.481 | 0.004 | 573 | 35 | 6 |
| state_expanded\_\_top3_legacy | 35 | state_expanded | State FE + expanded controls | top3_legacy | Legacy top-three weighted mean | state | ling_distance_top3_legacy |  | 1 | 1.593 | 0.208 | 0.012 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu | 36 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | state | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 1.090 | 0.297 | 0.003 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu_separate | 37 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | state | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 0.868 | 0.352 | 0.002 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all | 38 | state_expanded | State FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 4.046 | 0.001 | 0.032 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all_unmapped | 39 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 3.958 | 0.002 | 0.032 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_mapped | 40 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 3.486 | 0.004 | 0.045 | 573 | 35 | 6 |

Alternative linguistic-distance first stages: preferred adjustment sets

``` r
analysis_table(
  mapping_coverage_sensitivity[
    mapping_coverage_sensitivity$adjustment_id == "state_expanded" &
      mapping_coverage_sensitivity$construction_id %in% c("nonzero_mean", "distance_shares_all_unmapped", "distance_shares_mapped"),
  ],
  "Mapping-coverage sensitivity under state fixed effects and expanded controls",
  max_rows = 30
)
```

| specification_id | sequence | adjustment_id | adjustment | construction_id | construction | fixed_effect | excluded_instruments | included_language_controls | n_excluded_instruments | joint_excluded_f | joint_excluded_p | partial_r_squared | n | n_states | n_regions | minimum_mapped_share | coverage_sample_n |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| state_expanded\_\_nonzero_mean | 33 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.349 | 0.246 | 0.002 | 573 | 35 | 6 | 0 | 573 |
| state_expanded\_\_distance_shares_all_unmapped | 39 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 3.958 | 0.002 | 0.032 | 573 | 35 | 6 | 0 | 573 |
| state_expanded\_\_distance_shares_mapped | 40 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 3.486 | 0.004 | 0.045 | 573 | 35 | 6 | 0 | 573 |
| state_expanded\_\_nonzero_mean | 33 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.737 | 0.391 | 0.002 | 508 | 33 | 6 | 90 | 508 |
| state_expanded\_\_distance_shares_all_unmapped | 39 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 4.891 | 0.000 | 0.060 | 508 | 33 | 6 | 90 | 508 |
| state_expanded\_\_distance_shares_mapped | 40 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 6.448 | 0.000 | 0.064 | 508 | 33 | 6 | 90 | 508 |
| state_expanded\_\_nonzero_mean | 33 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.341 | 0.560 | 0.001 | 488 | 33 | 6 | 95 | 488 |
| state_expanded\_\_distance_shares_all_unmapped | 39 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 1.509 | 0.186 | 0.019 | 488 | 33 | 6 | 95 | 488 |
| state_expanded\_\_distance_shares_mapped | 40 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 1.875 | 0.097 | 0.018 | 488 | 33 | 6 | 95 | 488 |
| state_expanded\_\_nonzero_mean | 33 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.790 | 0.375 | 0.004 | 432 | 28 | 6 | 99 | 432 |
| state_expanded\_\_distance_shares_all_unmapped | 39 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share | 5 | 1.137 | 0.340 | 0.024 | 432 | 28 | 6 | 99 | 432 |
| state_expanded\_\_distance_shares_mapped | 40 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 1.398 | 0.224 | 0.025 | 432 | 28 | 6 | 99 | 432 |

Mapping-coverage sensitivity under state fixed effects and expanded
controls

``` r
analysis_table(unmapped_languages, "Largest unmapped C-16 language-state cells", max_rows = 20)
```

| canonical_language | state_code_2001 | unmapped_speakers | n_districts | share_of_unmapped_speakers |
|:---|:---|:---|:---|:---|
| Bhili/Bhilodi | 23 | 2973201 | 43 | 13.3349099725691 |
| Bhili/Bhilodi | 24 | 2405663 | 25 | 10.7894822883957 |
| Bhili/Bhilodi | 8 | 2208369 | 27 | 9.90461183122581 |
| Dogri | 1 | 2203656 | 11 | 9.88347386218143 |
| Khandeshi | 27 | 1854808 | 33 | 8.31887843990397 |
| Bhili/Bhilodi | 27 | 1482953 | 33 | 6.65109582182679 |
| Nepali | 19 | 1022725 | 18 | 4.58695722276957 |
| Nepali | 18 | 562599 | 22 | 2.52327609726264 |
| Halabi | 22 | 544874 | 12 | 2.44377885531237 |
| Miri/Mishing | 18 | 517170 | 22 | 2.31952545102519 |
| Karbi / Mikir | 18 | 406156 | 22 | 1.82162379698472 |
| Korku | 23 | 372224 | 20 | 1.66943759592088 |
| Nepali | 11 | 338606 | 4 | 1.51865969578637 |
| Koya | 28 | 248095 | 22 | 1.11271471038942 |
| Konyak | 13 | 248002 | 8 | 1.11229760215239 |
| Nepali | 9 | 247062 | 68 | 1.10808166943401 |
| Korku | 27 | 195652 | 22 | 0.877506030017172 |
| Lotha | 13 | 168356 | 8 | 0.755082519931158 |
| Coorgi/Kodagu | 29 | 164403 | 27 | 0.737353177339936 |
| Khandeshi | 24 | 152096 | 23 | 0.682155853972828 |
| Table truncated in rendered note; full CSV has 1157 rows. |  |  |  |  |

Largest unmapped C-16 language-state cells

``` r
analysis_table(distance4_languages, "Distance-four speaker composition by language and state", max_rows = 20)
```

| canonical_language | state_code_2001 | speakers | n_districts | national_language_speakers | speaker_share_of_distance4 |
|:---|:---|:---|:---|:---|:---|
| Kashmiri | 1 | 5387044 | 11 | 5486041 | 68.2949995334629 |
| Sindhi | 24 | 958787 | 25 | 2401863 | 12.1551555394183 |
| Sindhi | 27 | 658560 | 33 | 2401863 | 8.34898599171592 |
| Sindhi | 8 | 309215 | 27 | 2401863 | 3.92011616774241 |
| Sindhi | 23 | 254508 | 44 | 2401863 | 3.22656056666004 |
| Sindhi | 22 | 89325 | 16 | 2401863 | 1.13243011071129 |
| Kashmiri | 2 | 50192 | 12 | 5486041 | 0.636316060641712 |
| Sindhi | 7 | 40490 | 7 | 2401863 | 0.513317606299468 |
| Sindhi | 9 | 33407 | 61 | 2401863 | 0.423521888704528 |
| Kashmiri | 7 | 20480 | 7 | 5486041 | 0.259638048333245 |
| Sindhi | 29 | 14694 | 24 | 2401863 | 0.186285228623472 |
| Sindhi | 28 | 10320 | 21 | 2401863 | 0.130833235292924 |
| Sindhi | 33 | 7375 | 18 | 2401863 | 0.0934975882059417 |
| Sindhi | 19 | 5749 | 14 | 2401863 | 0.0728837470638588 |
| Sindhi | 6 | 5510 | 18 | 2401863 | 0.0698537913240323 |
| Kashmiri | 27 | 4988 | 29 | 5486041 | 0.0632360637249135 |
| Kashmiri | 5 | 3962 | 10 | 5486041 | 0.0502288060300937 |
| Kashmiri | 6 | 3717 | 17 | 5486041 | 0.0471227844557946 |
| Sindhi | 10 | 3251 | 25 | 2401863 | 0.041215004645087 |
| Sindhi | 21 | 2565 | 23 | 2401863 | 0.0325181442370495 |
| Table truncated in rendered note; full CSV has 69 rows. |  |  |  |  |  |

Distance-four speaker composition by language and state

``` r
analysis_table(distance4_leave_one_out, "Distance-four leave-one-language-out joint tests", max_rows = 15)
```

| omitted_distance4_language | joint_excluded_f | joint_excluded_p | partial_r_squared | n |
|:---|---:|---:|---:|---:|
| Kashmiri | 0.905 | 0.477 | 0.007 | 573 |
| Sindhi | 4.664 | 0.000 | 0.033 | 573 |

Distance-four leave-one-language-out joint tests

``` r
analysis_table(weak_iv_outcomes, "Weak-IV-aware exploratory outcome estimates", max_rows = 10)
```

| specification_id | estimate_2sls | std_error_clustered | p_value_clustered | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_left_truncated | ar_95_right_truncated | n |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|:---|---:|
| scalar_nonzero_mean | 0.048 | 0.052 | 0.358 | 3.601 | 0.058 | 3.601 | 0.058 | -0.110 | 0.110 | FALSE | TRUE | TRUE | 573 |
| distance_shares_all_unmapped | -0.001 | 0.006 | 0.815 | 4.050 | 0.001 | 4.050 | 0.001 | NA | NA | TRUE | FALSE | FALSE | 573 |
| distance_shares_mapped | 0.000 | 0.003 | 0.931 | 2.079 | 0.067 | 2.079 | 0.067 | -0.001 | 0.006 | FALSE | FALSE | FALSE | 573 |

Weak-IV-aware exploratory outcome estimates

``` r
analysis_table(
  alternative_distance_coefficients[
    grepl("distance_shares", alternative_distance_coefficients$specification_id) &
      grepl("state_expanded", alternative_distance_coefficients$specification_id),
  ],
  "State-expanded distance-share coefficients",
  max_rows = 20
)
```

| specification_id | term | estimate | std.error | statistic | p.value |
|:---|:---|---:|---:|---:|---:|
| state_expanded\_\_distance_shares_all | ling_share_distance_1 | -0.035 | 0.023 | -1.509 | 0.132 |
| state_expanded\_\_distance_shares_all | ling_share_distance_2 | -0.033 | 0.036 | -0.927 | 0.354 |
| state_expanded\_\_distance_shares_all | ling_share_distance_3 | -0.017 | 0.024 | -0.731 | 0.465 |
| state_expanded\_\_distance_shares_all | ling_share_distance_4 | 0.227 | 0.066 | 3.434 | 0.001 |
| state_expanded\_\_distance_shares_all | ling_share_distance_5 | 0.020 | 0.055 | 0.367 | 0.714 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_1 | -0.030 | 0.025 | -1.212 | 0.226 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_2 | -0.023 | 0.035 | -0.650 | 0.516 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_3 | -0.015 | 0.024 | -0.628 | 0.530 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_4 | 0.238 | 0.079 | 3.012 | 0.003 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_5 | 0.034 | 0.052 | 0.658 | 0.511 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_1 | -0.032 | 0.034 | -0.939 | 0.348 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_2 | -0.007 | 0.049 | -0.140 | 0.889 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_3 | -0.010 | 0.024 | -0.413 | 0.680 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_4 | 0.305 | 0.085 | 3.590 | 0.000 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_5 | 0.039 | 0.042 | 0.932 | 0.352 |

State-expanded distance-share coefficients

``` r
analysis_table(first_stage_vif, "Main and expanded-control VIF/GVIF diagnostics", max_rows = 40)
```

| term | model_scope | df | vif | gvif | gvif_scaled | status | reason | specification_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| ling_distance_nonzero_mean | model_regressors | 1 | 4.19094118596823 | 4.19094118596823 | 2.04717883585392 | estimated | NA | region_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 2.43965647174677 | 2.43965647174677 | 1.56193997059643 | estimated | NA | region_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 3.25621839824171 | 3.25621839824171 | 1.80449948690536 | estimated | NA | region_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.07903451652192 | 4.07903451652192 | 2.01966198075864 | estimated | NA | region_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 2.6474332899362 | 2.6474332899362 | 1.62709350989309 | estimated | NA | region_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 3.19436972218934 | 3.19436972218934 | 1.78727997867971 | estimated | NA | region_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 1.6646175970965 | 1.6646175970965 | 1.29020060343208 | estimated | NA | region_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 4.45908681339414 | 4.45908681339414 | 2.1116549939311 | estimated | NA | region_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 3.88702831941567 | 3.88702831941567 | 1.97155479746713 | estimated | NA | region_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.53696374496203 | 4.53696374496203 | 2.13001496355355 | estimated | NA | region_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 3.6486049625845 | 3.6486049625845 | 1.91013218458423 | estimated | NA | region_fe_census_controls |
| factor(region) | model_regressors | 5 | NA | 32.655476638836 | 1.41708402667471 | estimated | NA | region_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 4.43089985336262 | 4.43089985336262 | 2.10497027374797 | estimated | NA | region_fe_expanded_controls |
| log_population_2001 | model_regressors | 1 | 2.48046664834596 | 2.48046664834596 | 1.57494972883136 | estimated | NA | region_fe_expanded_controls |
| urban_share_2001 | model_regressors | 1 | 3.28580468067789 | 3.28580468067789 | 1.81267886860246 | estimated | NA | region_fe_expanded_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.85579412959077 | 4.85579412959077 | 2.20358665125535 | estimated | NA | region_fe_expanded_controls |
| sc_share_2001 | model_regressors | 1 | 2.73691673166291 | 2.73691673166291 | 1.65436293831278 | estimated | NA | region_fe_expanded_controls |
| st_share_2001 | model_regressors | 1 | 3.23989043615113 | 3.23989043615113 | 1.79996956534024 | estimated | NA | region_fe_expanded_controls |
| muslim_share_2001 | model_regressors | 1 | 1.8646685745115 | 1.8646685745115 | 1.36552867949066 | estimated | NA | region_fe_expanded_controls |
| dependency_ratio_2001 | model_regressors | 1 | 5.29570665734401 | 5.29570665734401 | 2.30124024329143 | estimated | NA | region_fe_expanded_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.93331643527213 | 4.93331643527213 | 2.22110702922487 | estimated | NA | region_fe_expanded_controls |
| log_population_density_2001 | model_regressors | 1 | 3.78734443538457 | 3.78734443538457 | 1.94611007792071 | estimated | NA | region_fe_expanded_controls |
| literacy_share_2001 | model_regressors | 1 | 3.28597929786626 | 3.28597929786626 | 1.81272703346816 | estimated | NA | region_fe_expanded_controls |
| worker_share_2001 | model_regressors | 1 | 3.53430183205489 | 3.53430183205489 | 1.87997389132267 | estimated | NA | region_fe_expanded_controls |
| cultivator_share_workers_2001 | model_regressors | 1 | 5.19801994309919 | 5.19801994309919 | 2.279916652665 | estimated | NA | region_fe_expanded_controls |
| agricultural_labourer_share_workers_2001 | model_regressors | 1 | 3.81842927354898 | 3.81842927354898 | 1.95408016047167 | estimated | NA | region_fe_expanded_controls |
| factor(region) | model_regressors | 5 | NA | 55.5491506570224 | 1.49440303981321 | estimated | NA | region_fe_expanded_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 6.9378651596392 | 6.9378651596392 | 2.6339827561393 | estimated | NA | state_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 5.61993575138572 | 5.61993575138572 | 2.37064036736611 | estimated | NA | state_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 5.96091872436069 | 5.96091872436069 | 2.44149927797669 | estimated | NA | state_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 8.0967736081637 | 8.0967736081637 | 2.84548301842828 | estimated | NA | state_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 4.12756112485302 | 4.12756112485302 | 2.031640008676 | estimated | NA | state_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 5.54013209347615 | 5.54013209347615 | 2.35374851959086 | estimated | NA | state_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 3.46112413849601 | 3.46112413849601 | 1.86040966953411 | estimated | NA | state_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 7.72526491195096 | 7.72526491195096 | 2.7794360780473 | estimated | NA | state_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 7.72895950960168 | 7.72895950960168 | 2.78010062940205 | estimated | NA | state_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 10.4421661398941 | 10.4421661398941 | 3.23143406862868 | estimated | NA | state_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 6.94735358026745 | 6.94735358026745 | 2.63578329539199 | estimated | NA | state_fe_census_controls |
| factor(state_code_2001) | model_regressors | 34 | NA | 64373.4820786059 | 1.17683690567705 | estimated | NA | state_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.14809303270608 | 7.14809303270608 | 2.67359178497879 | estimated | NA | state_fe_expanded_controls |
| Table truncated in rendered note; full CSV has 54 rows. |  |  |  |  |  |  |  |  |

Main and expanded-control VIF/GVIF diagnostics

``` r
analysis_table(first_stage_state_ranges, "State-by-state residual ranges", max_rows = 40)
```

| specification_id | state_code_2001 | n_districts | instrument_min | instrument_max | instrument_range | instrument_sd | treatment_min | treatment_max | treatment_range | treatment_sd |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| instrument_only | 1 | 11 | -0.851182985219902 | 0.772071674821638 | 1.62325466004154 | 0.57347169343217 | -1.72851030069023 | 63.6246894613877 | 65.3531997620779 | 22.1093140815969 |
| instrument_only | 2 | 12 | -2.21159382402039 | 1.29635445393169 | 3.50794827795208 | 1.23873080717102 | -13.1260780236274 | 22.8850966556704 | 36.0111746792977 | 9.35701241783805 |
| instrument_only | 3 | 16 | -2.22174524062217 | -2.1991591083274 | 0.0225861322947702 | 0.00732646114414134 | -4.46694830456123 | 20.4174860204924 | 24.8844343250536 | 6.95308526038906 |
| instrument_only | 4 | 1 | -1.97932392684506 | -1.97932392684506 | 0 | NA | 29.0887453324456 | 29.0887453324456 | 0 | NA |
| instrument_only | 5 | 12 | -1.72616205543851 | 1.17617097081338 | 2.90233302625189 | 0.837805203756018 | -13.3468176069717 | 29.5608828194392 | 42.9077004264109 | 11.4752595237954 |
| instrument_only | 6 | 19 | -2.2155969647879 | -0.829360158465982 | 1.38623680632192 | 0.38533353994777 | -9.95292621139165 | 27.2921562207821 | 37.2450824321737 | 9.28749951672546 |
| instrument_only | 7 | 7 | -1.7698073857661 | -0.390246991915282 | 1.37956039385082 | 0.453617656289558 | -14.8990140839211 | 36.7288772894966 | 51.6278913734177 | 15.4036748898376 |
| instrument_only | 8 | 27 | -2.11971419782389 | 0.726544347341938 | 2.84625854516583 | 0.833594038059209 | -14.8990140839211 | 2.06987459207157 | 16.9688886759927 | 5.26178839124252 |
| instrument_only | 9 | 68 | -2.16006804665273 | 0.409038051703458 | 2.56910609835619 | 0.733393419549063 | -14.8990140839211 | 0.467348741019265 | 15.3663628249404 | 3.88831210305508 |
| instrument_only | 10 | 37 | -0.407024269954822 | 1.7216045534449 | 2.12862882339972 | 0.591803066223717 | -14.8990140839211 | 2.27264783309537 | 17.1716619170165 | 3.31642856435554 |
| instrument_only | 11 | 4 | 1.31581153330618 | 1.66279281036203 | 0.34698127705585 | 0.146960168201645 | 64.0753614898039 | 67.016567645357 | 2.9412061555531 | 1.28367574500589 |
| instrument_only | 12 | 13 | 0.170126016858268 | 1.67025445672609 | 1.50012843986782 | 0.564577514403738 | 40.4173744253803 | 81.0761055759627 | 40.6587311505824 | 13.8367092842222 |
| instrument_only | 13 | 8 | 0.455313048356368 | 1.72768725327948 | 1.27237420492311 | 0.58177334656666 | 55.5792907532756 | 76.6256257720213 | 21.0463350187457 | 5.70471838138252 |
| instrument_only | 14 | 9 | 1.61432424074564 | 1.77028156243638 | 0.155957321690741 | 0.0535719124031742 | 13.4056159581393 | 71.3614853747201 | 57.9558694165808 | 18.9553593117256 |
| instrument_only | 15 | 8 | 0.519576378369418 | 1.7688270280655 | 1.24925064969608 | 0.419484955292958 | 9.03539901473087 | 51.3563871026949 | 42.320988087964 | 15.2211538043371 |
| instrument_only | 16 | 4 | 0.227354615410138 | 0.735985500705518 | 0.50863088529538 | 0.224691274707516 | -14.333598534694 | -10.0297447862768 | 4.30385374841714 | 1.87016207122847 |
| instrument_only | 17 | 7 | 0.974695855713068 | 1.75065468309586 | 0.77595882738279 | 0.309241586330358 | 22.2866699684821 | 46.0676336593552 | 23.7809636908731 | 7.33629548269078 |
| instrument_only | 18 | 22 | -0.203846955389752 | 0.884875907199348 | 1.0887228625891 | 0.296157279485657 | -14.8670800789279 | -0.540576432260234 | 14.3265036466676 | 3.95040621978854 |
| instrument_only | 19 | 18 | -0.236093860598022 | 0.0996336206181079 | 0.33572748121613 | 0.104026739965091 | -14.6051920321776 | 6.47153441263196 | 21.0767264448095 | 5.52787650045653 |
| instrument_only | 20 | 18 | 0.330831486741558 | 1.75597063427896 | 1.4251391475374 | 0.471678643574925 | -14.8398031319554 | 4.65520390120977 | 19.4950070331651 | 5.51420001295925 |
| instrument_only | 21 | 29 | -0.217589961534582 | 0.922123563343248 | 1.13971352487783 | 0.337821102190824 | -14.4227421717253 | -5.1554817931903 | 9.26726037853496 | 2.7443591122409 |
| instrument_only | 22 | 16 | -0.365763010064042 | 1.66467935958216 | 2.0304423696462 | 0.618308071099569 | -14.5994089190088 | -6.49858222927209 | 8.10082668973675 | 2.40834676626716 |
| instrument_only | 23 | 44 | -2.05334271214049 | 1.73416649193564 | 3.78750920407613 | 0.917836079785923 | -14.8990140839211 | 8.04724742431587 | 22.946261508237 | 5.1416190138038 |
| instrument_only | 24 | 25 | -2.21751636849689 | -1.06164923898138 | 1.15586712951551 | 0.270126507806241 | -14.8990140839211 | 4.11818599081957 | 19.0172000747407 | 4.92266595157379 |
| instrument_only | 25 | 2 | -2.2088260821244 | -1.8992907056213 | 0.3095353765031 | 0.218874563742473 | -8.00692903225135 | 18.4026607387735 | 26.4095897710248 | 18.6744000154465 |
| instrument_only | 26 | 1 | -1.60082355678106 | -1.60082355678106 | 0 | NA | -10.582928800204 | -10.582928800204 | 0 | NA |
| instrument_only | 27 | 33 | -1.40646447234651 | -0.0962973009266821 | 1.31016717141983 | 0.214477971004677 | -14.8990140839211 | 21.6673498113684 | 36.5663638952895 | 8.93882168572837 |
| instrument_only | 28 | 23 | 1.27725297071095 | 1.77677490308558 | 0.49952193237463 | 0.108307316753117 | -11.789635028031 | 46.407083749861 | 58.1967187778919 | 11.2780079315081 |
| instrument_only | 29 | 27 | 0.796637185943298 | 1.77104182946524 | 0.97440464352194 | 0.258313368180195 | -14.8990140839211 | 43.223416156469 | 58.1224302403901 | 12.2728643175785 |
| instrument_only | 30 | 2 | -1.00029297404955 | -0.877724610365692 | 0.12256836368386 | 0.0866689211197965 | 18.4573875387927 | 45.7309153131761 | 27.2735277743834 | 19.2852964361462 |
| instrument_only | 31 | 1 | 1.74658939737673 | 1.74658939737673 | 0 | NA | 12.7940798315329 | 12.7940798315329 | 0 | NA |
| instrument_only | 32 | 14 | 1.67043769853093 | 1.77632499815807 | 0.10588729962714 | 0.0280632982253082 | 2.67500390922837 | 47.8516469308443 | 45.1766430216159 | 11.9908795765426 |
| instrument_only | 33 | 29 | 1.67101992642275 | 1.77723556498407 | 0.10621563856132 | 0.0238547641269301 | -9.81017002374155 | 40.6013531278147 | 50.4115231515562 | 11.0367470170818 |
| instrument_only | 34 | 4 | 1.76220698957331 | 1.77587323140469 | 0.0136662418313804 | 0.0063910572068614 | 12.5266930410341 | 58.2899068058393 | 45.7632137648052 | 19.287824065982 |
| instrument_only | 35 | 2 | 0.992568463898048 | 1.30042720122678 | 0.30785873732873 | 0.217689000812673 | -7.28812987104306 | 26.3037698375391 | 33.5918997085821 | 23.7530600768768 |
| region_fe | 1 | 11 | 0.251422499368583 | 1.87467715941012 | 1.62325466004154 | 0.573471693432175 | -5.55416262929903 | 59.7990371327789 | 65.3531997620779 | 22.1093140815968 |
| region_fe | 2 | 12 | -1.10898833943191 | 2.39895993852017 | 3.50794827795208 | 1.23873080717102 | -16.9517303522362 | 19.0594443270616 | 36.0111746792977 | 9.35701241783805 |
| region_fe | 3 | 16 | -1.11913975603369 | -1.09655362373892 | 0.0225861322947702 | 0.00732646114414126 | -8.29260063317003 | 16.5918336918836 | 24.8844343250536 | 6.95308526038906 |
| region_fe | 4 | 1 | -0.876718442256577 | -0.876718442256577 | 0 | NA | 25.2630930038368 | 25.2630930038368 | 0 | NA |
| region_fe | 5 | 12 | -1.22099486211439 | 1.6813381641375 | 2.90233302625189 | 0.837805203756018 | -3.06810590678395 | 39.8395945196269 | 42.9077004264109 | 11.4752595237954 |
| Table truncated in rendered note; full CSV has 875 rows. |  |  |  |  |  |  |  |  |  |  |

State-by-state residual ranges

``` r
analysis_table(first_stage_state_deletion[order(abs(first_stage_state_deletion$estimate_change), decreasing = TRUE), ], "Leave-one-state-out influence", max_rows = 30)
```

| specification_id | specification | sequence | treatment | instrument | fixed_effect | control_blocks | n_controls | estimate | std.error | statistic | p.value | excluded_instrument_f | partial_r_squared | residual_instrument_sd | residual_treatment_sd | residual_correlation | instrument_variance_remaining | n | n_states | n_regions | status | reason | omitted_state | estimate_change | f_change |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.04536023915947 | 0.608357746847152 | 1.71833143339937 | 0.0864134790556959 | 2.95266291500832 | 0.0046527500425917 | 0.451041809073527 | 6.91238496655029 | 0.0682110697951087 | 0.115101756331154 | 505 | 34 | 6 | estimated | NA | 9 | 0.404812213282407 | 1.60345387571659 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.366540582729044 | 0.485152671239072 | 0.755515952932725 | 0.450285534143993 | 0.570804355135844 | 0.000887849542649693 | 0.487143535229738 | 5.99251764935849 | 0.0297968042355139 | 0.13690398021908 | 562 | 34 | 6 | estimated | NA | 1 | -0.274007443148022 | -0.778404684155883 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.884531418609376 | 0.608959399769941 | 1.45252937871317 | 0.14696581911988 | 2.10984159602486 | 0.00413235092459641 | 0.478694278524369 | 6.58677612679421 | 0.0642833642912101 | 0.134844189977506 | 561 | 34 | 6 | estimated | NA | 2 | 0.243983392732309 | 0.760632556733132 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.811860218216229 | 0.715733970864239 | 1.13430443609644 | 0.257231846420281 | 1.28664655374807 | 0.00289037200219964 | 0.447549246048394 | 6.75842066518466 | 0.0537621800358021 | 0.113108453164729 | 529 | 34 | 6 | estimated | NA | 23 | 0.171312192339162 | -0.0625624855436555 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.480972473802103 | 0.52853863846776 | 0.910004375832291 | 0.36324839157785 | 0.828107964033918 | 0.00133856283871816 | 0.486147500802096 | 6.39100118075541 | 0.0365863750420791 | 0.138115614504892 | 560 | 34 | 6 | estimated | NA | 12 | -0.159575552074964 | -0.521101075257809 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.516085172292304 | 0.520938160620375 | 0.990684137398781 | 0.322322875573981 | 0.981455060093567 | 0.00153873384311865 | 0.499805352166119 | 6.57567788875021 | 0.0392266980909382 | 0.153636781954329 | 544 | 34 | 6 | estimated | NA | 33 | -0.124462853584763 | -0.36775397919816 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.56356978479451 | 0.543043869816266 | 1.03779789464374 | 0.299849284773033 | 1.07702447012698 | 0.00175084733442709 | 0.489076807916145 | 6.58719667093322 | 0.0418431276845787 | 0.139918645723862 | 565 | 34 | 6 | estimated | NA | 13 | -0.0769782410825574 | -0.27218456916475 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.568468880977032 | 0.521464376475379 | 1.09013943544785 | 0.276178923675638 | 1.18840398871855 | 0.00185815848556228 | 0.493047784490863 | 6.50211027432719 | 0.0431063624719495 | 0.146605848132122 | 546 | 34 | 6 | estimated | NA | 29 | -0.0720791449000351 | -0.16080505057318 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.569615456080675 | 0.631548803502125 | 0.9019341861183 | 0.367527834760057 | 0.813485276088879 | 0.00159836346122615 | 0.469415884942514 | 6.68808486934522 | 0.0399795380316729 | 0.126453535201679 | 546 | 34 | 6 | estimated | NA | 8 | -0.070932569796392 | -0.535723763202847 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.710502189207658 | 0.563674508053625 | 1.26048309628376 | 0.208076082274748 | 1.58881763601709 | 0.0028570779365013 | 0.494322972208026 | 6.57075342661725 | 0.0534516411020242 | 0.149521858764732 | 554 | 34 | 6 | estimated | NA | 6 | 0.0699541633305915 | 0.239608596725366 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.578483714914436 | 0.574854246646902 | 1.00631371915352 | 0.314749487456079 | 1.01266730135658 | 0.00187968095827772 | 0.497516069422358 | 6.63828936107293 | 0.0433552875469334 | 0.149939570808186 | 550 | 34 | 6 | estimated | NA | 28 | -0.062064310962631 | -0.336541737935146 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.699350134965062 | 0.511784767073979 | 1.36649267418303 | 0.172385341618863 | 1.8673022285959 | 0.00279854708059083 | 0.494026185939345 | 6.53097954635572 | 0.0529012956418986 | 0.14594522143916 | 559 | 34 | 6 | estimated | NA | 32 | 0.0588021090879954 | 0.518093189304173 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.697054081654159 | 0.574039219104903 | 1.21429696518136 | 0.225188703659085 | 1.47451711964865 | 0.00280723618103596 | 0.49067895859659 | 6.45541895527418 | 0.0529833575855093 | 0.141683237895786 | 565 | 34 | 6 | estimated | NA | 15 | 0.0565060557770919 | 0.125308080356924 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.696028524384538 | 0.552145371181103 | 1.26058925912147 | 0.208036696670661 | 1.58908528021243 | 0.00270149650766413 | 0.495973736648217 | 6.64176511390238 | 0.0519759223839684 | 0.139438915762341 | 555 | 34 | 6 | estimated | NA | 19 | 0.0554804985074711 | 0.239876240920701 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.693329238818892 | 0.554017942347552 | 1.25145629017182 | 0.21135794932347 | 1.56614284621061 | 0.00263273250570672 | 0.497658232056348 | 6.72461368487802 | 0.0513101598682911 | 0.137991662424956 | 544 | 34 | 6 | estimated | NA | 21 | 0.0527812129418247 | 0.216933806918882 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.590900683234182 | 0.547082102345324 | 1.08009507293514 | 0.280613374360257 | 1.16660536657876 | 0.00189819028713623 | 0.490654328446144 | 6.65457391299376 | 0.043568225659726 | 0.140872590594245 | 555 | 34 | 6 | estimated | NA | 20 | -0.0496473426428851 | -0.182603672712963 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.59753286643916 | 0.532648923984554 | 1.12181371168317 | 0.262463412316236 | 1.25846600372037 | 0.00219625229318403 | 0.491451313401681 | 6.26615573125466 | 0.0468641898808023 | 0.143053608930677 | 564 | 34 | 6 | estimated | NA | 14 | -0.043015159437907 | -0.0907430355713521 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.598060988090983 | 0.551796186719705 | 1.08384400342871 | 0.278953211833765 | 1.17471782376838 | 0.00197984173512435 | 0.495073748992202 | 6.65426565900795 | 0.0444954125177387 | 0.138134786747926 | 551 | 34 | 6 | estimated | NA | 18 | -0.042487037786084 | -0.174491215523346 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.677103557527242 | 0.629563080366662 | 1.07551344518629 | 0.282676361508625 | 1.15672917077649 | 0.00235986821712568 | 0.486228794389683 | 6.77722484255781 | 0.0485784748332436 | 0.131331206405759 | 536 | 34 | 6 | estimated | NA | 10 | 0.0365555316501752 | -0.192479868515241 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.607220596958666 | 0.547205833796129 | 1.10967493300683 | 0.267654009534462 | 1.23137845694371 | 0.00211634145735997 | 0.489871344236908 | 6.46599940296906 | 0.0460037113433506 | 0.139914234712322 | 566 | 34 | 6 | estimated | NA | 7 | -0.0333274289184009 | -0.117830582348015 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.608633562506643 | 0.57487454255229 | 1.05872415188968 | 0.290244929472313 | 1.12089682979453 | 0.00203313590244549 | 0.498073311750306 | 6.72304416536261 | 0.045090308298416 | 0.143016560227478 | 540 | 34 | 6 | estimated | NA | 27 | -0.0319144633704239 | -0.228312209497197 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.672456670732533 | 0.57467275021678 | 1.17015583300038 | 0.242485856667417 | 1.3692646735048 | 0.00242424568130875 | 0.487836209258438 | 6.66269638456613 | 0.049236629467407 | 0.136669707622233 | 557 | 34 | 6 | estimated | NA | 22 | 0.0319086448554663 | 0.0200556342130771 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.656585077260133 | 0.580559892836947 | 1.13095149244927 | 0.258603944972513 | 1.27905127827323 | 0.0023323230972253 | 0.483217474210599 | 6.56960536429702 | 0.0482941310846853 | 0.135373703873901 | 561 | 34 | 6 | estimated | NA | 5 | 0.0160370513830661 | -0.070157761018496 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.653860103693436 | 0.550825139484088 | 1.18705566762231 | 0.235749750494249 | 1.40910115803425 | 0.00236532802144931 | 0.491114503842614 | 6.60270525423945 | 0.0486346380828819 | 0.141823326440371 | 566 | 34 | 6 | estimated | NA | 17 | 0.0133120778163694 | 0.0598921187425234 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.653155989472323 | 0.542080992528045 | 1.20490479923723 | 0.228799963066572 | 1.45179557522491 | 0.00240767288142689 | 0.495692914831564 | 6.59828218000912 | 0.0490680433828898 | 0.152270417055732 | 557 | 34 | 6 | estimated | NA | 3 | 0.0126079635952565 | 0.102586535933186 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.648697040302001 | 0.548590695926009 | 1.18247911442795 | 0.237552947532108 | 1.39825685605832 | 0.00233685034933572 | 0.489858765873438 | 6.57351033127072 | 0.0483409800204372 | 0.141118525110296 | 571 | 34 | 6 | estimated | NA | 25 | 0.00814901442493454 | 0.0490478167665915 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.633784924643036 | 0.557784182140734 | 1.13625474679224 | 0.256394266012245 | 1.29107484960789 | 0.00225560056159897 | 0.497084073161468 | 6.63346826921502 | 0.0474931633142755 | 0.156798867009659 | 548 | 34 | 6 | estimated | NA | 24 | -0.00676310123403123 | -0.0581341896838321 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.634810381966768 | 0.566017040685949 | 1.12153934658478 | 0.262574997398523 | 1.25785050593781 | 0.0022984168088729 | 0.490634048406956 | 6.49661768136127 | 0.0479418064831939 | 0.141671857164894 | 569 | 34 | 6 | estimated | NA | 34 | -0.0057376439102993 | -0.0913585333539191 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.63487313036113 | 0.57070446042937 | 1.11243765272744 | 0.266461088983205 | 1.23751753120574 | 0.00226224985593101 | 0.489870172323193 | 6.53879405054279 | 0.0475631144473117 | 0.140281266615707 | 571 | 34 | 6 | estimated | NA | 35 | -0.00567489551593747 | -0.111691508085989 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.637013736228626 | 0.553549336973388 | 1.15078041590943 | 0.250348446747178 | 1.32429556564069 | 0.00227015514264359 | 0.489871524470971 | 6.54942576020169 | 0.0476461450974066 | 0.140151480257875 | 571 | 34 | 6 | estimated | NA | 30 | -0.00353428964844116 | -0.0249134736510408 |
| Table truncated in rendered note; full CSV has 35 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Leave-one-state-out influence

``` r
analysis_table(first_stage_district_influence[order(first_stage_district_influence$cooks_distance, decreasing = TRUE), ], "Most influential districts in the expanded first stage", max_rows = 30)
```

| state_code_2001 | district_code_2001 | leverage | cooks_distance | studentized_residual | instrument_dfbeta |
|:---|:---|:---|:---|:---|:---|
| 35 | 1 | 0.538090329737038 | 0.209519859870667 | 2.99109331594167 | 0.00567489551594094 |
| 35 | 2 | 0.538090329737038 | 0.209519859870666 | -2.99109331594167 | 0.005674895515941 |
| 30 | 1 | 0.502240505677491 | 0.147034100425157 | -2.68797688535658 | 0.003534289648452 |
| 30 | 2 | 0.502240505677491 | 0.147034100425157 | 2.68797688535657 | 0.00353428964845226 |
| 34 | 3 | 0.29516307301641 | 0.119437799347976 | 3.78562451421137 | -0.00561808490280292 |
| 14 | 1 | 0.127683972409338 | 0.112290125910298 | 6.35751308041319 | -0.00586775978107184 |
| 25 | 2 | 0.540840317994956 | 0.0809344301313283 | 1.83906456659715 | -0.00814901442494214 |
| 25 | 1 | 0.540840317994955 | 0.080934430131328 | -1.83906456659715 | -0.00814901442494238 |
| 1 | 12 | 0.116620690537195 | 0.0789030204883662 | -5.56422192367221 | 0.0799141976948671 |
| 34 | 1 | 0.300093783509674 | 0.0730058865855134 | -2.9089677674344 | 0.0254208749035548 |
| 1 | 9 | 0.119869126338463 | 0.0658114262338778 | 4.9750163348057 | -0.0435342390832671 |
| 7 | 2 | 0.152214547133261 | 0.0586001521053615 | -4.05768401705154 | 0.037900515049122 |
| 1 | 10 | 0.144540719406203 | 0.0497317012081583 | 3.8473788825101 | -0.0252212389226669 |
| 15 | 8 | 0.157105924114544 | 0.0466950452638215 | 3.54206475924403 | 0.0119780613459828 |
| 7 | 4 | 0.149809798190035 | 0.0379072939409814 | 3.27676624768501 | 0.0299390945440826 |
| 14 | 2 | 0.150071464192017 | 0.0357550127695975 | -3.17720446233958 | 0.0390317703467473 |
| 12 | 6 | 0.0997196350068979 | 0.0320814460533775 | 3.81567280787858 | 0.0917940472355212 |
| 1 | 5 | 0.13524714507256 | 0.0295106137543799 | 3.06492568527572 | 0.0914801118992478 |
| 1 | 14 | 0.175902696255641 | 0.026184717182233 | -2.46357380030907 | 0.0924279496535883 |
| 1 | 1 | 0.122850340002193 | 0.0249976497479466 | -2.97945144901987 | -0.0362344302295137 |
| 15 | 7 | 0.205730667243197 | 0.0249529094451183 | 2.18043558774969 | -0.0897100465290975 |
| 1 | 6 | 0.115886637646315 | 0.0232929456345275 | 2.97283968519047 | 0.0586460642195708 |
| 32 | 10 | 0.100712909427964 | 0.0224040593668981 | 3.15757635966706 | -0.0130661640035522 |
| 14 | 5 | 0.173532848923126 | 0.0216303264722026 | -2.25547852998507 | -0.0303483453993635 |
| 5 | 5 | 0.123750432117897 | 0.0201983059448016 | 2.6625941845049 | -0.0887885192205904 |
| 1 | 13 | 0.198510941722141 | 0.0196819055194565 | -1.97875955364339 | 0.111326595411678 |
| 29 | 20 | 0.0937891297154694 | 0.0189081058447679 | 3.01500080103961 | 0.0205058942300132 |
| 6 | 8 | 0.0595607796686901 | 0.018294848694735 | 3.81047564062454 | -0.0195896088530533 |
| 28 | 5 | 0.179660088698327 | 0.0174292975163325 | 1.98023321626959 | 0.0502225148751727 |
| 11 | 4 | 0.261132968349567 | 0.016994726950042 | -1.53699074653729 | -0.00253572964627045 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |

Most influential districts in the expanded first stage

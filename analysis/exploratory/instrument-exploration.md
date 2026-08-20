# Instrument Exploration


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Current instrument and treatment diagnostics

The active construction uses the full mutually exclusive Census 2001
C-16 mother-tongue distribution. Language-group subtotal rows are
removed before aggregation, linguistic distances are attached from the
documented Shastry concordance, and unmapped speaker mass remains
visible. The preferred public scalar is `ling_distance_nonzero_mean`,
the speaker-weighted mean distance among mapped languages with distance
greater than zero. Hindi and Urdu are treated as distance zero and their
shares are reported separately.

The preferred public treatment is `emi_exposure_all_children_0708`, the
survey-weighted share of all children ages 5-19 who are both enrolled
and studying in English medium. The historical `EMIE` field remains a
compatibility measure of EMI among enrolled children but is not used in
the current district-level dotplot.

The tables below diagnose how the preferred exploratory first stage
changes with six-region fixed effects, state fixed effects, main and
expanded Census control sets, sequential thematic blocks, VIF/GVIF,
state deletion, and district influence. These diagnostics do not change
the public IV specification.

``` r
analysis_deviation_note("The active note describes the same all-child EMI exposure and full-distribution linguistic-distance scalar used by the public IV specification. Historical comments are retained only as provenance.")
```

**Deviation note.** The active note describes the same all-child EMI
exposure and full-distribution linguistic-distance scalar used by the
public IV specification. Historical comments are retained only as
provenance.

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
iv_diagnostic_applicability <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "iv_diagnostic_applicability.csv")
iv_specification_registry <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "iv_specification_registry.csv")
iv_overidentification <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "iv_overidentification.csv")
iv_monotonicity <- analysis_target_csv("diag_ext_alternative_distance_first_stages", "iv_monotonicity_summary.csv")
iv_joint_balance <- analysis_target_csv("diag_ext_census_2001_controls", "instrument_balance_joint.csv")
```

## District-level treatment and instrument check

The district-level dotplot uses 573 active panel rows and the same
all-child EMI exposure used in the public IV specification. The
accompanying table keeps the preferred linguistic-distance scalar
visible alongside the treatment.

``` r
instrument_dotplot_rows <- iv_dotplot[, intersect(c("district_order", "district_code", "state", "district", "emi_exposure_all_children_0708", "ling_distance_nonzero_mean", "state_prefix"), names(iv_dotplot)), drop = FALSE]
exposure_scale_summary <- data.frame(
  current_scale = "0-100 percentage scale",
  min_exposure = min(iv_dotplot$emi_exposure_all_children_0708, na.rm = TRUE),
  median_exposure = stats::median(iv_dotplot$emi_exposure_all_children_0708, na.rm = TRUE),
  max_exposure = max(iv_dotplot$emi_exposure_all_children_0708, na.rm = TRUE)
)
analysis_table(exposure_scale_summary, "All-child EMI-exposure scale check")
```

| current_scale          | min_exposure | median_exposure | max_exposure |
|:-----------------------|-------------:|----------------:|-------------:|
| 0-100 percentage scale |            0 |           6.232 |       95.975 |

All-child EMI-exposure scale check

``` r
analysis_table(instrument_dotplot_rows, "Current treatment and instrument by district", max_rows = 30)
```

| district_order | district_code | state | district | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state_prefix |
|:---|:---|:---|:---|:---|:---|:---|
| 1 | 1113 | Jammu & Kashmir | Jammu | 48.3960192416473 | 2.37106387927776 | 1 |
| 2 | 1114 | Jammu & Kashmir | Kathus | 33.5921008544035 | 2.71284727930298 | 1 |
| 3 | 1209 | Jammu & Kashmir | Doda | 78.5237035453088 | 3.9943185393193 | 1 |
| 4 | 1210 | Jammu & Kashmir | Udhampur | 72.6236337161995 | 3.68266891069835 | 1 |
| 5 | 1212 | Jammu & Kashmir | Rajauri | 13.1705037832309 | 3.51482393235577 | 1 |
| 6 | 1301 | Jammu & Kashmir | Kupwara | 31.8657493187537 | 3.98010042245708 | 1 |
| 7 | 1302 | Jammu & Kashmir | Baramula | 39.0896349502222 | 3.98043759195197 | 1 |
| 8 | 1303 | Jammu & Kashmir | Srinagar | 73.6252335167284 | 3.96930579486673 | 1 |
| 9 | 1304 | Jammu & Kashmir | Badgam | 50.3440453344655 | 3.98018342125992 | 1 |
| 10 | 1305 | Jammu & Kashmir | Pulwama | 73.7864106740856 | 3.96122222060843 | 1 |
| 11 | 1306 | Jammu & Kashmir | Anantnag | 72.0857411304068 | 3.98303814819404 | 1 |
| 12 | 2102 | Himachal Pradesh | Kangra | 21.4320768227842 | 2.25314792510676 | 2 |
| 13 | 2104 | Himachal Pradesh | Kullu | 1.77293606029376 | 3.95511276549157 | 2 |
| 14 | 2105 | Himachal Pradesh | Mandi | 11.914923634052 | 1.86593155893536 | 2 |
| 15 | 2106 | Himachal Pradesh | Hamirpur | 15.9566958650642 | 1.72932330827068 | 2 |
| 16 | 2107 | Himachal Pradesh | Una | 13.2443116562273 | 1.01657940663176 | 2 |
| 17 | 2201 | Himachal Pradesh | Chamba | 9.99246785718507 | 2.68250377073906 | 2 |
| 18 | 2203 | Himachal Pradesh | Lahul & Spiti | 5.80506231778363 | 4.51860131842935 | 2 |
| 19 | 2208 | Himachal Pradesh | Bilaspur | 15.5006331251223 | 1.01065304047727 | 2 |
| 20 | 2209 | Himachal Pradesh | Solan | 14.0781014588688 | 1.48349134909802 | 2 |
| 21 | 2210 | Himachal Pradesh | Sirmapur | 14.511327515228 | 1.53199712385404 | 2 |
| 22 | 2211 | Himachal Pradesh | Shimla | 37.7841107395915 | 1.92892068557311 | 2 |
| 23 | 2212 | Himachal Pradesh | Kinnaur | 4.0754499392593 | 4.27128302538422 | 2 |
| 24 | 3101 | Punjab | Gurdaspur | 25.9349887876681 | 1.02308775617026 | 3 |
| 25 | 3102 | Punjab | Amritsar | 21.8804829341996 | 1.00801524327818 | 3 |
| 26 | 3103 | Punjab | Kapurthala | 35.3165001044135 | 1.00845349867139 | 3 |
| 27 | 3104 | Punjab | Jalandhar | 31.317678667039 | 1.01759751041724 | 3 |
| 28 | 3106 | Punjab | Nawanshahr | 28.3822720514065 | 1.00441976088141 | 3 |
| 29 | 3107 | Punjab | Rupnagar | 30.4238156299204 | 1.01258610050619 | 3 |
| 30 | 3208 | Punjab | Fatehgarh Sahib | 19.5810353849101 | 1.00393552438769 | 3 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |  |

Current treatment and instrument by district

``` r
analysis_image("diag_ext_instrument_exploration", "emie_by_district_dotplot.png", "All-child EMI exposure by district")
```

![All-child EMI exposure by
district](../../outputs/diagnostics/extended/instrument_exploration/emie_by_district_dotplot.png)

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

| state | n_rows | mean_emi_exposure_all_children_0708 | mean_ling_distance_nonzero_mean | mean_npeople_0708 | mean_consumption_0708 | mean_dependency_ratio |
|:---|:---|:---|:---|:---|:---|:---|
| Andaman & Nicobar Islands | 2 | 24.4068340671691 | 4.36874469706007 | 114699.0625 | 1716.59345353575 | 39.9965239648858 |
| Andhra Pradesh | 13 | 17.50329470393 | 4.979381417228 | 3406138.48653846 | 806.038478947683 | 46.8784465840865 |
| Arunachal Pradesh | 13 | 72.0167035742077 | 4.36561188090261 | 76279.7523076923 | 821.925150923097 | 61.0798056117889 |
| Assam | 22 | 4.05362607555326 | 3.25842757278221 | 1070667.90886364 | 762.436038575419 | 58.0450186320867 |
| Bihar | 37 | 2.27384573509356 | 3.4504348031418 | 2051830.32918919 | 559.316304711329 | 84.7022490469553 |
| Chandigarh | 1 | 43.9877594163667 | 1.2429229376526 | 837516.24 | 2237.62405162436 | 36.7830442420546 |
| Chhattisgarh | 16 | 2.44891352706489 | 3.7062821508302 | 1455306.785 | 519.343724642298 | 59.4295625174172 |
| Dadra & Nagar Haveli | 1 | 4.31608528371716 | 1.6214233077166 | 204832.69 | 878.890652903109 | 55.2790526851351 |
| Daman & Diu | 2 | 20.0968799371822 | 1.16818847062481 | 69690.93 | 1391.7018118573 | 51.6253036614033 |
| Delhi | 7 | 26.6421146024289 | 2.19595583855424 | 1802076.03428571 | 1274.45536408576 | 51.9338015790853 |
| Goa | 2 | 46.9931655099055 | 2.28323807229004 | 698016.255 | 1277.6412702353 | 35.7209708999027 |
| Gujarat | 25 | 3.29157078968408 | 1.13380993069443 | 2038854.0708 | 858.757699979292 | 53.6880918495208 |
| Haryana | 19 | 15.4550600433681 | 1.3168094426959 | 1142009.75631579 | 974.810008859313 | 53.1686777255239 |
| Himachal Pradesh | 12 | 13.8390080826217 | 2.35396210649927 | 519885.804583333 | 932.211852792387 | 53.7694561219673 |
| Jammu & Kashmir | 11 | 53.3729796423138 | 3.64818274002658 | 735654.508181818 | 917.231422452719 | 52.1863851221851 |
| Jharkhand | 18 | 4.87131622223234 | 4.4326828791716 | 1395211.16555556 | 632.040527265219 | 67.2780276872427 |
| Karnataka | 27 | 12.5522398616991 | 4.81990590632138 | 1844944.73574074 | 753.91460740765 | 49.7504147543725 |
| Kerala | 14 | 37.8847415252017 | 4.9854378745229 | 2129849.7675 | 1061.81342032626 | 48.0392820929302 |
| Lakshadweep | 1 | 27.693093915454 | 4.96883626187439 | 57165.375 | 1258.92789866243 | 47.3127675768181 |
| Madhya Pradesh | 44 | 4.8124959319002 | 2.85251318024406 | 1333711.54318182 | 598.39039835291 | 63.7789027840119 |
| Maharashtra | 33 | 8.0006130127011 | 2.13576340235499 | 2757972.46242424 | 790.791751568391 | 52.2534100830016 |
| Manipur | 9 | 54.3931471021197 | 4.94684335092403 | 220217.051666667 | 819.916168102988 | 50.0165633266468 |
| Meghalaya | 7 | 46.5401931358657 | 4.73938076756495 | 325151.725 | 889.075788565463 | 63.5951262270377 |
| Mizoram | 8 | 43.4928512499453 | 4.71618566629543 | 104185.15875 | 1154.1292730263 | 61.7142451234391 |
| Nagaland | 8 | 80.5294327657661 | 4.39772043730242 | 118572.34625 | 1183.08745343988 | 41.5966283338895 |
| Odisha | 29 | 4.48207369404672 | 3.22899303105019 | 1205721.18155172 | 530.726549256539 | 53.2755205864839 |
| Puducherry | 4 | 46.1645721489379 | 4.99375031211973 | 207733.7475 | 1172.67173441507 | 44.4357277807352 |
| Punjab | 16 | 22.8096522536849 | 1.00981642560685 | 1452066.4490625 | 1116.02721196292 | 50.1406819071539 |
| Rajasthan | 27 | 3.67185054785357 | 2.62807544017798 | 1755742.01925926 | 730.020097099085 | 70.0862133128767 |
| Sikkim | 4 | 80.4287978097929 | 4.71958237654858 | 129015.62625 | 773.645294969817 | 55.1816987031961 |
| Table truncated in rendered note; full CSV has 36 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | var | label | N | Min | 1Q | Med | 3Q | Max | Mean | SD | desc |
|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Treatment and instrument | ling_distance_nonzero_mean | ling_distance_nonzero_mean | Linguistic distance | 573 | 1.00 | 2.14 | 3.13 | 4.55 | 5.00 | 3.22 | 1.31 | Population-weighted mean linguistic distance among mapped speakers with positive distance from Hindi |
| Treatment and instrument | emi_exposure_all_children_0708 | emi_exposure_all_children_0708 | EMI exposure | 573 | 0.00 | 1.42 | 6.23 | 17.91 | 95.98 | 14.90 | 20.35 | Share of children ages 5-19 enrolled in English-medium instruction |
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
| legacy_emie_dotplot | The historical code plotted EMI among enrolled children by district code. | the current plot uses all-child EMI exposure from the active district panel |
| legacy_peak_comment | Legacy notes described high EMI-among-enrolled values in several geographically distant regions. | use current treatment and instrument diagnostics rather than the legacy visual impression |
| smaller_units_question | Legacy comments asked whether smaller units of analysis would be useful. | retained as exploratory rationale, not a final-paper claim |
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

| specification_id | adjustment_id | construction_id | estimate_2sls | std_error_clustered | p_value_clustered | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_left_truncated | ar_95_right_truncated | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.00545970878953654 | 0.00257226663682176 | 0.0342231307625105 | 4.65587363637861 | 0.0313638018823993 | 4.65587363637859 | 0.0313638018823997 | -0.0137797965628218 | -0.00110238372502575 | FALSE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_distant_share | unadjusted | distant_share | -0.00367467297438497 | 0.00287636270123687 | 0.201930281348538 | 1.6123215882087 | 0.204682994047374 | 1.6123215882087 | 0.204682994047374 | -0.0209452907754891 | 0.00330715117507724 | FALSE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | -0.00403338438986888 | 0.00164248171692293 | 0.0143593117148593 | 3.8462485719563 | 0.0503429639929909 | 3.84624857195631 | 0.0503429639929908 | -0.00716549421266734 | 0 | FALSE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | -0.00598475166235261 | 0.00396078158372087 | 0.13134192427113 | 2.54242381902217 | 0.111378841554435 | 2.54242381902219 | 0.111378841554434 | -0.0292131687131822 | 0.00165357558753862 | FALSE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | -0.006125134844008 | 0.00378657513379503 | 0.10630467591649 | 2.84312776165345 | 0.0923136223401577 | 2.84312776165347 | 0.0923136223401571 | -0.0237012500880535 | 0.00110238372502575 | FALSE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | -0.000946604817748755 | 0.00131814907899312 | 0.472969721541804 | 20.2504929831194 | 1.29318327426803e-18 | 20.2504929831194 | 1.29318327426805e-18 | NA | NA | TRUE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | -0.00143221637917461 | 0.00107745987727014 | 0.18429641468844 | 25.691795031304 | 2.13167577807892e-23 | 25.691795031304 | 2.13167577807883e-23 | NA | NA | TRUE | FALSE | FALSE | 573 | estimated | NA |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | -0.00233557838082002 | 0.00128897126099754 | 0.0705158718084157 | 24.8262213634502 | 1.1895093063821e-22 | 24.8262213634503 | 1.18950930638197e-22 | NA | NA | TRUE | FALSE | FALSE | 573 | estimated | NA |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | -0.00608807310689442 | 0.0057224145342319 | 0.287836126187689 | 1.25642609253422 | 0.262812570705136 | 1.25642609253425 | 0.26281257070513 | -0.110238372502574 | 0.110238372502574 | FALSE | TRUE | TRUE | 573 | estimated | NA |
| region_main\_\_distant_share | region_main | distant_share | -0.0332801226865113 | 0.065064645604641 | 0.609208828819868 | 2.2348130120196 | 0.1355002744322 | 2.23481301201962 | 0.135500274432198 | -0.110238372502574 | 0.110238372502574 | FALSE | TRUE | TRUE | 573 | estimated | NA |
| Table truncated in rendered note; full CSV has 58 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Weak-IV-aware exploratory outcome estimates

``` r
analysis_table(iv_diagnostic_applicability, "IV diagnostic applicability and implementation status", max_rows = 30)
```

| specification_id | diagnostic_id | diagnostic_family | applicable | implemented | will_run | reason |
|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_distant_share | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_top3_legacy | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_nonzero_mean_hindi_urdu | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| Table truncated in rendered note; full CSV has 406 rows. |  |  |  |  |  |  |

IV diagnostic applicability and implementation status

``` r
analysis_table(iv_joint_balance, "Joint holdout-covariate balance tests", max_rows = 30)
```

| specification_id | adjustment_id | construction_id | fixed_effect | instrument | tested_covariates | n_tested_covariates | joint_f | joint_p | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 8.83323877240575 | 1.44838729808545e-16 | 573 | estimated |  |
| unadjusted\_\_distant_share | unadjusted | distant_share | none | ling_share_distance_ge3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 30.8537646841817 | 1.97176549729579e-57 | 573 | estimated |  |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | none | ling_distance_top3_legacy | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 24.6794883484925 | 3.71940950497086e-47 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.66791255103196 | 8.44043313962893e-10 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.47960709008315 | 2.13264454228488e-09 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | none | ling_share_distance_1 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.648908456202748 | 0.81275558494665 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | none | ling_share_distance_2 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.422627560123302 | 0.961866526153158 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | none | ling_share_distance_3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 2.35373224632415 | 0.00458385823485709 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | none | ling_share_distance_4 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.541865409951055 | 0.898566715489814 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | none | ling_share_distance_5 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 3.26528539575487 | 8.47466320797693e-05 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | none | ling_share_distance_1 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.686497199566838 | 0.777399187104907 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | none | ling_share_distance_2 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.421815425942203 | 0.962173268233161 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | none | ling_share_distance_3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 2.31975952100607 | 0.00527444866973593 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | none | ling_share_distance_4 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.524058598500274 | 0.91030401232382 | 573 | estimated |  |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | none | ling_share_distance_5 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 3.35624377344472 | 5.59048525561593e-05 | 573 | estimated |  |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | none | ling_mapped_share_distance_1 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.71636039627468 | 0.74786091062149 | 573 | estimated |  |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | none | ling_mapped_share_distance_2 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.486059504030178 | 0.932692600724243 | 573 | estimated |  |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | none | ling_mapped_share_distance_3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 2.70455455534158 | 0.00103482433408083 | 573 | estimated |  |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | none | ling_mapped_share_distance_4 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 0.448847542452949 | 0.951041244890679 | 573 | estimated |  |
| unadjusted\_\_distance_shares_mapped | unadjusted | distance_shares_mapped | none | ling_mapped_share_distance_5 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 4.47871241610951 | 2.77789052867285e-07 | 573 | estimated |  |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 6.53260037358611 | 3.84760693475442e-05 | 573 | estimated |  |
| region_main\_\_distant_share | region_main | distant_share | region | ling_share_distance_ge3 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 1.76493285824673 | 0.134428169297865 | 573 | estimated |  |
| region_main\_\_top3_legacy | region_main | top3_legacy | region | ling_distance_top3_legacy | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 0.052746202516678 | 0.994795789219202 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 4.91078676013482 | 0.000674696954346619 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 5.29895492246109 | 0.000341196638480112 | 573 | estimated |  |
| region_main\_\_distance_shares_all | region_main | distance_shares_all | region | ling_share_distance_1 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 1.28696562897651 | 0.273888937465985 | 573 | estimated |  |
| region_main\_\_distance_shares_all | region_main | distance_shares_all | region | ling_share_distance_2 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 1.20289259402415 | 0.308503140375853 | 573 | estimated |  |
| region_main\_\_distance_shares_all | region_main | distance_shares_all | region | ling_share_distance_3 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 1.87783996147697 | 0.112846372326206 | 573 | estimated |  |
| region_main\_\_distance_shares_all | region_main | distance_shares_all | region | ling_share_distance_4 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 1.07984139645791 | 0.365628529127096 | 573 | estimated |  |
| region_main\_\_distance_shares_all | region_main | distance_shares_all | region | ling_share_distance_5 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 0.848831161961156 | 0.494634404818198 | 573 | estimated |  |
| Table truncated in rendered note; full CSV has 94 rows. |  |  |  |  |  |  |  |  |  |  |  |

Joint holdout-covariate balance tests

``` r
analysis_table(iv_specification_registry, "De-duplicated IV diagnostic specification registry", max_rows = 30)
```

| specification_id | adjustment_id | adjustment | construction_id | construction | outcome | treatment | fixed_effect | controls | included_language_controls | excluded_instruments | n_endogenous | n_excluded_instruments | panel_variant | sample_rule | cluster | tier | sequence |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 1 |
| unadjusted\_\_distant_share | unadjusted | Unadjusted | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_share_distance_ge3 | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 2 |
| unadjusted\_\_top3_legacy | unadjusted | Unadjusted | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_top3_legacy | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 3 |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | Unadjusted | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 4 |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | Unadjusted | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_share;urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 5 |
| unadjusted\_\_distance_shares_all | unadjusted | Unadjusted | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 6 |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | Unadjusted | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | ling_unmapped_speaker_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 7 |
| unadjusted\_\_distance_shares_mapped | unadjusted | Unadjusted | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 8 |
| region_main\_\_nonzero_mean | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 9 |
| region_main\_\_distant_share | region_main | Six-region FE + main controls | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_ge3 | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 10 |
| region_main\_\_top3_legacy | region_main | Six-region FE + main controls | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_top3_legacy | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 11 |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 12 |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_share;urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 13 |
| region_main\_\_distance_shares_all | region_main | Six-region FE + main controls | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 14 |
| region_main\_\_distance_shares_all_unmapped | region_main | Six-region FE + main controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | ling_unmapped_speaker_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 15 |
| region_main\_\_distance_shares_mapped | region_main | Six-region FE + main controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 16 |
| region_expanded\_\_nonzero_mean | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 |  | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 17 |
| region_expanded\_\_distant_share | region_expanded | Six-region FE + expanded controls | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 |  | ling_share_distance_ge3 | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 18 |
| region_expanded\_\_top3_legacy | region_expanded | Six-region FE + expanded controls | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 |  | ling_distance_top3_legacy | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 19 |
| region_expanded\_\_nonzero_mean_hindi_urdu | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | hindi_urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 20 |
| region_expanded\_\_nonzero_mean_hindi_urdu_separate | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | hindi_share;urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 21 |
| region_expanded\_\_distance_shares_all | region_expanded | Six-region FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 22 |
| region_expanded\_\_distance_shares_all_unmapped | region_expanded | Six-region FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unmapped share controlled | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | ling_unmapped_speaker_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 23 |
| region_expanded\_\_distance_shares_mapped | region_expanded | Six-region FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 24 |
| state_main\_\_nonzero_mean | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 25 |
| state_main\_\_distant_share | state_main | State FE + main controls | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_ge3 | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 26 |
| state_main\_\_top3_legacy | state_main | State FE + main controls | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_top3_legacy | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 27 |
| state_main\_\_nonzero_mean_hindi_urdu | state_main | State FE + main controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 28 |
| state_main\_\_nonzero_mean_hindi_urdu_separate | state_main | State FE + main controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_share;urdu_share | ling_distance_nonzero_mean | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 29 |
| state_main\_\_distance_shares_all | state_main | State FE + main controls | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | state | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | A | 30 |
| Table truncated in rendered note; full CSV has 58 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

De-duplicated IV diagnostic specification registry

``` r
analysis_table(
  iv_overidentification[iv_overidentification$status != "not_applicable", ],
  "Overidentifying-restrictions diagnostics for multi-instrument specifications",
  max_rows = 30
)
```

| specification_id | n_endogenous | n_excluded_instruments | test | status | statistic | df | p.value | reason |
|:---|---:|---:|:---|:---|---:|---:|---:|:---|
| unadjusted\_\_distance_shares_all | 1 | 5 | sargan | estimated | 12.751 | 4 | 0.013 | NA |
| unadjusted\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 14.955 | 4 | 0.005 | NA |
| unadjusted\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 17.369 | 4 | 0.002 | NA |
| region_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 12.063 | 4 | 0.017 | NA |
| region_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 12.557 | 4 | 0.014 | NA |
| region_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 12.446 | 4 | 0.014 | NA |
| region_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 13.324 | 4 | 0.010 | NA |
| region_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 15.000 | 4 | 0.005 | NA |
| region_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 15.065 | 4 | 0.005 | NA |
| state_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 16.412 | 4 | 0.003 | NA |
| state_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 15.318 | 4 | 0.004 | NA |
| state_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 10.095 | 4 | 0.039 | NA |
| state_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 15.207 | 4 | 0.004 | NA |
| state_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 14.479 | 4 | 0.006 | NA |
| state_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 9.332 | 4 | 0.053 | NA |

Overidentifying-restrictions diagnostics for multi-instrument
specifications

``` r
analysis_table(
  iv_monotonicity[iv_monotonicity$adjustment_id %in% c("region_main", "state_main", "state_expanded"), ],
  "Residualized scalar first-stage shape diagnostics",
  max_rows = 30
)
```

| specification_id | adjustment_id | construction_id | instrument | fixed_effect | linear_slope | spearman_rho | isotonic_r_squared | n_bins | share_nondecreasing_bin_steps | n_negative_bin_steps | n_state_slopes | share_negative_state_slopes | n | status | reason |
|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|
| region_main\_\_nonzero_mean | region_main | nonzero_mean | ling_distance_nonzero_mean | region | 3.628 | 0.146 | 0.053 | 10 | 0.778 | 2 | 26 | 0.308 | 573 | estimated | NA |
| region_main\_\_distant_share | region_main | distant_share | ling_share_distance_ge3 | region | -0.038 | -0.052 | 0.023 | 10 | 0.556 | 4 | 26 | 0.423 | 573 | estimated | NA |
| region_main\_\_top3_legacy | region_main | top3_legacy | ling_distance_top3_legacy | region | 4.494 | 0.301 | 0.121 | 10 | 0.667 | 3 | 26 | 0.385 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | region | 4.281 | 0.188 | 0.081 | 10 | 0.778 | 2 | 26 | 0.269 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | region | 4.296 | 0.176 | 0.081 | 10 | 0.778 | 2 | 26 | 0.308 | 573 | estimated | NA |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | ling_distance_nonzero_mean | state | 0.514 | 0.037 | 0.011 | 10 | 0.556 | 4 | 26 | 0.308 | 573 | estimated | NA |
| state_main\_\_distant_share | state_main | distant_share | ling_share_distance_ge3 | state | 0.031 | 0.030 | 0.026 | 10 | 0.556 | 4 | 26 | 0.423 | 573 | estimated | NA |
| state_main\_\_top3_legacy | state_main | top3_legacy | ling_distance_top3_legacy | state | 1.441 | 0.052 | 0.057 | 10 | 0.556 | 4 | 26 | 0.423 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu | state_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.578 | 0.037 | 0.017 | 10 | 0.444 | 5 | 26 | 0.346 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu_separate | state_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | state | 0.535 | 0.040 | 0.010 | 10 | 0.444 | 5 | 26 | 0.423 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | ling_distance_nonzero_mean | state | 0.641 | 0.031 | 0.011 | 10 | 0.556 | 4 | 26 | 0.385 | 573 | estimated | NA |
| state_expanded\_\_distant_share | state_expanded | distant_share | ling_share_distance_ge3 | state | 0.030 | 0.011 | 0.025 | 10 | 0.444 | 5 | 26 | 0.538 | 573 | estimated | NA |
| state_expanded\_\_top3_legacy | state_expanded | top3_legacy | ling_distance_top3_legacy | state | 1.364 | 0.050 | 0.059 | 10 | 0.444 | 5 | 26 | 0.462 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_hindi_urdu | state_expanded | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.684 | 0.032 | 0.012 | 10 | 0.556 | 4 | 26 | 0.385 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_hindi_urdu_separate | state_expanded | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | state | 0.615 | 0.030 | 0.011 | 10 | 0.667 | 3 | 26 | 0.346 | 573 | estimated | NA |

Residualized scalar first-stage shape diagnostics

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
| ling_distance_nonzero_mean | model_regressors | 1 | 4.19094118596821 | 4.19094118596821 | 2.04717883585392 | estimated | NA | region_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 2.4396564717468 | 2.4396564717468 | 1.56193997059644 | estimated | NA | region_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 3.25621839824172 | 3.25621839824172 | 1.80449948690536 | estimated | NA | region_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.07903451652194 | 4.07903451652194 | 2.01966198075865 | estimated | NA | region_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 2.6474332899362 | 2.6474332899362 | 1.62709350989309 | estimated | NA | region_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 3.19436972218934 | 3.19436972218934 | 1.78727997867971 | estimated | NA | region_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 1.66461759709651 | 1.66461759709651 | 1.29020060343208 | estimated | NA | region_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 4.45908681339413 | 4.45908681339413 | 2.11165499393109 | estimated | NA | region_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 3.88702831941567 | 3.88702831941567 | 1.97155479746713 | estimated | NA | region_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.53696374496203 | 4.53696374496203 | 2.13001496355355 | estimated | NA | region_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 3.64860496258455 | 3.64860496258455 | 1.91013218458424 | estimated | NA | region_fe_census_controls |
| factor(region) | model_regressors | 5 | NA | 32.6554766388361 | 1.41708402667471 | estimated | NA | region_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 4.43089985336262 | 4.43089985336262 | 2.10497027374797 | estimated | NA | region_fe_expanded_controls |
| log_population_2001 | model_regressors | 1 | 2.48046664834599 | 2.48046664834599 | 1.57494972883137 | estimated | NA | region_fe_expanded_controls |
| urban_share_2001 | model_regressors | 1 | 3.28580468067791 | 3.28580468067791 | 1.81267886860246 | estimated | NA | region_fe_expanded_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.8557941295908 | 4.8557941295908 | 2.20358665125536 | estimated | NA | region_fe_expanded_controls |
| sc_share_2001 | model_regressors | 1 | 2.73691673166293 | 2.73691673166293 | 1.65436293831279 | estimated | NA | region_fe_expanded_controls |
| st_share_2001 | model_regressors | 1 | 3.23989043615114 | 3.23989043615114 | 1.79996956534024 | estimated | NA | region_fe_expanded_controls |
| muslim_share_2001 | model_regressors | 1 | 1.86466857451151 | 1.86466857451151 | 1.36552867949066 | estimated | NA | region_fe_expanded_controls |
| dependency_ratio_2001 | model_regressors | 1 | 5.29570665734403 | 5.29570665734403 | 2.30124024329144 | estimated | NA | region_fe_expanded_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.93331643527214 | 4.93331643527214 | 2.22110702922487 | estimated | NA | region_fe_expanded_controls |
| log_population_density_2001 | model_regressors | 1 | 3.78734443538462 | 3.78734443538462 | 1.94611007792073 | estimated | NA | region_fe_expanded_controls |
| literacy_share_2001 | model_regressors | 1 | 3.28597929786627 | 3.28597929786627 | 1.81272703346816 | estimated | NA | region_fe_expanded_controls |
| worker_share_2001 | model_regressors | 1 | 3.53430183205488 | 3.53430183205488 | 1.87997389132267 | estimated | NA | region_fe_expanded_controls |
| cultivator_share_workers_2001 | model_regressors | 1 | 5.19801994309921 | 5.19801994309921 | 2.27991665266501 | estimated | NA | region_fe_expanded_controls |
| agricultural_labourer_share_workers_2001 | model_regressors | 1 | 3.81842927354898 | 3.81842927354898 | 1.95408016047167 | estimated | NA | region_fe_expanded_controls |
| factor(region) | model_regressors | 5 | NA | 55.5491506570227 | 1.49440303981321 | estimated | NA | region_fe_expanded_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 6.93786515963915 | 6.93786515963915 | 2.63398275613929 | estimated | NA | state_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 5.61993575138572 | 5.61993575138572 | 2.37064036736611 | estimated | NA | state_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 5.96091872436052 | 5.96091872436052 | 2.44149927797665 | estimated | NA | state_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 8.0967736081637 | 8.0967736081637 | 2.84548301842828 | estimated | NA | state_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 4.12756112485299 | 4.12756112485299 | 2.03164000867599 | estimated | NA | state_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 5.54013209347623 | 5.54013209347623 | 2.35374851959087 | estimated | NA | state_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 3.46112413849601 | 3.46112413849601 | 1.86040966953411 | estimated | NA | state_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 7.72526491195113 | 7.72526491195113 | 2.77943607804733 | estimated | NA | state_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 7.72895950960147 | 7.72895950960147 | 2.78010062940201 | estimated | NA | state_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 10.442166139894 | 10.442166139894 | 3.23143406862867 | estimated | NA | state_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 6.9473535802674 | 6.9473535802674 | 2.63578329539198 | estimated | NA | state_fe_census_controls |
| factor(state_code_2001) | model_regressors | 34 | NA | 64373.4820786063 | 1.17683690567705 | estimated | NA | state_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.14809303270603 | 7.14809303270603 | 2.67359178497878 | estimated | NA | state_fe_expanded_controls |
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
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.04536023915945 | 0.608357746847161 | 1.71833143339929 | 0.0864134790557091 | 2.95266291500807 | 0.00465275004259441 | 0.451041809073526 | 6.9123849665503 | 0.0682110697951074 | 0.115101756331154 | 505 | 34 | 6 | estimated | NA | 9 | 0.404812213282416 | 1.6034538757165 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.366540582729035 | 0.485152671239069 | 0.75551595293271 | 0.450285534144002 | 0.57080435513582 | 0.000887849542649871 | 0.487143535229738 | 5.9925176493585 | 0.0297968042355124 | 0.136903980219079 | 562 | 34 | 6 | estimated | NA | 1 | -0.274007443147996 | -0.778404684155744 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.884531418609361 | 0.608959399769941 | 1.45252937871314 | 0.146965819119886 | 2.10984159602479 | 0.00413235092459746 | 0.478694278524368 | 6.58677612679421 | 0.064283364291209 | 0.134844189977506 | 561 | 34 | 6 | estimated | NA | 2 | 0.24398339273233 | 0.760632556733228 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.811860218216198 | 0.715733970864245 | 1.13430443609639 | 0.257231846420304 | 1.28664655374795 | 0.0028903720022025 | 0.447549246048394 | 6.75842066518467 | 0.053762180035801 | 0.113108453164729 | 529 | 34 | 6 | estimated | NA | 23 | 0.171312192339167 | -0.0625624855436151 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.480972473802074 | 0.528538638467758 | 0.910004375832239 | 0.363248391577877 | 0.828107964033823 | 0.00133856283871879 | 0.486147500802096 | 6.39100118075541 | 0.0365863750420771 | 0.138115614504892 | 560 | 34 | 6 | estimated | NA | 12 | -0.159575552074957 | -0.521101075257741 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.51608517229226 | 0.520938160620373 | 0.9906841373987 | 0.32232287557402 | 0.981455060093407 | 0.00153873384312097 | 0.499805352166118 | 6.57567788875022 | 0.0392266980909356 | 0.153636781954329 | 544 | 34 | 6 | estimated | NA | 33 | -0.124462853584771 | -0.367753979198157 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.563569784794472 | 0.543043869816273 | 1.03779789464366 | 0.299849284773071 | 1.07702447012681 | 0.00175084733442783 | 0.489076807916145 | 6.58719667093323 | 0.0418431276845772 | 0.139918645723861 | 565 | 34 | 6 | estimated | NA | 13 | -0.0769782410825595 | -0.272184569164758 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.56846888097701 | 0.521464376475378 | 1.0901394354478 | 0.276178923675656 | 1.18840398871846 | 0.00185815848556085 | 0.493047784490862 | 6.50211027432719 | 0.0431063624719478 | 0.146605848132121 | 546 | 34 | 6 | estimated | NA | 29 | -0.0720791449000212 | -0.160805050573106 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.569615456080683 | 0.631548803502133 | 0.9019341861183 | 0.367527834760056 | 0.81348527608888 | 0.0015983634612254 | 0.469415884942513 | 6.68808486934522 | 0.0399795380316729 | 0.126453535201678 | 546 | 34 | 6 | estimated | NA | 8 | -0.0709325697963485 | -0.535723763202685 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.710502189207657 | 0.56367450805363 | 1.26048309628375 | 0.208076082274752 | 1.58881763601706 | 0.00285707793649932 | 0.494322972208026 | 6.57075342661725 | 0.0534516411020237 | 0.149521858764732 | 554 | 34 | 6 | estimated | NA | 6 | 0.069954163330626 | 0.239608596725498 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.57848371491442 | 0.574854246646908 | 1.00631371915348 | 0.314749487456097 | 1.0126673013565 | 0.00187968095827983 | 0.497516069422357 | 6.63828936107294 | 0.0433552875469318 | 0.149939570808186 | 550 | 34 | 6 | estimated | NA | 28 | -0.062064310962611 | -0.33654173793506 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.699350134965036 | 0.511784767073986 | 1.36649267418296 | 0.172385341618885 | 1.86730222859571 | 0.00279854708059144 | 0.494026185939344 | 6.53097954635572 | 0.0529012956418972 | 0.14594522143916 | 559 | 34 | 6 | estimated | NA | 32 | 0.0588021090880045 | 0.518093189304143 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.697054081654121 | 0.574039219104907 | 1.21429696518128 | 0.225188703659114 | 1.47451711964847 | 0.00280723618103379 | 0.490678958596589 | 6.45541895527418 | 0.0529833575855077 | 0.141683237895786 | 565 | 34 | 6 | estimated | NA | 15 | 0.0565060557770901 | 0.125308080356906 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.696028524384511 | 0.552145371181105 | 1.26058925912142 | 0.20803669667068 | 1.58908528021229 | 0.00270149650766487 | 0.495973736648217 | 6.64176511390238 | 0.0519759223839666 | 0.139438915762341 | 555 | 34 | 6 | estimated | NA | 19 | 0.0554804985074796 | 0.23987624092073 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.693329238818862 | 0.554017942347555 | 1.25145629017176 | 0.211357949323492 | 1.56614284621046 | 0.00263273250570524 | 0.497658232056348 | 6.72461368487802 | 0.0513101598682898 | 0.137991662424956 | 544 | 34 | 6 | estimated | NA | 21 | 0.0527812129418312 | 0.216933806918894 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.590900683234156 | 0.547082102345328 | 1.08009507293508 | 0.280613374360281 | 1.16660536657865 | 0.00189819028713608 | 0.490654328446144 | 6.65457391299376 | 0.0435682256597254 | 0.140872590594244 | 555 | 34 | 6 | estimated | NA | 20 | -0.0496473426428747 | -0.182603672712918 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.597532866439166 | 0.532648923984561 | 1.12181371168317 | 0.262463412316238 | 1.25846600372037 | 0.00219625229318321 | 0.491451313401679 | 6.26615573125466 | 0.0468641898808023 | 0.143053608930676 | 564 | 34 | 6 | estimated | NA | 14 | -0.043015159437865 | -0.0907430355711989 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.598060988090996 | 0.551796186719714 | 1.08384400342872 | 0.278953211833763 | 1.17471782376839 | 0.00197984173512346 | 0.495073748992202 | 6.65426565900795 | 0.0444954125177397 | 0.138134786747926 | 551 | 34 | 6 | estimated | NA | 18 | -0.0424870377860354 | -0.174491215523172 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.677103557527217 | 0.629563080366664 | 1.07551344518625 | 0.282676361508645 | 1.15672917077639 | 0.00235986821712449 | 0.486228794389683 | 6.77722484255781 | 0.048578474833242 | 0.131331206405759 | 536 | 34 | 6 | estimated | NA | 10 | 0.0365555316501858 | -0.192479868515173 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.607220596958657 | 0.547205833796134 | 1.1096749330068 | 0.267654009534473 | 1.23137845694365 | 0.00211634145736028 | 0.489871344236908 | 6.46599940296906 | 0.0460037113433496 | 0.139914234712322 | 566 | 34 | 6 | estimated | NA | 7 | -0.0333274289183741 | -0.117830582347911 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.608633562506632 | 0.574874542552296 | 1.05872415188965 | 0.290244929472326 | 1.12089682979447 | 0.00203313590244653 | 0.498073311750306 | 6.72304416536262 | 0.045090308298415 | 0.143016560227478 | 540 | 34 | 6 | estimated | NA | 27 | -0.0319144633703992 | -0.228312209497098 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.672456670732515 | 0.574672750216785 | 1.17015583300033 | 0.242485856667434 | 1.36926467350471 | 0.00242424568131022 | 0.487836209258437 | 6.66269638456613 | 0.0492366294674055 | 0.136669707622233 | 557 | 34 | 6 | estimated | NA | 22 | 0.031908644855484 | 0.0200556342131417 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.6565850772601 | 0.580559892836952 | 1.1309514924492 | 0.258603944972541 | 1.27905127827308 | 0.0023323230972262 | 0.483217474210599 | 6.56960536429703 | 0.0482941310846837 | 0.135373703873901 | 561 | 34 | 6 | estimated | NA | 5 | 0.0160370513830684 | -0.0701577610184854 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.653860103693417 | 0.550825139484093 | 1.18705566762227 | 0.235749750494267 | 1.40910115803414 | 0.00236532802144901 | 0.491114503842614 | 6.60270525423945 | 0.0486346380828809 | 0.141823326440371 | 566 | 34 | 6 | estimated | NA | 17 | 0.0133120778163862 | 0.0598921187425794 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.653155989472328 | 0.542080992528049 | 1.20490479923723 | 0.228799963066571 | 1.45179557522491 | 0.00240767288142704 | 0.495692914831563 | 6.59828218000912 | 0.0490680433828888 | 0.152270417055731 | 557 | 34 | 6 | estimated | NA | 3 | 0.0126079635952966 | 0.102586535933349 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.648697040301966 | 0.54859069592601 | 1.18247911442789 | 0.237552947532134 | 1.39825685605816 | 0.00233685034933602 | 0.489858765873438 | 6.57351033127072 | 0.0483409800204345 | 0.141118525110296 | 571 | 34 | 6 | estimated | NA | 25 | 0.0081490144249351 | 0.0490478167665969 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.633784924643011 | 0.557784182140739 | 1.13625474679218 | 0.256394266012268 | 1.29107484960777 | 0.00225560056159776 | 0.497084073161467 | 6.63346826921502 | 0.0474931633142748 | 0.156798867009658 | 548 | 34 | 6 | estimated | NA | 24 | -0.00676310123402013 | -0.0581341896837955 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.634810381966715 | 0.566017040685947 | 1.12153934658469 | 0.262574997398561 | 1.25785050593761 | 0.00229841680887184 | 0.490634048406956 | 6.49661768136128 | 0.0479418064831905 | 0.141671857164893 | 569 | 34 | 6 | estimated | NA | 34 | -0.00573764391031628 | -0.0913585333539593 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.634873130361138 | 0.570704460429381 | 1.11243765272744 | 0.266461088983207 | 1.23751753120573 | 0.00226224985593071 | 0.489870172323192 | 6.53879405054279 | 0.0475631144473132 | 0.140281266615707 | 571 | 34 | 6 | estimated | NA | 35 | -0.00567489551589273 | -0.111691508085837 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.6370137362286 | 0.553549336973393 | 1.15078041590938 | 0.250348446747201 | 1.32429556564056 | 0.00227015514264165 | 0.489871524470971 | 6.54942576020169 | 0.0476461450974056 | 0.140151480257875 | 571 | 34 | 6 | estimated | NA | 30 | -0.00353428964843072 | -0.0249134736510082 |
| Table truncated in rendered note; full CSV has 35 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Leave-one-state-out influence

``` r
analysis_table(first_stage_district_influence[order(first_stage_district_influence$cooks_distance, decreasing = TRUE), ], "Most influential districts in the expanded first stage", max_rows = 30)
```

| state_code_2001 | district_code_2001 | leverage | cooks_distance | studentized_residual | instrument_dfbeta |
|:---|:---|:---|:---|:---|:---|
| 35 | 1 | 0.538090329737042 | 0.209519859870683 | 2.99109331594176 | 0.00567489551591337 |
| 35 | 2 | 0.538090329737043 | 0.209519859870683 | -2.99109331594176 | 0.00567489551591332 |
| 30 | 1 | 0.502240505677492 | 0.147034100425159 | -2.68797688535659 | 0.00353428964844677 |
| 30 | 2 | 0.502240505677491 | 0.147034100425158 | 2.68797688535659 | 0.00353428964844666 |
| 34 | 3 | 0.295163073016413 | 0.119437799347975 | 3.78562451421133 | -0.00561808490278036 |
| 14 | 1 | 0.127683972409339 | 0.112290125910301 | 6.35751308041325 | -0.00586775978110531 |
| 25 | 2 | 0.540840317994955 | 0.0809344301313226 | 1.83906456659709 | -0.00814901442492961 |
| 25 | 1 | 0.540840317994956 | 0.0809344301313224 | -1.83906456659709 | -0.00814901442492995 |
| 1 | 12 | 0.116620690537195 | 0.0789030204883659 | -5.5642219236722 | 0.079914197694868 |
| 34 | 1 | 0.300093783509675 | 0.0730058865855125 | -2.90896776743438 | 0.0254208749035618 |
| 1 | 9 | 0.119869126338462 | 0.0658114262338769 | 4.97501633480567 | -0.0435342390832683 |
| 7 | 2 | 0.152214547133261 | 0.05860015210536 | -4.0576840170515 | 0.0379005150491323 |
| 1 | 10 | 0.144540719406203 | 0.0497317012081586 | 3.84737888251011 | -0.025221238922672 |
| 15 | 8 | 0.157105924114545 | 0.0466950452638223 | 3.54206475924405 | 0.0119780613459743 |
| 7 | 4 | 0.149809798190033 | 0.0379072939409818 | 3.27676624768505 | 0.0299390945440727 |
| 14 | 2 | 0.150071464192018 | 0.0357550127695976 | -3.17720446233956 | 0.0390317703467568 |
| 12 | 6 | 0.099719635006898 | 0.0320814460533777 | 3.81567280787859 | 0.091794047235518 |
| 1 | 5 | 0.13524714507256 | 0.0295106137543802 | 3.06492568527574 | 0.0914801118992487 |
| 1 | 14 | 0.175902696255642 | 0.0261847171822336 | -2.46357380030909 | 0.0924279496535832 |
| 1 | 1 | 0.122850340002193 | 0.024997649747947 | -2.97945144901989 | -0.0362344302295203 |
| 15 | 7 | 0.205730667243197 | 0.0249529094451174 | 2.18043558774964 | -0.0897100465290863 |
| 1 | 6 | 0.115886637646315 | 0.0232929456345272 | 2.97283968519046 | 0.0586460642195707 |
| 32 | 10 | 0.100712909427964 | 0.022404059366898 | 3.15757635966705 | -0.0130661640035524 |
| 14 | 5 | 0.17353284892313 | 0.021630326472204 | -2.25547852998512 | -0.0303483453993761 |
| 5 | 5 | 0.123750432117897 | 0.0201983059448013 | 2.66259418450489 | -0.0887885192205887 |
| 1 | 13 | 0.198510941722141 | 0.0196819055194561 | -1.97875955364337 | 0.11132659541168 |
| 29 | 20 | 0.0937891297154707 | 0.018908105844768 | 3.01500080103961 | 0.0205058942300201 |
| 6 | 8 | 0.0595607796686903 | 0.0182948486947351 | 3.81047564062454 | -0.0195896088530562 |
| 28 | 5 | 0.17966008869833 | 0.0174292975163335 | 1.98023321626962 | 0.0502225148751772 |
| 11 | 4 | 0.261132968349567 | 0.0169947269500415 | -1.53699074653727 | -0.00253572964626976 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |

Most influential districts in the expanded first stage

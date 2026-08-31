# Instrument Exploration


``` r
helper <- if (file.exists("analysis/_analysis_helpers.R")) "analysis/_analysis_helpers.R" else "../_analysis_helpers.R"
source(helper)
```

## Current instrument and treatment diagnostics

The active construction uses the full mutually exclusive Census 2001
C-16 mother-tongue distribution. Language-group subtotal rows are
removed before aggregation, linguistic distances are resolved
mother-tongue-first from the documented Shastry concordance, and
unmapped speaker mass remains visible. The preferred public scalar is
`ling_distance_nonzero_mean`, the speaker-weighted mean distance among
mapped languages with distance greater than zero. Native Hindi and Urdu
are treated as the zero-distance reference and their mother-tongue
shares are reported separately; distinct leaves inside the Census Hindi
language group do not inherit zero distance.

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
dise_treatment_summary <- analysis_target_csv("diag_ext_dise", "dise_treatment_summary.csv")
dise_publication_checks <- analysis_target_csv("diag_ext_dise", "dise_publication_checks.csv")
dise_nss_validation <- analysis_target_csv("diag_ext_dise", "dise_nss_validation.csv")
dise_first_stage <- analysis_target_csv("diag_ext_dise", "dise_first_stage_permutations.csv")
dise_weak_iv <- analysis_target_csv("diag_ext_dise", "dise_weak_iv_outcomes.csv")
dise_dynamic_panel <- analysis_target_csv("diag_ext_dise", "dise_dynamic_district_year_2001.csv")
dise_dynamic_summary <- analysis_target_csv("diag_ext_dise", "dise_dynamic_first_stage_summary.csv")
dise_dynamic_event <- analysis_target_csv("diag_ext_dise", "dise_dynamic_first_stage_event_study.csv")
dise_school_quality_registry <- analysis_target_csv("diag_ext_dise", "dise_school_quality_registry.csv")
dise_school_quality_baseline <- analysis_target_csv("diag_ext_dise", "dise_school_quality_baseline_association.csv")
dise_school_quality_report <- analysis_target_csv("diag_ext_dise", "dise_school_quality_report_2001.csv")
dise_school_quality_summary <- analysis_target_csv("diag_ext_dise", "dise_school_quality_dynamic_summary.csv")
dise_school_quality_event <- analysis_target_csv("diag_ext_dise", "dise_school_quality_dynamic_event_study.csv")
```

## Administrative DISE treatment validation

Archived DISE district report-card data provide an administrative
measurement of English-medium enrollment that is independent of the NSS
household treatment. Medium identities for the 2005-06 through 2007-08
raw enrollment slots come from the corresponding published district
report cards rather than from slot position. The full diagnostic CSVs
retain every registered instrument construction, fixed-effect choice,
and control/absorption specification; the rendered tables below show the
main nonzero-mean-distance comparisons compactly.

``` r
analysis_table(dise_publication_checks, "DISE raw-to-published report validation anchors", max_rows = 20)
```

| academic_year | state | district | metric | expected_value | actual_value | difference | matches | source_pdf | source_page | note |
|:---|:---|:---|:---|---:|---:|---:|:---|:---|---:|:---|
| 2005-06 | JAMMU & KASHMIR | KUPWARA | dise_medium_slot_1_enrollment | 92642 | 92642 | 0 | TRUE | District report card 2005-06 vol-1.pdf | 219 | Published medium table labels slot 1 English. |
| 2005-06 | JAMMU & KASHMIR | KUPWARA | dise_medium_slot_2_enrollment | 711 | 711 | 0 | TRUE | District report card 2005-06 vol-1.pdf | 219 | Published medium table labels slot 2 Others. |
| 2005-06 | JAMMU & KASHMIR | KUPWARA | dise_medium_slot_3_enrollment | 65 | 65 | 0 | TRUE | District report card 2005-06 vol-1.pdf | 219 | Published medium table labels slot 3 Urdu. |

DISE raw-to-published report validation anchors

``` r
analysis_table(dise_treatment_summary, "DISE baseline treatment coverage and scale")
```

| variable | n_nonmissing | mean | sd | min | max | construct_id | treatment | analysis_scope | domain | margin | source_side | paper_role | label |
|:---|---:|---:|---:|---:|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| dise_emi_enrollment_share_total_0708 | 538 | 12.689 | 23.800 | 0 | 100.000 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| dise_emi_enrollment_share_total_0508_pooled | 475 | 13.205 | 24.543 | 0 | 99.879 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| dise_emi_gross_enrollment_ratio_age_6_13_0708 | 340 | 10.937 | 22.352 | 0 | 198.399 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | 299 | 10.772 | 22.246 | 0 | 172.514 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| dise_hindi_enrollment_share_total_0708 | 502 | 46.672 | 46.600 | 0 | 100.000 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| dise_english_share_english_hindi_0708 | 486 | 43.519 | 45.040 | 0 | 100.000 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| dise_private_enrollment_share_0708 | 552 | 27.401 | 18.619 | 0 | 95.114 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| dise_private_school_share_0708 | 552 | 19.486 | 14.641 | 0 | 95.481 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |

DISE baseline treatment coverage and scale

``` r
analysis_table(dise_nss_validation, "NSS-DISE district EMI measurement agreement")
```

| dise_variable | nss_variable | comparison | n | pearson | spearman | mean_dise | mean_nss | mean_difference | rmse | state_residual_pearson | status |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| dise_emi_enrollment_share_total_0708 | emi_share_enrolled_0708 | enrolled_total_denominator | 520 | 0.896 | 0.753 | 12.627 | 18.894 | -6.267 | 12.841 | 0.607 | estimated |
| dise_emi_enrollment_share_total_0708 | emi_exposure_all_children_0708 | all_child_context | 520 | 0.882 | 0.759 | 12.627 | 14.770 | -2.143 | 11.425 | 0.562 | estimated |

NSS-DISE district EMI measurement agreement

``` r
dise_main_first_stage <- dise_first_stage[
  dise_first_stage$construction_id == "nonzero_mean" &
    dise_first_stage$adjustment_id %in% c(
      "unadjusted", "region_main", "region_expanded", "state_main", "state_expanded"
    ),
  , drop = FALSE
]
analysis_table(
  dise_main_first_stage,
  "DISE constructs: preferred-distance first stages across fixed effects and controls",
  max_rows = 50
)
```

| specification_id | sequence | adjustment_id | adjustment | construction_id | construction | fixed_effect | excluded_instruments | included_language_controls | n_excluded_instruments | joint_excluded_f | joint_excluded_p | partial_r_squared | n | n_states | n_regions | construct_id | treatment | analysis_scope | domain | margin | source_side | paper_role | label |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 13.261 | 0.000 | 0.162 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 2.760 | 0.097 | 0.020 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 7.606 | 0.006 | 0.050 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.716 | 0.398 | 0.002 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.951 | 0.163 | 0.005 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 11.795 | 0.001 | 0.155 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 3.196 | 0.075 | 0.032 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 8.975 | 0.003 | 0.077 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.723 | 0.190 | 0.025 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 3.148 | 0.077 | 0.038 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 16.368 | 0.000 | 0.134 | 336 | 31 | 6 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.001 | 0.978 | 0.000 | 336 | 31 | 6 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.991 | 0.320 | 0.004 | 336 | 31 | 6 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.388 | 0.534 | 0.002 | 336 | 31 | 6 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.116 | 0.292 | 0.006 | 336 | 31 | 6 | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 15.214 | 0.000 | 0.121 | 295 | 29 | 6 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.118 | 0.731 | 0.001 | 295 | 29 | 6 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 2.176 | 0.141 | 0.012 | 295 | 29 | 6 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.493 | 0.483 | 0.003 | 295 | 29 | 6 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.509 | 0.220 | 0.008 | 295 | 29 | 6 | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 4.140 | 0.042 | 0.098 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.206 | 0.273 | 0.007 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.040 | 0.308 | 0.006 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.431 | 0.232 | 0.009 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.367 | 0.243 | 0.009 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only | medium | enrollment_composition | administrative_equilibrium | language_substitution | DISE 2007-08 Hindi-medium enrollment / total enrollment |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 7.165 | 0.008 | 0.159 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.275 | 0.600 | 0.002 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.040 | 0.842 | 0.000 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.351 | 0.554 | 0.003 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.255 | 0.614 | 0.002 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only | medium | english_hindi_composition | administrative_equilibrium | language_substitution | DISE 2007-08 English share among English + Hindi enrollment |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 1.519 | 0.218 | 0.027 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.128 | 0.289 | 0.018 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.243 | 0.265 | 0.019 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.824 | 0.177 | 0.004 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.836 | 0.176 | 0.004 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only | management | enrollment_composition | administrative_equilibrium | institution_choice_context | DISE 2007-08 private enrollment share |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 1.574 | 0.210 | 0.030 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.923 | 0.337 | 0.012 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.985 | 0.321 | 0.013 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.044 | 0.307 | 0.002 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.190 | 0.276 | 0.002 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only | management | school_stock_composition | administrative_supply | institutional_environment | DISE 2007-08 private-school share |

DISE constructs: preferred-distance first stages across fixed effects
and controls

``` r
dise_main_weak_iv <- dise_weak_iv[
  dise_weak_iv$construction_id == "nonzero_mean" &
    dise_weak_iv$adjustment_id %in% c(
      "unadjusted", "region_main", "region_expanded", "state_main", "state_expanded"
    ),
  , drop = FALSE
]
analysis_table(
  dise_main_weak_iv,
  "Administrative EMI treatments: weak-IV-aware outcome estimates",
  max_rows = 40
)
```

| specification_id | adjustment_id | construction_id | estimate_2sls | std_error_clustered | p_value_clustered | effective_f | effective_f_critical_value | effective_f_p_value | effective_f_df | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_n_components | ar_95_disconnected | ar_95_contains_zero | ar_95_grid_accepted_min | ar_95_grid_accepted_max | ar_95_left_truncated | ar_95_right_truncated | ar_95_components | n | status | reason | construct_id | treatment | analysis_scope | domain | margin | source_side | paper_role | label |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|:---|:---|---:|---:|:---|:---|:---|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.004 | 0.002 | 0.025 | 13.287 | 23.109 | 0.315 | 1 | 3.401 | 0.066 | 3.401 | 0.066 | -0.007 | 0.000 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.000 | FALSE | FALSE | \[-0.00719088, 0\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.001 | 0.006 | 0.847 | 2.847 | 23.109 | 0.930 | 1 | 0.042 | 0.838 | 0.042 | 0.838 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958784, -0.026846\] U \[-0.0124642, 0.0958784 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.000 | 0.004 | 0.906 | 7.895 | 23.109 | 0.638 | 1 | 0.013 | 0.908 | 0.013 | 0.908 | -0.008 | 0.016 | FALSE | 1 | FALSE | TRUE | -0.008 | 0.016 | FALSE | FALSE | \[-0.00767027, 0.0158199\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.038 | 0.050 | 0.450 | 0.778 | 23.109 | 0.989 | 1 | 3.324 | 0.069 | 3.324 | 0.069 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958784, -0.0158199\] U \[-0.00335574, 0.0958784 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.023 | 0.023 | 0.316 | 2.132 | 23.109 | 0.956 | 1 | 2.776 | 0.096 | 2.776 | 0.096 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958784, -0.0536919\] U \[-0.00383514, 0.0958784 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv | medium | enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium enrollment / total enrollment |
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.004 | 0.002 | 0.024 | 11.821 | 23.109 | 0.391 | 1 | 3.027 | 0.083 | 3.027 | 0.083 | -0.007 | 0.000 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.000 | FALSE | FALSE | \[-0.00700099, 0.000466733\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.002 | 0.005 | 0.767 | 3.312 | 23.109 | 0.910 | 1 | 0.106 | 0.744 | 0.106 | 0.744 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.093 | 0.093 | TRUE | TRUE | \[grid\<= -0.0933465, -0.0536743\] U \[-0.00746772, 0.0933465 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.000 | 0.003 | 0.991 | 9.365 | 23.109 | 0.541 | 1 | 0.000 | 0.991 | 0.000 | 0.991 | -0.006 | 0.013 | FALSE | 1 | FALSE | TRUE | -0.006 | 0.013 | FALSE | FALSE | \[-0.00560079, 0.0126018\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.015 | 0.014 | 0.286 | 1.888 | 23.109 | 0.963 | 1 | 4.397 | 0.037 | 4.397 | 0.037 | NA | NA | FALSE | 2 | TRUE | FALSE | -0.093 | 0.093 | TRUE | TRUE | \[grid\<= -0.0933465, -0.028004\] U \[0.000933465, 0.0933465 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.011 | 0.009 | 0.224 | 3.475 | 23.109 | 0.903 | 1 | 3.459 | 0.064 | 3.459 | 0.064 | NA | NA | FALSE | 1 | FALSE | TRUE | 0.000 | 0.093 | FALSE | TRUE | \[-0.000466733, 0.0933465 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv | medium | pooled_enrollment_composition | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment |
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.002 | 0.002 | 0.322 | 16.417 | 23.109 | 0.187 | 1 | 0.981 | 0.323 | 0.981 | 0.323 | -0.007 | 0.002 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.002 | FALSE | FALSE | \[-0.00738072, 0.00196819\] | 336 | estimated | NA | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.146 | 5.491 | 0.979 | 0.001 | 23.109 | 1.000 | 1 | 0.051 | 0.822 | 0.051 | 0.822 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.098 | 0.098 | TRUE | TRUE | \[grid\<= -0.0984095, 0.0984095 \<=grid\] | 336 | estimated | NA | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.003 | 0.020 | 0.884 | 1.051 | 23.109 | 0.984 | 1 | 0.023 | 0.879 | 0.023 | 0.879 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.098 | 0.098 | TRUE | TRUE | \[grid\<= -0.0984095, 0.0984095 \<=grid\] | 336 | estimated | NA | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.045 | 0.067 | 0.504 | 0.442 | 23.109 | 0.994 | 1 | 1.885 | 0.171 | 1.885 | 0.171 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.098 | 0.098 | TRUE | TRUE | \[grid\<= -0.0984095, 0.0984095 \<=grid\] | 336 | estimated | NA | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.029 | 0.028 | 0.303 | 1.284 | 23.109 | 0.979 | 1 | 2.112 | 0.147 | 2.112 | 0.147 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.098 | 0.098 | TRUE | TRUE | \[grid\<= -0.0984095, 0.0984095 \<=grid\] | 336 | estimated | NA | emi_age6_13_gross_0708 | dise_emi_gross_enrollment_ratio_age_6_13_0708 | structural_iv | medium | population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.002 | 0.002 | 0.308 | 15.266 | 23.109 | 0.228 | 1 | 0.970 | 0.325 | 0.970 | 0.325 | -0.007 | 0.002 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.002 | FALSE | FALSE | \[-0.00745039, 0.00248346\] | 295 | estimated | NA | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.009 | 0.058 | 0.875 | 0.125 | 23.109 | 0.998 | 1 | 0.047 | 0.829 | 0.047 | 0.829 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.099 | 0.099 | TRUE | TRUE | \[grid\<= -0.0993385, 0.0993385 \<=grid\] | 295 | estimated | NA | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.002 | 0.012 | 0.852 | 2.327 | 23.109 | 0.949 | 1 | 0.039 | 0.844 | 0.039 | 0.844 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.099 | 0.099 | TRUE | TRUE | \[grid\<= -0.0993385, 0.0993385 \<=grid\] | 295 | estimated | NA | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.042 | 0.059 | 0.470 | 0.569 | 23.109 | 0.992 | 1 | 1.407 | 0.237 | 1.407 | 0.237 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.099 | 0.099 | TRUE | TRUE | \[grid\<= -0.0993385, 0.0993385 \<=grid\] | 295 | estimated | NA | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.027 | 0.027 | 0.308 | 1.761 | 23.109 | 0.967 | 1 | 1.371 | 0.243 | 1.371 | 0.243 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.099 | 0.099 | TRUE | TRUE | \[grid\<= -0.0993385, 0.0993385 \<=grid\] | 295 | estimated | NA | emi_age6_13_gross_0508_pooled | dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled | structural_iv | medium | pooled_population_scaled_enrollment | administrative_equilibrium | formal_english_exposure | DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator |

Administrative EMI treatments: weak-IV-aware outcome estimates

## Longitudinal DISE relevance

The dynamic panel asks whether the EMI-distance gradient changes over
time within Census-2001 districts. The first model absorbs district and
academic-year fixed effects; the second replaces common year effects
with state-by-academic-year effects. Coefficients are relative to
2007-08 and inference is clustered by Census-2001 district.

``` r
dise_dynamic_qa <- do.call(
  rbind,
  lapply(split(dise_dynamic_panel, dise_dynamic_panel$academic_year), function(x) {
    data.frame(
      academic_year = x$academic_year[[1]],
      n_rows = nrow(x),
      n_emi = sum(is.finite(x$dise_emi_enrollment_share_total)),
      n_invalid_english_count = if ("dise_english_count_valid" %in% names(x)) {
        sum(x$dise_english_count_valid %in% FALSE, na.rm = TRUE)
      } else {
        0L
      },
      stringsAsFactors = FALSE
    )
  })
)
analysis_table(dise_dynamic_qa, "Longitudinal DISE EMI coverage and count-validity checks", max_rows = 20)
```

| academic_year | n_rows | n_emi | n_invalid_english_count |
|:--------------|-------:|------:|------------------------:|
| 2005-06       |    547 |   503 |                      44 |
| 2006-07       |    550 |   528 |                      22 |
| 2007-08       |    552 |   538 |                      14 |
| 2008-09       |    552 |   471 |                      81 |
| 2009-10       |    552 |   468 |                      84 |
| 2010-11       |    532 |   461 |                      71 |
| 2011-12       |    552 |   490 |                      62 |
| 2012-13       |    553 |   486 |                      67 |
| 2013-14       |    553 |   504 |                      49 |
| 2014-15       |    549 |   536 |                      13 |
| 2015-16       |    549 |   549 |                       0 |

Longitudinal DISE EMI coverage and count-validity checks

``` r
analysis_table(
  dise_dynamic_summary[dise_dynamic_summary$construction_id == "nonzero_mean", ],
  "Preferred linguistic-distance trajectory: joint clustered tests",
  max_rows = 10
)
```

| instrument | dynamic_fe | reference_year | n | n_districts | n_years | joint_distance_year_f | joint_distance_year_p | pre_distance_year_f | pre_distance_year_p | post_distance_year_f | post_distance_year_p | cluster_status | outcome | construction_id | equivalent_construction_ids |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|:---|:---|:---|
| ling_distance_nonzero_mean | district_year | 2007-08 | 5353 | 554 | 11 | 2.575 | 0.004 | 2.298 | 0.101 | 2.269 | 0.020 | estimated | dise_emi_enrollment_share_total | nonzero_mean | nonzero_mean;nonzero_mean_hindi_urdu;nonzero_mean_hindi_urdu_separate;nonzero_mean_shastry |
| ling_distance_nonzero_mean | district_state_year | 2007-08 | 5353 | 554 | 11 | 1.759 | 0.063 | 1.495 | 0.224 | 1.528 | 0.142 | estimated | dise_emi_enrollment_share_total | nonzero_mean | nonzero_mean;nonzero_mean_hindi_urdu;nonzero_mean_hindi_urdu_separate;nonzero_mean_shastry |

Preferred linguistic-distance trajectory: joint clustered tests

``` r
analysis_table(
  dise_dynamic_event[dise_dynamic_event$construction_id == "nonzero_mean", ],
  "Preferred linguistic-distance trajectory: year coefficients relative to 2007-08",
  max_rows = 30
)
```

| academic_year | reference_year | estimate | std.error | statistic | p.value | outcome | construction_id | instrument | dynamic_fe |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|:---|
| 2005-06 | 2007-08 | -0.606 | 0.306 | -1.981 | 0.048 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2006-07 | 2007-08 | -0.426 | 0.228 | -1.868 | 0.062 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2008-09 | 2007-08 | 0.569 | 0.215 | 2.644 | 0.008 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2009-10 | 2007-08 | 0.206 | 0.374 | 0.551 | 0.582 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2010-11 | 2007-08 | 0.523 | 0.409 | 1.278 | 0.201 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2011-12 | 2007-08 | 0.637 | 0.409 | 1.557 | 0.120 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2012-13 | 2007-08 | 0.812 | 0.421 | 1.928 | 0.054 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2013-14 | 2007-08 | 0.632 | 0.491 | 1.288 | 0.198 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2014-15 | 2007-08 | 1.191 | 0.496 | 2.398 | 0.016 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2015-16 | 2007-08 | 1.339 | 0.525 | 2.551 | 0.011 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2005-06 | 2007-08 | 2.287 | 1.483 | 1.543 | 0.123 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2006-07 | 2007-08 | 2.011 | 1.164 | 1.727 | 0.084 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2008-09 | 2007-08 | 0.616 | 1.119 | 0.550 | 0.582 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2009-10 | 2007-08 | 0.426 | 1.024 | 0.416 | 0.677 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2010-11 | 2007-08 | 0.472 | 1.115 | 0.423 | 0.672 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2011-12 | 2007-08 | 0.222 | 1.109 | 0.200 | 0.841 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2012-13 | 2007-08 | -0.527 | 1.015 | -0.519 | 0.604 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2013-14 | 2007-08 | -0.413 | 1.054 | -0.391 | 0.695 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2014-15 | 2007-08 | -0.284 | 1.092 | -0.260 | 0.795 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2015-16 | 2007-08 | -0.576 | 1.093 | -0.527 | 0.598 | dise_emi_enrollment_share_total | nonzero_mean | ling_distance_nonzero_mean | district_state_year |

Preferred linguistic-distance trajectory: year coefficients relative to
2007-08

## DISE school-quality mechanisms

The 2005-06 and 2006-07 associations are predetermined school-system
diagnostics estimated with state fixed effects and state-clustered
inference after count-preserving Census-2001 harmonization. Later
mechanism trajectories are reconstructed from the published district
report cards rather than the corrupted raw School/Teacher sheets.
Because the publication values are already ratios, only one-to-one
deterministic later-district mappings are retained on Census-2001
geography; split parents are not averaged. PTR and single-teacher
trajectories begin in 2011-12, while the girls’-toilet trajectory begins
in 2012-13 because the report-card denominator changes after 2011-12.

``` r
analysis_table(dise_school_quality_registry, "DISE school-quality mechanism registry")
```

| outcome | baseline_outcome | dynamic_outcome | label | direction | domain | margin | source_side | paper_role | dynamic_start_year | dynamic_reference_year | dynamic_status | definition_note |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| dise_pupils_per_teacher | dise_pupils_per_teacher_0708 | dise_report_pupils_per_teacher | Pupils per teacher | lower_is_better | quality | teacher_resources | administrative_supply | complementarity | 2011-12 | 2011-12 | estimated_report_cards | Published all-school PTR. |
| dise_single_teacher_school_share | dise_single_teacher_school_share_0708 | dise_report_single_teacher_school_share | Single-teacher schools (%) | lower_is_better | quality | school_stock_quality | administrative_supply | complementarity | 2011-12 | 2011-12 | estimated_report_cards | Published all-school single-teacher-school percentage. |
| dise_girls_toilet_school_share | dise_girls_toilet_school_share_0708 | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | higher_is_better | quality | school_amenity | administrative_supply | complementarity | 2012-13 | 2012-13 | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. |

DISE school-quality mechanism registry

``` r
analysis_table(
  dise_school_quality_baseline,
  "Predetermined 2005-06/2006-07 school-quality associations with preferred linguistic distance",
  max_rows = 10
)
```

| academic_year | outcome | instrument | estimate | std.error | statistic | p.value | n | cluster_variable | cluster_status |
|:---|:---|:---|---:|---:|---:|---:|---:|:---|:---|
| 2005-06 | dise_pupils_per_teacher | ling_distance_nonzero_mean | -0.717 | 0.836 | -0.857 | 0.391 | 530 | state_code_2001 | estimated |
| 2005-06 | dise_single_teacher_school_share | ling_distance_nonzero_mean | 1.757 | 0.435 | 4.035 | 0.000 | 530 | state_code_2001 | estimated |
| 2005-06 | dise_girls_toilet_school_share | ling_distance_nonzero_mean | -2.834 | 0.966 | -2.934 | 0.003 | 530 | state_code_2001 | estimated |
| 2006-07 | dise_pupils_per_teacher | ling_distance_nonzero_mean | -1.415 | 0.801 | -1.766 | 0.077 | 533 | state_code_2001 | estimated |
| 2006-07 | dise_single_teacher_school_share | ling_distance_nonzero_mean | 1.500 | 0.421 | 3.566 | 0.000 | 533 | state_code_2001 | estimated |
| 2006-07 | dise_girls_toilet_school_share | ling_distance_nonzero_mean | -2.398 | 0.722 | -3.322 | 0.001 | 533 | state_code_2001 | estimated |

Predetermined 2005-06/2006-07 school-quality associations with preferred
linguistic distance

``` r
school_quality_coverage <- do.call(
  rbind,
  lapply(split(dise_school_quality_report, dise_school_quality_report$academic_year), function(x) {
    data.frame(
      academic_year = x$academic_year[[1]],
      n_targets = nrow(x),
      n_one_to_one = sum(x$dise_report_school_quality_status == "one_to_one_report_ratio"),
      n_ptr = sum(is.finite(x$dise_report_pupils_per_teacher)),
      n_single_teacher = sum(is.finite(x$dise_report_single_teacher_school_share)),
      n_girls_toilet = sum(is.finite(x$dise_report_girls_toilet_school_share)),
      stringsAsFactors = FALSE
    )
  })
)
analysis_table(
  school_quality_coverage,
  "Publication-derived school-quality coverage on Census-2001 geography",
  max_rows = 10
)
```

| academic_year | n_targets | n_one_to_one | n_ptr | n_single_teacher | n_girls_toilet |
|:--------------|----------:|-------------:|------:|-----------------:|---------------:|
| 2011-12       |       552 |          535 |   535 |              535 |            535 |
| 2012-13       |       553 |          533 |   533 |              533 |            533 |
| 2013-14       |       552 |          532 |   532 |              532 |            532 |
| 2014-15       |       551 |          531 |   531 |              531 |            531 |

Publication-derived school-quality coverage on Census-2001 geography

``` r
analysis_table(
  dise_school_quality_summary,
  "Publication-derived school-quality trajectories: joint clustered tests",
  max_rows = 20
)
```

| instrument | dynamic_fe | reference_year | n | n_districts | n_years | joint_distance_year_f | joint_distance_year_p | pre_distance_year_f | pre_distance_year_p | post_distance_year_f | post_distance_year_p | cluster_status | outcome | label | dynamic_status | definition_note |
|:---|:---|:---|---:|---:|---:|---:|---:|:---|:---|---:|---:|:---|:---|:---|:---|:---|
| ling_distance_nonzero_mean | district_year | 2011-12 | 2065 | 523 | 4 | 3.922 | 0.008 | NA | NA | 3.922 | 0.008 | estimated | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. |
| ling_distance_nonzero_mean | district_state_year | 2011-12 | 2065 | 523 | 4 | 0.411 | 0.745 | NA | NA | 0.411 | 0.745 | estimated | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. |
| ling_distance_nonzero_mean | district_year | 2011-12 | 2065 | 523 | 4 | 5.599 | 0.001 | NA | NA | 5.599 | 0.001 | estimated | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. |
| ling_distance_nonzero_mean | district_state_year | 2011-12 | 2065 | 523 | 4 | 0.657 | 0.578 | NA | NA | 0.657 | 0.578 | estimated | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. |
| ling_distance_nonzero_mean | district_year | 2012-13 | 1546 | 519 | 3 | 11.786 | 0.000 | NA | NA | 11.786 | 0.000 | estimated | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. |
| ling_distance_nonzero_mean | district_state_year | 2012-13 | 1546 | 519 | 3 | 1.437 | 0.238 | NA | NA | 1.437 | 0.238 | estimated | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. |

Publication-derived school-quality trajectories: joint clustered tests

``` r
analysis_table(
  dise_school_quality_event,
  "Publication-derived school-quality trajectories: year coefficients",
  max_rows = 40
)
```

| academic_year | reference_year | estimate | std.error | statistic | p.value | outcome | label | dynamic_status | definition_note | dynamic_fe | instrument |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|:---|:---|:---|
| 2012-13 | 2011-12 | 0.256 | 0.151 | 1.698 | 0.089 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_year | ling_distance_nonzero_mean |
| 2013-14 | 2011-12 | 0.342 | 0.168 | 2.032 | 0.042 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_year | ling_distance_nonzero_mean |
| 2014-15 | 2011-12 | 0.551 | 0.196 | 2.804 | 0.005 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_year | ling_distance_nonzero_mean |
| 2012-13 | 2011-12 | -0.442 | 0.789 | -0.559 | 0.576 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_state_year | ling_distance_nonzero_mean |
| 2013-14 | 2011-12 | -0.367 | 0.795 | -0.461 | 0.645 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_state_year | ling_distance_nonzero_mean |
| 2014-15 | 2011-12 | -0.193 | 0.844 | -0.229 | 0.819 | dise_report_pupils_per_teacher | Pupils per teacher | estimated_report_cards | Published all-school PTR. | district_state_year | ling_distance_nonzero_mean |
| 2012-13 | 2011-12 | 0.406 | 0.126 | 3.226 | 0.001 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_year | ling_distance_nonzero_mean |
| 2013-14 | 2011-12 | 0.242 | 0.165 | 1.467 | 0.142 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_year | ling_distance_nonzero_mean |
| 2014-15 | 2011-12 | 0.091 | 0.177 | 0.514 | 0.607 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_year | ling_distance_nonzero_mean |
| 2012-13 | 2011-12 | 0.427 | 0.329 | 1.300 | 0.194 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_state_year | ling_distance_nonzero_mean |
| 2013-14 | 2011-12 | 0.248 | 0.371 | 0.670 | 0.503 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_state_year | ling_distance_nonzero_mean |
| 2014-15 | 2011-12 | 0.082 | 0.427 | 0.191 | 0.848 | dise_report_single_teacher_school_share | Single-teacher schools (%) | estimated_report_cards | Published all-school single-teacher-school percentage. | district_state_year | ling_distance_nonzero_mean |
| 2013-14 | 2012-13 | 0.865 | 0.182 | 4.748 | 0.000 | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. | district_year | ling_distance_nonzero_mean |
| 2014-15 | 2012-13 | 0.587 | 0.427 | 1.376 | 0.169 | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. | district_year | ling_distance_nonzero_mean |
| 2013-14 | 2012-13 | 0.888 | 0.654 | 1.358 | 0.174 | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. | district_state_year | ling_distance_nonzero_mean |
| 2014-15 | 2012-13 | -0.313 | 1.536 | -0.204 | 0.839 | dise_report_girls_toilet_school_share | Schools with girls’ toilet (%) | estimated_report_cards | Published all-school eligible-school percentage; 2011-12 is excluded because the report-card denominator changes from all schools to girls’/coeducational schools in 2012-13. | district_state_year | ling_distance_nonzero_mean |

Publication-derived school-quality trajectories: year coefficients

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
| 1 | 1113 | Jammu & Kashmir | Jammu | 48.3960192416473 | 1.23149769037605 | 1 |
| 2 | 1114 | Jammu & Kashmir | Kathus | 33.5921008544035 | 1.08852360186491 | 1 |
| 3 | 1209 | Jammu & Kashmir | Doda | 78.5237035453088 | 3.4450773447949 | 1 |
| 4 | 1210 | Jammu & Kashmir | Udhampur | 72.6236337161995 | 1.39358936012059 | 1 |
| 5 | 1212 | Jammu & Kashmir | Rajauri | 13.1705037832309 | 1.24009399235556 | 1 |
| 6 | 1301 | Jammu & Kashmir | Kupwara | 31.8657493187537 | 3.68099387465247 | 1 |
| 7 | 1302 | Jammu & Kashmir | Baramula | 39.0896349502222 | 3.85542927008564 | 1 |
| 8 | 1303 | Jammu & Kashmir | Srinagar | 73.6252335167284 | 3.84309021403949 | 1 |
| 9 | 1304 | Jammu & Kashmir | Badgam | 50.3440453344655 | 3.93166236962381 | 1 |
| 10 | 1305 | Jammu & Kashmir | Pulwama | 73.7864106740856 | 3.86179074779268 | 1 |
| 11 | 1306 | Jammu & Kashmir | Anantnag | 72.0857411304068 | 3.73270388945244 | 1 |
| 12 | 2102 | Himachal Pradesh | Kangra | 21.4320768227842 | 2.08700724143244 | 2 |
| 13 | 2104 | Himachal Pradesh | Kullu | 1.77293606029376 | 3.93093789239834 | 2 |
| 14 | 2105 | Himachal Pradesh | Mandi | 11.914923634052 | 1.85632898958811 | 2 |
| 15 | 2106 | Himachal Pradesh | Hamirpur | 15.9566958650642 | 1.41739775114296 | 2 |
| 16 | 2107 | Himachal Pradesh | Una | 13.2443116562273 | 1.02218667767401 | 2 |
| 17 | 2201 | Himachal Pradesh | Chamba | 9.99246785718507 | 1.9358135365307 | 2 |
| 18 | 2203 | Himachal Pradesh | Lahul & Spiti | 5.80506231778363 | 4.44892149348516 | 2 |
| 19 | 2208 | Himachal Pradesh | Bilaspur | 15.5006331251223 | 1.01556550239184 | 2 |
| 20 | 2209 | Himachal Pradesh | Solan | 14.0781014588688 | 1.81884674264331 | 2 |
| 21 | 2210 | Himachal Pradesh | Sirmapur | 14.511327515228 | 1.61570768584628 | 2 |
| 22 | 2211 | Himachal Pradesh | Shimla | 37.7841107395915 | 2.27068993116696 | 2 |
| 23 | 2212 | Himachal Pradesh | Kinnaur | 4.0754499392593 | 4.91092567732487 | 2 |
| 24 | 3101 | Punjab | Gurdaspur | 25.9349887876681 | 1.02376501870207 | 3 |
| 25 | 3102 | Punjab | Amritsar | 21.8804829341996 | 1.00856128268153 | 3 |
| 26 | 3103 | Punjab | Kapurthala | 35.3165001044135 | 1.0103462655763 | 3 |
| 27 | 3104 | Punjab | Jalandhar | 31.317678667039 | 1.0191361526903 | 3 |
| 28 | 3106 | Punjab | Nawanshahr | 28.3822720514065 | 1.00822071042134 | 3 |
| 29 | 3107 | Punjab | Rupnagar | 30.4238156299204 | 1.01645574815098 | 3 |
| 30 | 3208 | Punjab | Fatehgarh Sahib | 19.5810353849101 | 1.00442409096888 | 3 |
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
| Andaman & Nicobar Islands | 2 | 18.1700233096527 | 4.25457336795535 | 171329.6625 | 1610.80625339684 | 42.3968482751965 |
| Andhra Pradesh | 13 | 17.50329470393 | 4.97084313916762 | 3406138.48653846 | 806.038478947683 | 46.8784465840865 |
| Arunachal Pradesh | 13 | 72.0167035742077 | 4.33583977140935 | 76279.7523076923 | 821.925150923097 | 61.0798056117889 |
| Assam | 22 | 4.05362607555326 | 3.38330225359571 | 1070667.90886364 | 762.436038575419 | 58.0450186320867 |
| Bihar | 37 | 2.27384573509356 | 3.14293416226008 | 2051830.32918919 | 559.316304711329 | 84.7022490469553 |
| Chandigarh | 1 | 43.9877594163667 | 1.29894867208878 | 837516.24 | 2237.62405162436 | 36.7830442420546 |
| Chhattisgarh | 16 | 2.44891352706489 | 3.53266402865211 | 1455306.785 | 519.343724642298 | 59.4295625174172 |
| Dadra & Nagar Haveli | 1 | 4.31608528371716 | 1.66896856581532 | 204832.69 | 878.890652903109 | 55.2790526851351 |
| Daman & Diu | 2 | 20.0968799371822 | 1.19954717236104 | 69690.93 | 1391.7018118573 | 51.6253036614033 |
| Delhi | 7 | 26.6421146024289 | 2.19464505953987 | 1802076.03428571 | 1274.45536408576 | 51.9338015790853 |
| Goa | 2 | 46.9931655099055 | 2.27617749269112 | 698016.255 | 1277.6412702353 | 35.7209708999027 |
| Gujarat | 25 | 3.29157078968408 | 1.12804150475298 | 2038854.0708 | 858.757699979292 | 53.6880918495208 |
| Haryana | 19 | 15.4550600433681 | 1.26339967439969 | 1142009.75631579 | 974.810008859313 | 53.1686777255239 |
| Himachal Pradesh | 12 | 13.8390080826217 | 2.36086076013541 | 519885.804583333 | 932.211852792387 | 53.7694561219673 |
| Jammu & Kashmir | 11 | 53.3729796423138 | 2.84585930501441 | 735654.508181818 | 917.231422452719 | 52.1863851221851 |
| Jharkhand | 18 | 4.87131622223234 | 3.61339670251259 | 1395211.16555556 | 632.040527265219 | 67.2780276872427 |
| Karnataka | 27 | 12.5522398616991 | 4.74616442326267 | 1844944.73574074 | 753.91460740765 | 49.7504147543725 |
| Kerala | 14 | 37.8847415252017 | 4.98531440009801 | 2129849.7675 | 1061.81342032626 | 48.0392820929302 |
| Lakshadweep | 1 | 27.693093915454 | 4.96879878164858 | 57165.375 | 1258.92789866243 | 47.3127675768181 |
| Madhya Pradesh | 44 | 4.8124959319002 | 2.46362881452976 | 1333711.54318182 | 598.39039835291 | 63.7789027840119 |
| Maharashtra | 33 | 8.0006130127011 | 2.09100713510861 | 2757972.46242424 | 790.791751568391 | 52.2534100830016 |
| Manipur | 9 | 54.3931471021197 | 4.93289611067276 | 220217.051666667 | 819.916168102988 | 50.0165633266468 |
| Meghalaya | 7 | 46.5401931358657 | 4.7463815498894 | 325151.725 | 889.075788565463 | 63.5951262270377 |
| Mizoram | 8 | 43.4928512499453 | 4.71243017805013 | 104185.15875 | 1154.1292730263 | 61.7142451234391 |
| Nagaland | 8 | 80.5294327657661 | 4.3626909976799 | 118572.34625 | 1183.08745343988 | 41.5966283338895 |
| Odisha | 29 | 4.48207369404672 | 3.24008937236287 | 1205721.18155172 | 530.726549256539 | 53.2755205864839 |
| Puducherry | 4 | 46.1645721489379 | 4.99326772267732 | 207733.7475 | 1172.67173441507 | 44.4357277807352 |
| Punjab | 16 | 22.8096522536849 | 1.01113882125616 | 1452066.4490625 | 1116.02721196292 | 50.1406819071539 |
| Rajasthan | 27 | 3.67185054785357 | 1.04340446674048 | 1755742.01925926 | 730.020097099085 | 70.0862133128767 |
| Sikkim | 4 | 80.4287978097929 | 4.71107405512736 | 129015.62625 | 773.645294969817 | 55.1816987031961 |
| Table truncated in rendered note; full CSV has 36 rows. |  |  |  |  |  |  |

Current IV-panel state summary

``` r
analysis_table(iv_rows, "Current keyed IV summary rows", max_rows = 30)
```

| group | variable | var | label | N | Min | 1Q | Med | 3Q | Max | Mean | SD | desc |
|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| Treatment and instrument | ling_distance_nonzero_mean | ling_distance_nonzero_mean | Linguistic distance | 573 | 1.00 | 1.97 | 3.02 | 4.33 | 5.00 | 3.05 | 1.36 | Population-weighted mean linguistic distance among mapped speakers with positive distance from Hindi |
| Treatment and instrument | emi_exposure_all_children_0708 | emi_exposure_all_children_0708 | EMI exposure | 573 | 0.00 | 1.42 | 6.23 | 17.91 | 95.98 | 14.88 | 20.33 | Share of children ages 5-19 enrolled in English-medium instruction |
| Consumption outcomes | real_consumption_0708 | real_consumption_0708 | Real consumption, 2007-08 | 573 | 439.46 | 826.48 | 986.00 | 1179.91 | 2945.06 | 1035.34 | 294.39 | Person-weighted monthly consumption in common prices |
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
| instrument_only | Instrument only | 1 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none |  | 0 | 5.622 | 1.641 | 3.425 | 0.001 | 11.731 | 0.142 | 1.362 | 20.332 | 0.376 | 1.000 | 573 | 35 | 6 | estimated | NA |
| region_fe | Six-region fixed effects | 2 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region |  | 0 | 7.220 | 3.488 | 2.070 | 0.039 | 4.283 | 0.098 | 0.689 | 15.895 | 0.313 | 0.256 | 573 | 35 | 6 | estimated | NA |
| state_fe | State fixed effects | 3 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state |  | 0 | 0.336 | 0.959 | 0.351 | 0.726 | 0.123 | 0.000 | 0.538 | 8.413 | 0.022 | 0.156 | 573 | 35 | 6 | estimated | NA |
| census_controls | Main Census controls | 4 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 4.686 | 1.240 | 3.780 | 0.000 | 14.288 | 0.130 | 1.091 | 14.195 | 0.360 | 0.642 | 573 | 35 | 6 | estimated | NA |
| region_fe_census_controls | Six-region fixed effects + main Census controls | 5 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 4.050 | 2.251 | 1.799 | 0.073 | 3.238 | 0.036 | 0.587 | 12.543 | 0.190 | 0.186 | 573 | 35 | 6 | estimated | NA |
| state_fe_census_controls | State fixed effects + main Census controls | 6 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 0.839 | 0.966 | 0.869 | 0.385 | 0.755 | 0.004 | 0.492 | 6.591 | 0.063 | 0.130 | 573 | 35 | 6 | estimated | NA |
| expanded_controls | Expanded Census controls | 7 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.452 | 1.194 | 4.567 | 0.000 | 20.856 | 0.175 | 1.056 | 13.756 | 0.418 | 0.601 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_controls | Six-region fixed effects + expanded Census controls | 8 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.063 | 2.131 | 2.376 | 0.018 | 5.644 | 0.057 | 0.574 | 12.139 | 0.239 | 0.177 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.916 | 0.909 | 1.007 | 0.314 | 1.015 | 0.005 | 0.484 | 6.546 | 0.068 | 0.126 | 573 | 35 | 6 | estimated | NA |
| region_fe_main_without_human_capital | Six-region FE + main controls without human capital | 10 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 5.379 | 2.132 | 2.523 | 0.012 | 6.364 | 0.061 | 0.604 | 13.179 | 0.246 | 0.197 | 573 | 35 | 6 | estimated | NA |
| state_fe_main_without_human_capital | State FE + main controls without human capital | 11 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 1.239 | 0.985 | 1.257 | 0.209 | 1.581 | 0.008 | 0.496 | 6.772 | 0.091 | 0.133 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_without_human_capital | Six-region FE + expanded controls without human capital | 12 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 5.997 | 2.104 | 2.851 | 0.005 | 8.127 | 0.078 | 0.599 | 12.891 | 0.279 | 0.193 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_without_human_capital | State FE + expanded controls without human capital | 13 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 1.368 | 1.017 | 1.346 | 0.179 | 1.810 | 0.010 | 0.490 | 6.764 | 0.099 | 0.129 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_scale_geography | Six-region FE + through basic scale geography | 14 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography | 3 | 5.739 | 2.821 | 2.035 | 0.042 | 4.140 | 0.071 | 0.651 | 14.059 | 0.266 | 0.228 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_social_composition | Six-region FE + through social composition | 15 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition | 6 | 4.892 | 2.349 | 2.083 | 0.038 | 4.338 | 0.049 | 0.621 | 13.766 | 0.221 | 0.208 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_human_capital | Six-region FE + through human capital | 16 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital | 8 | 3.203 | 2.290 | 1.399 | 0.162 | 1.957 | 0.023 | 0.605 | 12.673 | 0.153 | 0.197 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_demography | Six-region FE + through demography | 17 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography | 9 | 3.226 | 2.283 | 1.413 | 0.158 | 1.997 | 0.024 | 0.603 | 12.673 | 0.153 | 0.196 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_economic_structure | Six-region FE + through economic structure | 18 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 4.033 | 2.506 | 1.609 | 0.108 | 2.590 | 0.035 | 0.581 | 12.587 | 0.186 | 0.182 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_development | Six-region FE + through basic development | 19 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.063 | 2.131 | 2.376 | 0.018 | 5.644 | 0.057 | 0.574 | 12.139 | 0.239 | 0.177 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_scale_geography | State FE + through basic scale geography | 20 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography | 3 | 1.630 | 0.822 | 1.983 | 0.048 | 3.933 | 0.015 | 0.519 | 6.971 | 0.121 | 0.146 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_social_composition | State FE + through social composition | 21 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition | 6 | 1.520 | 0.957 | 1.588 | 0.113 | 2.523 | 0.012 | 0.503 | 6.941 | 0.110 | 0.137 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_human_capital | State FE + through human capital | 22 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital | 8 | 0.879 | 0.867 | 1.013 | 0.311 | 1.027 | 0.004 | 0.497 | 6.582 | 0.066 | 0.133 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_demography | State FE + through demography | 23 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography | 9 | 0.835 | 0.858 | 0.973 | 0.331 | 0.946 | 0.004 | 0.496 | 6.572 | 0.063 | 0.133 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_economic_structure | State FE + through economic structure | 24 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 0.982 | 0.922 | 1.064 | 0.288 | 1.133 | 0.005 | 0.489 | 6.552 | 0.073 | 0.129 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_development | State FE + through basic development | 25 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.916 | 0.909 | 1.007 | 0.314 | 1.015 | 0.005 | 0.484 | 6.546 | 0.068 | 0.126 | 573 | 35 | 6 | estimated | NA |

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
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 5.644 | 0.018 | 0.057 | 573 | 35 | 6 |
| region_expanded\_\_distant_share | 32 | region_expanded | Six-region FE + expanded controls | distant_share | Share speaking languages at distance three or higher | region | ling_share_distance_ge3 |  | 1 | 0.212 | 0.645 | 0.002 | 573 | 35 | 6 |
| region_expanded\_\_top3_legacy | 33 | region_expanded | Six-region FE + expanded controls | top3_legacy | Legacy top-three weighted mean | region | ling_distance_top3_legacy |  | 1 | 6.054 | 0.014 | 0.054 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu | 34 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | region | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 5.777 | 0.017 | 0.058 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_shastry | 35 | region_expanded | Six-region FE + expanded controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | region | ling_distance_nonzero_mean | hindi_urdu_share;native_english_share | 1 | 5.634 | 0.018 | 0.057 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_sensitivity_low | 36 | region_expanded | Six-region FE + expanded controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | region | ling_distance_nonzero_mean_sensitivity_low | hindi_urdu_share;native_english_share | 1 | 5.756 | 0.017 | 0.059 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_sensitivity_high | 37 | region_expanded | Six-region FE + expanded controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | region | ling_distance_nonzero_mean_sensitivity_high | hindi_urdu_share;native_english_share | 1 | 0.464 | 0.496 | 0.003 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu_separate | 38 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | region | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 4.984 | 0.026 | 0.053 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all | 39 | region_expanded | Six-region FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 20.536 | 0.000 | 0.200 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all_unmapped | 40 | region_expanded | Six-region FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 15.921 | 0.000 | 0.196 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_mapped | 41 | region_expanded | Six-region FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | region | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 14.606 | 0.000 | 0.192 | 573 | 35 | 6 |
| region_expanded\_\_glottolog_mean | 42 | region_expanded | Six-region FE + expanded controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | region | ling_distance_glottolog_nonhindi_mean |  | 1 | 0.000 | 0.993 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_glottolog_mean_shastry | 43 | region_expanded | Six-region FE + expanded controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | region | ling_distance_glottolog_nonhindi_mean | hindi_urdu_share;native_english_share | 1 | 0.004 | 0.949 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_dyen_noncognate | 44 | region_expanded | Six-region FE + expanded controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | region | ling_distance_dyen_noncognate_pct |  | 1 | 0.071 | 0.790 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_dyen_noncognate_shastry | 45 | region_expanded | Six-region FE + expanded controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | region | ling_distance_dyen_noncognate_pct | hindi_urdu_share;native_english_share | 1 | 0.035 | 0.851 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.015 | 0.314 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_distant_share | 62 | state_expanded | State FE + expanded controls | distant_share | Share speaking languages at distance three or higher | state | ling_share_distance_ge3 |  | 1 | 0.796 | 0.373 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_top3_legacy | 63 | state_expanded | State FE + expanded controls | top3_legacy | Legacy top-three weighted mean | state | ling_distance_top3_legacy |  | 1 | 2.100 | 0.148 | 0.012 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu | 64 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | state | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 1.130 | 0.288 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_shastry | 65 | state_expanded | State FE + expanded controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | state | ling_distance_nonzero_mean | hindi_urdu_share;native_english_share | 1 | 1.088 | 0.297 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_sensitivity_low | 66 | state_expanded | State FE + expanded controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | state | ling_distance_nonzero_mean_sensitivity_low | hindi_urdu_share;native_english_share | 1 | 1.070 | 0.301 | 0.004 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_sensitivity_high | 67 | state_expanded | State FE + expanded controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | state | ling_distance_nonzero_mean_sensitivity_high | hindi_urdu_share;native_english_share | 1 | 1.110 | 0.293 | 0.001 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu_separate | 68 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | state | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 1.007 | 0.316 | 0.004 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all | 69 | state_expanded | State FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 3.228 | 0.007 | 0.035 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 3.724 | 0.003 | 0.033 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 0.985 | 0.426 | 0.023 | 573 | 35 | 6 |
| state_expanded\_\_glottolog_mean | 72 | state_expanded | State FE + expanded controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | state | ling_distance_glottolog_nonhindi_mean |  | 1 | 0.268 | 0.605 | 0.001 | 573 | 35 | 6 |
| state_expanded\_\_glottolog_mean_shastry | 73 | state_expanded | State FE + expanded controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | state | ling_distance_glottolog_nonhindi_mean | hindi_urdu_share;native_english_share | 1 | 0.105 | 0.746 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_dyen_noncognate | 74 | state_expanded | State FE + expanded controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | state | ling_distance_dyen_noncognate_pct |  | 1 | 0.177 | 0.674 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_dyen_noncognate_shastry | 75 | state_expanded | State FE + expanded controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | state | ling_distance_dyen_noncognate_pct | hindi_urdu_share;native_english_share | 1 | 0.165 | 0.685 | 0.000 | 573 | 35 | 6 |

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

| specification_id | sequence | adjustment_id | adjustment | construction_id | construction | fixed_effect | excluded_instruments | included_language_controls | n_excluded_instruments | joint_excluded_f | joint_excluded_p | partial_r_squared | n | n_states | n_regions | minimum_mapped_share | coverage_variable | coverage_sample_n |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.015 | 0.314 | 0.005 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.758 | 0.384 | 0.002 | 486 | 32 | 6 | 90 | ling_mapped_speaker_share | 486 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.849 | 0.357 | 0.004 | 461 | 32 | 6 | 95 | ling_mapped_speaker_share | 461 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.118 | 0.732 | 0.000 | 378 | 25 | 6 | 99 | ling_mapped_speaker_share | 378 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 3.724 | 0.003 | 0.033 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 2.820 | 0.016 | 0.027 | 486 | 32 | 6 | 90 | ling_mapped_speaker_share | 486 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 5.235 | 0.000 | 0.050 | 461 | 32 | 6 | 95 | ling_mapped_speaker_share | 461 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 2.158 | 0.058 | 0.023 | 378 | 25 | 6 | 99 | ling_mapped_speaker_share | 378 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 0.985 | 0.426 | 0.023 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 3.421 | 0.005 | 0.025 | 486 | 32 | 6 | 90 | ling_mapped_speaker_share | 486 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 7.502 | 0.000 | 0.049 | 461 | 32 | 6 | 95 | ling_mapped_speaker_share | 461 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 5.632 | 0.000 | 0.026 | 378 | 25 | 6 | 99 | ling_mapped_speaker_share | 378 |

Mapping-coverage sensitivity under state fixed effects and expanded
controls

``` r
analysis_table(unmapped_languages, "Largest unmapped C-16 language-state cells", max_rows = 20)
```

| mother_tongue | canonical_language | state_code_2001 | unmapped_speakers | n_districts | share_of_unmapped_speakers |
|:---|:---|:---|:---|:---|:---|
| Others | Hindi | 10 | 12613801 | 37 | 32.8687005766315 |
| Wagdi | Bhili/Bhilodi | 8 | 2111080 | 27 | 5.50099501437475 |
| Garhwali | Hindi | 5 | 2102799 | 12 | 5.47941660914423 |
| Pahari | Hindi | 2 | 2065071 | 12 | 5.3811060098764 |
| Kumauni | Hindi | 5 | 1874754 | 12 | 4.88518313241522 |
| Kangri | Hindi | 2 | 1119555 | 12 | 2.91730605818743 |
| Nepali | Nepali | 19 | 1022683 | 18 | 2.66487962762463 |
| Surjapuri | Hindi | 10 | 928458 | 11 | 2.41935067787879 |
| Others | Hindi | 20 | 800695 | 18 | 2.086429317238 |
| Barel | Bhili/Bhilodi | 23 | 635548 | 35 | 1.65609374320056 |
| Mandeali | Hindi | 2 | 611506 | 12 | 1.59344575158698 |
| Nepali | Nepali | 18 | 562543 | 22 | 1.46585929399711 |
| Halabi | Halabi | 22 | 544661 | 11 | 1.41926286333268 |
| Pahari | Hindi | 1 | 529827 | 11 | 1.38060882841064 |
| Vasava | Bhili/Bhilodi | 24 | 412943 | 18 | 1.07603567094613 |
| Brajbhasha | Hindi | 8 | 399694 | 27 | 1.04151178604103 |
| Nepali | Nepali | 11 | 338603 | 4 | 0.882322514946062 |
| Kurmali Thar | Hindi | 19 | 306756 | 17 | 0.799336465993491 |
| Surjapuri | Hindi | 19 | 288077 | 18 | 0.750663234342627 |
| Gamti/Gavit | Bhili/Bhilodi | 24 | 282753 | 16 | 0.736790099522284 |
| Table truncated in rendered note; full CSV has 1714 rows. |  |  |  |  |  |

Largest unmapped C-16 language-state cells

``` r
analysis_table(distance4_languages, "Distance-four speaker composition by language and state", max_rows = 20)
```

| mother_tongue | canonical_language | state_code_2001 | speakers | n_districts | national_language_speakers | speaker_share_of_distance4 |
|:---|:---|:---|:---|:---|:---|:---|
| Kashmiri | Kashmiri | 1 | 5262911 | 12 | 5320696 | 66.7212861616977 |
| Kachchhi | Sindhi | 24 | 660775 | 26 | 800325 | 8.3770669622754 |
| Sindhi | Sindhi | 27 | 523478 | 34 | 1585750 | 6.63646514967728 |
| Sindhi | Sindhi | 8 | 299532 | 28 | 1585750 | 3.79735858854266 |
| Sindhi | Sindhi | 24 | 297234 | 26 | 1585750 | 3.76822537393964 |
| Sindhi | Sindhi | 23 | 252968 | 45 | 1585750 | 3.20703700247873 |
| Kachchhi | Sindhi | 27 | 132127 | 34 | 800325 | 1.67505841856088 |
| Sindhi | Sindhi | 22 | 87545 | 17 | 1585750 | 1.10986391315107 |
| Siraji | Kashmiri | 1 | 46302 | 7 | 87179 | 0.58700004462529 |
| Others | Kashmiri | 1 | 44435 | 10 | 60528 | 0.563330892465223 |
| Siraji | Kashmiri | 2 | 40863 | 8 | 87179 | 0.51804636567585 |
| Sindhi | Sindhi | 7 | 40453 | 8 | 1585750 | 0.512848533653554 |
| Sindhi | Sindhi | 9 | 33402 | 62 | 1585750 | 0.423458500509134 |
| Kishtwari | Kashmiri | 1 | 33396 | 8 | 33426 | 0.423382434674661 |
| Kashmiri | Kashmiri | 7 | 20458 | 8 | 5320696 | 0.259359140273512 |
| Sindhi | Sindhi | 29 | 12942 | 24 | 1585750 | 0.164074004957464 |
| Sindhi | Sindhi | 28 | 10131 | 21 | 1585750 | 0.128437161507037 |
| Others | Sindhi | 8 | 9347 | 10 | 60528 | 0.11849789246928 |
| Kashmiri | Kashmiri | 2 | 9298 | 13 | 5320696 | 0.11787668815442 |
| Sindhi | Sindhi | 33 | 7105 | 19 | 1585750 | 0.0900746256546733 |
| Table truncated in rendered note; full CSV has 162 rows. |  |  |  |  |  |  |

Distance-four speaker composition by language and state

``` r
analysis_table(distance4_leave_one_out, "Distance-four leave-one-language-out joint tests", max_rows = 15)
```

| omitted_distance4_language | joint_excluded_f | joint_excluded_p | partial_r_squared | n |
|:---|---:|---:|---:|---:|
| Kachchhi | 4.070 | 0.001 | 0.033 | 573 |
| Kashmiri | 0.415 | 0.838 | 0.010 | 573 |
| Kishtwari | 3.700 | 0.003 | 0.030 | 573 |
| Others | 3.690 | 0.003 | 0.029 | 573 |
| Sindhi | 3.835 | 0.002 | 0.034 | 573 |
| Siraji | 4.317 | 0.001 | 0.031 | 573 |

Distance-four leave-one-language-out joint tests

``` r
analysis_table(weak_iv_outcomes, "Weak-IV-aware exploratory outcome estimates", max_rows = 10)
```

| specification_id | adjustment_id | construction_id | estimate_2sls | std_error_clustered | p_value_clustered | effective_f | effective_f_critical_value | effective_f_p_value | effective_f_df | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_n_components | ar_95_disconnected | ar_95_contains_zero | ar_95_grid_accepted_min | ar_95_grid_accepted_max | ar_95_left_truncated | ar_95_right_truncated | ar_95_components | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.00485413648408047 | 0.00212977287302198 | 0.0230245301900153 | 11.7516308965675 | 23.1085112116065 | 0.395201426672468 | 1 | 4.21400545178731 | 0.040546721269239 | 4.21400545178731 | 0.040546721269239 | -0.0104918313474366 | -0.000552201649865081 | FALSE | 1 | FALSE | FALSE | -0.0104918313474366 | -0.000552201649865081 | FALSE | FALSE | \[-0.0104918, -0.000552202\] | 573 | estimated | NA |
| unadjusted\_\_distant_share | unadjusted | distant_share | -0.00579673169584968 | 0.00415738697682759 | 0.163762983123477 | 4.09060847555788 | 23.1085112116065 | 0.872805370821969 | 1 | 2.77927959690084 | 0.096039453101219 | 2.77927959690084 | 0.0960394531012188 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.110440329973016 | 0.00110440329973016 | TRUE | FALSE | \[grid\<= -0.11044, 0.0011044\] | 573 | estimated | NA |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | -0.00454354652968262 | 0.00180920172400751 | 0.0123018427678873 | 16.3244099547573 | 23.1085112116065 | 0.189952841265447 | 1 | 4.43756940703664 | 0.0355919668502914 | 4.43756940703664 | 0.0355919668502913 | -0.00828302474797624 | -0.000552201649865081 | FALSE | 1 | FALSE | FALSE | -0.00828302474797624 | -0.000552201649865081 | FALSE | FALSE | \[-0.00828302, -0.000552202\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | -0.00439506839776046 | 0.00268512986366392 | 0.102220496297055 | 9.52941864325911 | 23.1085112116065 | 0.530012790451569 | 1 | 2.34247551281997 | 0.126444122729268 | 2.34247551281998 | 0.126444122729267 | -0.0115962346471667 | 0.00165660494959524 | FALSE | 1 | FALSE | TRUE | -0.0115962346471667 | 0.00165660494959524 | FALSE | FALSE | \[-0.0115962, 0.0016566\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_shastry | unadjusted | nonzero_mean_shastry | -0.00436132976683922 | 0.00263994883573457 | 0.0990758704367839 | 9.69601598134048 | 23.1085112116065 | 0.519315225521698 | 1 | 2.42765287988453 | 0.119767273140065 | 2.42765287988457 | 0.119767273140063 | -0.0115962346471667 | 0.00110440329973016 | FALSE | 1 | FALSE | TRUE | -0.0115962346471667 | 0.00110440329973016 | FALSE | FALSE | \[-0.0115962, 0.0011044\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_sensitivity_low | unadjusted | nonzero_mean_sensitivity_low | -0.00432009449057584 | 0.00265079710931034 | 0.103711131797404 | 9.82914235113751 | 23.1085112116065 | 0.510822517241498 | 1 | 2.35310013464267 | 0.125589944747971 | 2.35310013464269 | 0.12558994474797 | -0.0115962346471667 | 0.00165660494959524 | FALSE | 1 | FALSE | TRUE | -0.0115962346471667 | 0.00165660494959524 | FALSE | FALSE | \[-0.0115962, 0.0016566\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_sensitivity_high | unadjusted | nonzero_mean_sensitivity_high | -0.00369433944166042 | 0.00310993899699249 | 0.235362988740311 | 8.90424589559872 | 23.1085112116065 | 0.570748204013008 | 1 | 1.28636681224253 | 0.257196178956275 | 1.28636681224252 | 0.257196178956275 | -0.0121484362970318 | 0.00386541154905558 | FALSE | 1 | FALSE | TRUE | -0.0121484362970318 | 0.00386541154905558 | FALSE | FALSE | \[-0.0121484, 0.00386541\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | -0.0045433181160396 | 0.00254444949768066 | 0.0746997743584913 | 10.5278142719394 | 23.1085112116065 | 0.467171599617833 | 1 | 2.71396708922478 | 0.100025427805109 | 2.71396708922479 | 0.100025427805108 | -0.0110440329973017 | 0.00110440329973016 | FALSE | 1 | FALSE | TRUE | -0.0110440329973017 | 0.00110440329973016 | FALSE | FALSE | \[-0.011044, 0.0011044\] | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | -0.00100725267351976 | 0.0013117679189808 | 0.442888432193402 | 9.25351880362562 | 19.3637841388305 | 0.610728092669059 | 1.96941262684311 | 16.3153670903423 | 4.70991452706161e-15 | 16.3153670903422 | 4.70991452706256e-15 | NA | NA | TRUE | 0 | FALSE | FALSE | NA | NA | FALSE | FALSE | NA | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | -0.00153631733913764 | 0.00119030773984092 | 0.197335661027075 | 13.3629987698474 | 20.4246026454299 | 0.290598934499384 | 1.58039812063326 | 22.3724827262348 | 1.70576842067905e-20 | 22.3724827262348 | 1.70576842067908e-20 | NA | NA | TRUE | 0 | FALSE | FALSE | NA | NA | FALSE | FALSE | NA | 573 | estimated | NA |
| Table truncated in rendered note; full CSV has 93 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Weak-IV-aware exploratory outcome estimates

``` r
analysis_table(iv_diagnostic_applicability, "IV diagnostic applicability and implementation status", max_rows = 30)
```

| specification_id | diagnostic_id | diagnostic_family | applicable | implemented | will_run | reason |
|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | effective_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_distant_share | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | effective_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_distant_share | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_top3_legacy | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | effective_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | monotonicity_shape | monotonicity | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_top3_legacy | overidentification | overidentification | FALSE | TRUE | FALSE | exactly_identified |
| unadjusted\_\_nonzero_mean_hindi_urdu | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | effective_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | balance_covariates | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | balance_joint | independence | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | anderson_rubin | weak_identification | TRUE | TRUE | TRUE | NA |
| Table truncated in rendered note; full CSV has 744 rows. |  |  |  |  |  |  |

IV diagnostic applicability and implementation status

``` r
analysis_table(iv_joint_balance, "Joint holdout-covariate balance tests", max_rows = 30)
```

| specification_id | adjustment_id | construction_id | fixed_effect | instrument | tested_covariates | n_tested_covariates | joint_f | joint_p | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 7.99307457370493 | 8.90174408009734e-15 | 573 | estimated |  |
| unadjusted\_\_distant_share | unadjusted | distant_share | none | ling_share_distance_ge3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 50.5411642024887 | 1.65742599834248e-85 | 573 | estimated |  |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | none | ling_distance_top3_legacy | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 16.1760554308659 | 1.54201329453102e-31 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.79296094809036 | 4.56458363405227e-10 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_shastry | unadjusted | nonzero_mean_shastry | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.48007529472653 | 1.55186901800352e-11 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_sensitivity_low | unadjusted | nonzero_mean_sensitivity_low | none | ling_distance_nonzero_mean_sensitivity_low | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.36114512118508 | 2.7893095533509e-11 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_sensitivity_high | unadjusted | nonzero_mean_sensitivity_high | none | ling_distance_nonzero_mean_sensitivity_high | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 7.13883140926004 | 6.0280461316441e-13 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.79715437222932 | 4.48391069228847e-10 | 573 | estimated |  |
| unadjusted\_\_glottolog_mean | unadjusted | glottolog_mean | none | ling_distance_glottolog_nonhindi_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.9668761503136 | 1.39559463586046e-12 | 573 | estimated |  |
| unadjusted\_\_glottolog_mean_shastry | unadjusted | glottolog_mean_shastry | none | ling_distance_glottolog_nonhindi_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.08232630991346 | 1.10194324361083e-10 | 573 | estimated |  |
| unadjusted\_\_dyen_noncognate | unadjusted | dyen_noncognate | none | ling_distance_dyen_noncognate_pct | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 17.3261028318373 | 9.29403984472188e-34 | 573 | estimated |  |
| unadjusted\_\_dyen_noncognate_shastry | unadjusted | dyen_noncognate_shastry | none | ling_distance_dyen_noncognate_pct | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 11.5781986268508 | 2.62746348234374e-22 | 573 | estimated |  |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 13.256332255539 | 2.54250295822459e-10 | 573 | estimated |  |
| region_main\_\_distant_share | region_main | distant_share | region | ling_share_distance_ge3 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.41287262333319 | 0.0480174902151429 | 573 | estimated |  |
| region_main\_\_top3_legacy | region_main | top3_legacy | region | ling_distance_top3_legacy | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 0.643356743376577 | 0.631773913601769 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 12.2917567781619 | 1.39389900296397e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_shastry | region_main | nonzero_mean_shastry | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 11.608678739785 | 4.66777041616539e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | nonzero_mean_sensitivity_low | region | ling_distance_nonzero_mean_sensitivity_low | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 11.6965008297178 | 3.99624285168773e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | nonzero_mean_sensitivity_high | region | ling_distance_nonzero_mean_sensitivity_high | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 5.32052853836261 | 0.000328479471183337 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 14.6490790761642 | 2.21897221832896e-11 | 573 | estimated |  |
| region_main\_\_glottolog_mean | region_main | glottolog_mean | region | ling_distance_glottolog_nonhindi_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.26374093934227 | 0.0611510902689427 | 573 | estimated |  |
| region_main\_\_glottolog_mean_shastry | region_main | glottolog_mean_shastry | region | ling_distance_glottolog_nonhindi_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.41873233711914 | 0.047565812153335 | 573 | estimated |  |
| region_main\_\_dyen_noncognate | region_main | dyen_noncognate | region | ling_distance_dyen_noncognate_pct | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 5.71031199888573 | 0.000165060699621946 | 573 | estimated |  |
| region_main\_\_dyen_noncognate_shastry | region_main | dyen_noncognate_shastry | region | ling_distance_dyen_noncognate_pct | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 6.09946491908444 | 8.29891959859627e-05 | 573 | estimated |  |
| region_expanded\_\_nonzero_mean |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| region_expanded\_\_distant_share |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| region_expanded\_\_top3_legacy |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| region_expanded\_\_nonzero_mean_hindi_urdu |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| region_expanded\_\_nonzero_mean_shastry |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| region_expanded\_\_nonzero_mean_sensitivity_low |  |  |  |  |  | 0 | NA | NA | NA | not_applicable | All diagnostic balance covariates are already included as nuisance controls. |
| Table truncated in rendered note; full CSV has 78 rows. |  |  |  |  |  |  |  |  |  |  |  |

Joint holdout-covariate balance tests

``` r
analysis_table(iv_specification_registry, "De-duplicated IV diagnostic specification registry", max_rows = 30)
```

| specification_id | adjustment_id | adjustment | construction_id | construction | outcome | treatment | fixed_effect | controls | included_language_controls | excluded_instruments | mapping_coverage_variable | n_endogenous | n_excluded_instruments | panel_variant | sample_rule | cluster | tier | sequence |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 1 |
| unadjusted\_\_distant_share | unadjusted | Unadjusted | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_share_distance_ge3 | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 2 |
| unadjusted\_\_top3_legacy | unadjusted | Unadjusted | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_top3_legacy | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 3 |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | Unadjusted | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 4 |
| unadjusted\_\_nonzero_mean_shastry | unadjusted | Unadjusted | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 5 |
| unadjusted\_\_nonzero_mean_sensitivity_low | unadjusted | Unadjusted | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_low | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 6 |
| unadjusted\_\_nonzero_mean_sensitivity_high | unadjusted | Unadjusted | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_high | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 7 |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | Unadjusted | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_share;urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 8 |
| unadjusted\_\_distance_shares_all | unadjusted | Unadjusted | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 9 |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | Unadjusted | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | ling_unmapped_speaker_share;native_english_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 10 |
| unadjusted\_\_distance_shares_mapped | unadjusted | Unadjusted | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 11 |
| unadjusted\_\_glottolog_mean | unadjusted | Unadjusted | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 12 |
| unadjusted\_\_glottolog_mean_shastry | unadjusted | Unadjusted | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share;native_english_share | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 13 |
| unadjusted\_\_dyen_noncognate | unadjusted | Unadjusted | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | none |  |  | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 14 |
| unadjusted\_\_dyen_noncognate_shastry | unadjusted | Unadjusted | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | none |  | hindi_urdu_share;native_english_share | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 15 |
| region_main\_\_nonzero_mean | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 16 |
| region_main\_\_distant_share | region_main | Six-region FE + main controls | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_ge3 | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 17 |
| region_main\_\_top3_legacy | region_main | Six-region FE + main controls | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_top3_legacy | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 18 |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 19 |
| region_main\_\_nonzero_mean_shastry | region_main | Six-region FE + main controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 20 |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | Six-region FE + main controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_low | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 21 |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | Six-region FE + main controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_high | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 22 |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_share;urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 23 |
| region_main\_\_distance_shares_all | region_main | Six-region FE + main controls | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | A | 24 |
| region_main\_\_distance_shares_all_unmapped | region_main | Six-region FE + main controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | ling_unmapped_speaker_share;native_english_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | A | 25 |
| region_main\_\_distance_shares_mapped | region_main | Six-region FE + main controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | A | 26 |
| region_main\_\_glottolog_mean | region_main | Six-region FE + main controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 27 |
| region_main\_\_glottolog_mean_shastry | region_main | Six-region FE + main controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 28 |
| region_main\_\_dyen_noncognate | region_main | Six-region FE + main controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 29 |
| region_main\_\_dyen_noncognate_shastry | region_main | Six-region FE + main controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | A | 30 |
| Table truncated in rendered note; full CSV has 93 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

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
| unadjusted\_\_distance_shares_all | 1 | 5 | sargan | estimated | 15.364 | 4 | 0.004 | NA |
| unadjusted\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 19.255 | 4 | 0.001 | NA |
| unadjusted\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 20.567 | 4 | 0.000 | NA |
| region_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 10.731 | 4 | 0.030 | NA |
| region_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 18.250 | 4 | 0.001 | NA |
| region_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 10.157 | 4 | 0.038 | NA |
| region_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 11.141 | 4 | 0.025 | NA |
| region_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 18.355 | 4 | 0.001 | NA |
| region_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 10.909 | 4 | 0.028 | NA |
| state_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 10.741 | 4 | 0.030 | NA |
| state_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 8.979 | 4 | 0.062 | NA |
| state_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 8.508 | 4 | 0.075 | NA |
| state_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 9.968 | 4 | 0.041 | NA |
| state_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 8.399 | 4 | 0.078 | NA |
| state_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 7.607 | 4 | 0.107 | NA |

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
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| region_main\_\_nonzero_mean | region_main | nonzero_mean | ling_distance_nonzero_mean | region | 4.05042744579001 | 0.20438226056921 | 0.0620610606698453 | 10 | 0.777777777777778 | 2 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_distant_share | region_main | distant_share | ling_share_distance_ge3 | region | -0.0535597746027639 | -0.0639326833299506 | 0.0134113145771736 | 10 | 0.333333333333333 | 6 | 26 | 0.461538461538462 | 573 | estimated | NA |
| region_main\_\_top3_legacy | region_main | top3_legacy | ling_distance_top3_legacy | region | 3.88485505026667 | 0.285620585518427 | 0.101721865022242 | 10 | 0.777777777777778 | 2 | 26 | 0.346153846153846 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | region | 4.05968878641937 | 0.204523671960781 | 0.0617377149830833 | 10 | 0.777777777777778 | 2 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_shastry | region_main | nonzero_mean_shastry | ling_distance_nonzero_mean | region | 4.01762118327476 | 0.203102158982634 | 0.0618532413328876 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | region | 4.06474704723098 | 0.207098545688764 | 0.0619180779363854 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | nonzero_mean_sensitivity_high | ling_distance_nonzero_mean_sensitivity_high | region | 0.516248528597235 | 0.107551559664955 | 0.013049750067307 | 10 | 0.555555555555556 | 4 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | region | 3.96284957622603 | 0.195660169226764 | 0.0681970180094382 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_glottolog_mean | region_main | glottolog_mean | ling_distance_glottolog_nonhindi_mean | region | -0.220596275142146 | -0.00597365857230498 | 0.00738595786709073 | 10 | 0.444444444444444 | 5 | 26 | 0.5 | 573 | estimated | NA |
| region_main\_\_glottolog_mean_shastry | region_main | glottolog_mean_shastry | ling_distance_glottolog_nonhindi_mean | region | -0.202179430300114 | 0.000656858146323093 | 0.00759608249799926 | 10 | 0.444444444444444 | 5 | 26 | 0.461538461538462 | 573 | estimated | NA |
| region_main\_\_dyen_noncognate | region_main | dyen_noncognate | ling_distance_dyen_noncognate_pct | region | -0.0224130752794601 | 0.0247397858175537 | 0.00959707889975792 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| region_main\_\_dyen_noncognate_shastry | region_main | dyen_noncognate_shastry | ling_distance_dyen_noncognate_pct | region | -0.0279491238120206 | 0.0210947907921475 | 0.0077787199625482 | 10 | 0.444444444444444 | 5 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | ling_distance_nonzero_mean | state | 0.839457291252968 | 0.0658186150460445 | 0.0669542830409092 | 10 | 0.666666666666667 | 3 | 26 | 0.346153846153846 | 573 | estimated | NA |
| state_main\_\_distant_share | state_main | distant_share | ling_share_distance_ge3 | state | 0.0257007835864833 | 0.0745557596534483 | 0.0233027187777007 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_top3_legacy | state_main | top3_legacy | ling_distance_top3_legacy | state | 1.25188769799169 | 0.0983478914139111 | 0.0414662483706367 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu | state_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.865121301723658 | 0.0707632936594755 | 0.0689332159295629 | 10 | 0.777777777777778 | 2 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_shastry | state_main | nonzero_mean_shastry | ling_distance_nonzero_mean | state | 0.847143248604903 | 0.0707358023154218 | 0.066409532712915 | 10 | 0.666666666666667 | 3 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_sensitivity_low | state_main | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | state | 0.800507979103332 | 0.0673124602380125 | 0.0669216151299863 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_sensitivity_high | state_main | nonzero_mean_sensitivity_high | ling_distance_nonzero_mean_sensitivity_high | state | 0.43068520703726 | 0.0518660244110378 | 0.0111462163462467 | 10 | 0.555555555555556 | 4 | 26 | 0.307692307692308 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu_separate | state_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | state | 0.86879709695795 | 0.0706204789974424 | 0.0653244263592586 | 10 | 0.555555555555556 | 4 | 26 | 0.269230769230769 | 573 | estimated | NA |
| state_main\_\_glottolog_mean | state_main | glottolog_mean | ling_distance_glottolog_nonhindi_mean | state | -0.115015179894935 | -0.0260601995374055 | 0.0158401253915149 | 10 | 0.333333333333333 | 6 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_glottolog_mean_shastry | state_main | glottolog_mean_shastry | ling_distance_glottolog_nonhindi_mean | state | -0.0791717440431592 | -0.0089921571213871 | 0.0166468721106128 | 10 | 0.444444444444444 | 5 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_dyen_noncognate | state_main | dyen_noncognate | ling_distance_dyen_noncognate_pct | state | 0.0138082256397622 | 0.0450290355794123 | 0.0126619983289635 | 10 | 0.444444444444444 | 5 | 26 | 0.461538461538462 | 573 | estimated | NA |
| state_main\_\_dyen_noncognate_shastry | state_main | dyen_noncognate_shastry | ling_distance_dyen_noncognate_pct | state | 0.0133020543226307 | 0.0426793229755814 | 0.0126029103083658 | 10 | 0.444444444444444 | 5 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | ling_distance_nonzero_mean | state | 0.915809783242566 | 0.054602720737314 | 0.0618365840076777 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_distant_share | state_expanded | distant_share | ling_share_distance_ge3 | state | 0.0280700358659057 | 0.0702568405926853 | 0.0236789647276127 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_top3_legacy | state_expanded | top3_legacy | ling_distance_top3_legacy | state | 1.25052924332589 | 0.0952039915135305 | 0.0454639060219402 | 10 | 0.666666666666667 | 3 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_hindi_urdu | state_expanded | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.936596191902617 | 0.0587840840043624 | 0.0619371130106938 | 10 | 0.666666666666667 | 3 | 26 | 0.346153846153846 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_shastry | state_expanded | nonzero_mean_shastry | ling_distance_nonzero_mean | state | 0.920798652343708 | 0.0577818299692901 | 0.0624514457002047 | 10 | 0.666666666666667 | 3 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_sensitivity_low | state_expanded | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | state | 0.89086567357758 | 0.0548267975161092 | 0.063079724377214 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
| Table truncated in rendered note; full CSV has 36 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

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
| state_expanded\_\_distance_shares_all | ling_share_distance_1 | 0.033 | 0.018 | 1.854 | 0.064 |
| state_expanded\_\_distance_shares_all | ling_share_distance_2 | -0.019 | 0.045 | -0.424 | 0.672 |
| state_expanded\_\_distance_shares_all | ling_share_distance_3 | -0.008 | 0.012 | -0.651 | 0.515 |
| state_expanded\_\_distance_shares_all | ling_share_distance_4 | 0.264 | 0.071 | 3.726 | 0.000 |
| state_expanded\_\_distance_shares_all | ling_share_distance_5 | 0.022 | 0.051 | 0.426 | 0.670 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_1 | 0.032 | 0.017 | 1.859 | 0.064 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_2 | -0.020 | 0.052 | -0.380 | 0.704 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_3 | -0.011 | 0.020 | -0.550 | 0.583 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_4 | 0.262 | 0.067 | 3.917 | 0.000 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_5 | 0.017 | 0.050 | 0.347 | 0.728 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_1 | 0.019 | 0.019 | 0.952 | 0.342 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_2 | 0.016 | 0.041 | 0.381 | 0.703 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_3 | -0.003 | 0.012 | -0.247 | 0.805 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_4 | 0.188 | 0.093 | 2.010 | 0.045 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_5 | 0.039 | 0.041 | 0.947 | 0.344 |

State-expanded distance-share coefficients

``` r
analysis_table(first_stage_vif, "Main and expanded-control VIF/GVIF diagnostics", max_rows = 40)
```

| term | model_scope | df | vif | gvif | gvif_scaled | status | reason | specification_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| ling_distance_nonzero_mean | model_regressors | 1 | 5.37411533753367 | 5.37411533753367 | 2.31821382480859 | estimated | NA | region_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 2.48475573348372 | 2.48475573348372 | 1.57631079850508 | estimated | NA | region_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 3.19290757268839 | 3.19290757268839 | 1.7868708886454 | estimated | NA | region_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.21760738483294 | 4.21760738483294 | 2.05368142242971 | estimated | NA | region_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 2.58155832527255 | 2.58155832527255 | 1.60672285266394 | estimated | NA | region_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 3.25847325853406 | 3.25847325853406 | 1.80512416706831 | estimated | NA | region_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 1.71778683284222 | 1.71778683284222 | 1.31064367119451 | estimated | NA | region_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 4.52464025205895 | 4.52464025205895 | 2.12712017809501 | estimated | NA | region_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 3.99036395707579 | 3.99036395707579 | 1.9975895366856 | estimated | NA | region_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.55561556737286 | 4.55561556737286 | 2.13438880417155 | estimated | NA | region_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 3.47885220788373 | 3.47885220788373 | 1.86516814466785 | estimated | NA | region_fe_census_controls |
| factor(region) | model_regressors | 5 | NA | 37.6732540146115 | 1.43748497291406 | estimated | NA | region_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 5.63483867613971 | 5.63483867613971 | 2.37378151398559 | estimated | NA | region_fe_expanded_controls |
| log_population_2001 | model_regressors | 1 | 2.53581194970406 | 2.53581194970406 | 1.59242329476307 | estimated | NA | region_fe_expanded_controls |
| urban_share_2001 | model_regressors | 1 | 3.21520779790047 | 3.21520779790047 | 1.79310005239542 | estimated | NA | region_fe_expanded_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 5.09829107444858 | 5.09829107444858 | 2.25793956394953 | estimated | NA | region_fe_expanded_controls |
| sc_share_2001 | model_regressors | 1 | 2.67197213144836 | 2.67197213144836 | 1.63461681486774 | estimated | NA | region_fe_expanded_controls |
| st_share_2001 | model_regressors | 1 | 3.30440860027322 | 3.30440860027322 | 1.81780323475156 | estimated | NA | region_fe_expanded_controls |
| muslim_share_2001 | model_regressors | 1 | 1.91633170718003 | 1.91633170718003 | 1.38431633204988 | estimated | NA | region_fe_expanded_controls |
| dependency_ratio_2001 | model_regressors | 1 | 5.22796013037653 | 5.22796013037653 | 2.28647329535609 | estimated | NA | region_fe_expanded_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.93749973225823 | 4.93749973225823 | 2.22204854408229 | estimated | NA | region_fe_expanded_controls |
| log_population_density_2001 | model_regressors | 1 | 3.64981918529372 | 3.64981918529372 | 1.9104499954968 | estimated | NA | region_fe_expanded_controls |
| literacy_share_2001 | model_regressors | 1 | 3.2209774544285 | 3.2209774544285 | 1.79470818085518 | estimated | NA | region_fe_expanded_controls |
| worker_share_2001 | model_regressors | 1 | 3.48477618595882 | 3.48477618595882 | 1.86675552388598 | estimated | NA | region_fe_expanded_controls |
| cultivator_share_workers_2001 | model_regressors | 1 | 5.19558827283553 | 5.19558827283553 | 2.27938330976506 | estimated | NA | region_fe_expanded_controls |
| agricultural_labourer_share_workers_2001 | model_regressors | 1 | 3.92464530053917 | 3.92464530053917 | 1.98107175552507 | estimated | NA | region_fe_expanded_controls |
| factor(region) | model_regressors | 5 | NA | 62.1277508951286 | 1.51122302956186 | estimated | NA | region_fe_expanded_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.6641181382247 | 7.6641181382247 | 2.76841437256504 | estimated | NA | state_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 5.61926161428935 | 5.61926161428935 | 2.3704981785037 | estimated | NA | state_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 5.95646849482049 | 5.95646849482049 | 2.44058773553021 | estimated | NA | state_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 8.07543067368843 | 8.07543067368843 | 2.84173022535364 | estimated | NA | state_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 4.16640503114339 | 4.16640503114339 | 2.04117736396017 | estimated | NA | state_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 5.58710382884114 | 5.58710382884114 | 2.36370552921491 | estimated | NA | state_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 3.49235365030959 | 3.49235365030959 | 1.86878400311796 | estimated | NA | state_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 7.73071457612947 | 7.73071457612947 | 2.78041625950674 | estimated | NA | state_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 7.78460752704718 | 7.78460752704718 | 2.79009095318543 | estimated | NA | state_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 10.5297145744733 | 10.5297145744733 | 3.24495216828743 | estimated | NA | state_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 6.77166310233431 | 6.77166310233431 | 2.60224193770186 | estimated | NA | state_fe_census_controls |
| factor(state_code_2001) | model_regressors | 34 | NA | 63977.1702744449 | 1.17673003515029 | estimated | NA | state_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.92544671366014 | 7.92544671366014 | 2.81521699228677 | estimated | NA | state_fe_expanded_controls |
| Table truncated in rendered note; full CSV has 54 rows. |  |  |  |  |  |  |  |  |

Main and expanded-control VIF/GVIF diagnostics

``` r
analysis_table(first_stage_state_ranges, "State-by-state residual ranges", max_rows = 40)
```

| specification_id | state_code_2001 | n_districts | instrument_min | instrument_max | instrument_range | instrument_sd | treatment_min | treatment_max | treatment_range | treatment_sd |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| instrument_only | 1 | 11 | -1.95676929983649 | 0.886369467922413 | 2.8431387677589 | 1.28256593381945 | -1.7067413277146 | 63.6464584343633 | 65.3531997620779 | 22.1093140815969 |
| instrument_only | 2 | 12 | -2.02972739930956 | 1.86563277562347 | 3.89536017493303 | 1.32072078342093 | -13.1043090506517 | 22.906865628646 | 36.0111746792977 | 9.35701241783805 |
| instrument_only | 3 | 16 | -2.04468945407418 | -2.02132802418258 | 0.0233614298916001 | 0.00767986612439208 | -4.4451793315856 | 20.439254993468 | 24.8844343250536 | 6.95308526038906 |
| instrument_only | 4 | 1 | -1.74634422961262 | -1.74634422961262 | 0 | NA | 29.1105143054212 | 29.1105143054212 | 0 | NA |
| instrument_only | 5 | 12 | -1.18735057305129 | 1.05421770188751 | 2.2415682749388 | 0.684324584971318 | -13.3250486339961 | 29.5826517924148 | 42.9077004264109 | 11.4752595237954 |
| instrument_only | 6 | 19 | -2.03700438675809 | -1.07135970262856 | 0.96564468412953 | 0.283706846558088 | -9.93115723841601 | 27.3139251937577 | 37.2450824321737 | 9.28749951672546 |
| instrument_only | 7 | 7 | -1.54219120560884 | -0.348331060981957 | 1.19386014462688 | 0.394978586421689 | -14.8772451109455 | 36.7506462624722 | 51.6278913734177 | 15.4036748898376 |
| instrument_only | 8 | 27 | -2.04348893800603 | -1.87148387808073 | 0.1720050599253 | 0.051626934032749 | -14.8772451109455 | 2.0916435650472 | 16.9688886759927 | 5.26178839124252 |
| instrument_only | 9 | 68 | -1.97931926001089 | 0.352033951275513 | 2.3313532112864 | 0.656571422691293 | -14.8772451109455 | 0.489117713994897 | 15.3663628249404 | 3.88831210305509 |
| instrument_only | 10 | 37 | -0.131325259095097 | 1.79470023161785 | 1.92602549071295 | 0.354269274574157 | -14.8772451109455 | 2.294416806071 | 17.1716619170165 | 3.31642856435554 |
| instrument_only | 11 | 4 | 1.4184102742707 | 1.88652404928696 | 0.46811377501626 | 0.223044167486406 | 64.0971304627795 | 67.0383366183326 | 2.94120615555309 | 1.28367574500588 |
| instrument_only | 12 | 13 | 0.287793426568333 | 1.83807180629017 | 1.55027837972184 | 0.554193192453197 | 40.4391433983559 | 81.0978745489383 | 40.6587311505824 | 13.8367092842222 |
| instrument_only | 13 | 8 | 0.534652897756593 | 1.90061960341573 | 1.36596670565914 | 0.609924122099547 | 55.6010597262512 | 76.6473947449969 | 21.0463350187457 | 5.70471838138252 |
| instrument_only | 14 | 9 | 1.75799531552022 | 1.94545146128966 | 0.18745614576944 | 0.0682723160414595 | 13.4273849311149 | 71.3832543476957 | 57.9558694165808 | 18.9553593117256 |
| instrument_only | 15 | 8 | 0.695700887118483 | 1.94349322979478 | 1.2477923426763 | 0.418198484652357 | 9.0571679877065 | 51.3781560756705 | 42.320988087964 | 15.2211538043371 |
| instrument_only | 16 | 4 | 0.401764694367003 | 0.908068195782783 | 0.50630350141578 | 0.223585411471733 | -14.3118295617183 | -10.0079758133012 | 4.3038537484171 | 1.87016207122845 |
| instrument_only | 17 | 7 | 1.1931847222495 | 1.9257558590041 | 0.7325711367546 | 0.290810858300757 | 22.3084389414577 | 46.0894026323308 | 23.7809636908731 | 7.33629548269078 |
| instrument_only | 18 | 22 | -0.00828309190438681 | 1.49376856470772 | 1.50205165661211 | 0.436489701136628 | -14.8453111059522 | -0.518807459284602 | 14.3265036466676 | 3.95040621978854 |
| instrument_only | 19 | 18 | -0.0727188824063472 | 0.254821318744063 | 0.32754020115041 | 0.101459004409985 | -14.5834230592019 | 6.4933033856076 | 21.0767264448095 | 5.52787650045652 |
| instrument_only | 20 | 18 | -0.0244261178589871 | 1.41653806134722 | 1.44096417920621 | 0.449724955673132 | -14.8180341589797 | 4.6769728741854 | 19.4950070331651 | 5.51420001295925 |
| instrument_only | 21 | 29 | -0.0452168136421469 | 1.09899343445338 | 1.14421024809553 | 0.342448598902929 | -14.4009731987496 | -5.13371282021467 | 9.26726037853493 | 2.74435911224089 |
| instrument_only | 22 | 16 | -0.324056047204457 | 1.81913827890782 | 2.14319432611228 | 0.554396242436879 | -14.5776399460332 | -6.47681325629646 | 8.10082668973674 | 2.40834676626717 |
| instrument_only | 23 | 44 | -2.0304881860506 | 1.89011139476201 | 3.92059958081261 | 1.17037230114088 | -14.8772451109455 | 8.0690163972915 | 22.946261508237 | 5.1416190138038 |
| instrument_only | 24 | 25 | -2.04056630140149 | -0.892500698500907 | 1.14806560290058 | 0.254460585338104 | -14.8772451109455 | 4.1399549637952 | 19.0172000747407 | 4.92266595157379 |
| instrument_only | 25 | 2 | -2.03160066219217 | -1.65989079648855 | 0.37170986570362 | 0.262838566672971 | -7.98516005927572 | 18.4244297117491 | 26.4095897710248 | 18.6744000154465 |
| instrument_only | 26 | 1 | -1.37632433588608 | -1.37632433588608 | 0 | NA | -10.5611598272283 | -10.5611598272283 | 0 | NA |
| instrument_only | 27 | 33 | -1.6951307391106 | 0.0658136109057028 | 1.7609443500163 | 0.256358588759682 | -14.8772451109455 | 21.689118784344 | 36.5663638952895 | 8.93882168572837 |
| instrument_only | 28 | 23 | 1.28493137266284 | 1.94849004798783 | 0.66355867532499 | 0.192842579354385 | -11.7678660550553 | 46.4288527228366 | 58.1967187778919 | 11.2780079315081 |
| instrument_only | 29 | 27 | 0.955267114174133 | 1.93072008709499 | 0.975452972920857 | 0.255007244602427 | -14.8772451109455 | 43.2451851294446 | 58.1224302403901 | 12.2728643175785 |
| instrument_only | 30 | 2 | -0.828306803654227 | -0.709924014366337 | 0.11838278928789 | 0.0837092730812452 | 18.4791565117683 | 45.7526842861517 | 27.2735277743834 | 19.2852964361461 |
| instrument_only | 31 | 1 | 1.92350587994718 | 1.92350587994718 | 0 | NA | 12.8158488045085 | 12.8158488045085 | 0 | NA |
| instrument_only | 32 | 14 | 1.84725061127115 | 1.95315362242036 | 0.10590301114921 | 0.0281099017918269 | 2.696772882204 | 47.8734159038199 | 45.1766430216159 | 11.9908795765426 |
| instrument_only | 33 | 29 | 1.84749737853692 | 1.9541575057177 | 0.10666012718078 | 0.0248111146403481 | -9.78840105076591 | 40.6231221007903 | 50.4115231515562 | 11.0367470170818 |
| instrument_only | 34 | 4 | 1.93806869863449 | 1.95282719420095 | 0.0147584955664601 | 0.00681906182806364 | 12.5484620140097 | 58.3116757788149 | 45.7632137648052 | 19.287824065982 |
| instrument_only | 35 | 2 | 1.0970702084783 | 1.32149072402959 | 0.22442051555129 | 0.158689268383698 | -7.26636089806743 | 13.8519172954819 | 21.1182781935493 | 14.9328777176427 |
| region_fe | 1 | 11 | -0.466863291912915 | 2.37627547584608 | 2.843138767759 | 1.28256593381946 | -5.55416262929903 | 59.7990371327789 | 65.3531997620779 | 22.1093140815969 |
| region_fe | 2 | 12 | -0.539821391385985 | 3.35553878354704 | 3.89536017493302 | 1.32072078342093 | -16.9517303522362 | 19.0594443270616 | 36.0111746792978 | 9.35701241783806 |
| region_fe | 3 | 16 | -0.554783446150604 | -0.531422016259005 | 0.0233614298915991 | 0.00767986612439193 | -8.29260063317003 | 16.5918336918836 | 24.8844343250536 | 6.95308526038907 |
| region_fe | 4 | 1 | -0.256438221689045 | -0.256438221689045 | 0 | NA | 25.2630930038368 | 25.2630930038368 | 0 | NA |
| region_fe | 5 | 12 | -0.726504443577987 | 1.51506383136081 | 2.2415682749388 | 0.684324584971317 | -3.06810590678395 | 39.8395945196269 | 42.9077004264108 | 11.4752595237954 |
| Table truncated in rendered note; full CSV has 875 rows. |  |  |  |  |  |  |  |  |  |  |

State-by-state residual ranges

``` r
analysis_table(first_stage_state_deletion[order(abs(first_stage_state_deletion$estimate_change), decreasing = TRUE), ], "Leave-one-state-out influence", max_rows = 30)
```

| specification_id | specification | treatment | instrument | omitted_state | estimate | excluded_instrument_f | estimate_change | f_change |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 23 | 1.82157474378406 | 2.32169572751529 | 0.905764960541485 | 1.30698357548271 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 1 | 0.218498085362771 | 0.154274818190413 | -0.697311697879802 | -0.86043733384217 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 2 | 1.19965044377952 | 1.37398276327484 | 0.283840660536945 | 0.359270611242256 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 9 | 1.19075827609984 | 1.04908486659358 | 0.274948492857264 | 0.0343727145610018 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 29 | 0.771838994378048 | 0.82173595432293 | -0.143970788864525 | -0.192976197709653 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 12 | 0.772237062825076 | 0.800454286634194 | -0.143572720417496 | -0.214257865398389 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 15 | 1.02982600158689 | 1.26222213241388 | 0.114016218344317 | 0.247509980381298 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 33 | 0.804773335618254 | 0.859372939374905 | -0.111036447624318 | -0.155339212657678 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 13 | 0.828838327140327 | 0.827007512398241 | -0.0869714561022452 | -0.187704639634341 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 18 | 0.836390103084145 | 0.860276606187685 | -0.0794196801584273 | -0.154435545844898 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 5 | 0.994462321210279 | 1.07061769704557 | 0.0786525379677064 | 0.0559055450129831 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 32 | 0.9759645997565 | 1.25293910052751 | 0.0601548165139281 | 0.238226948494926 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 21 | 0.973874503896505 | 1.11183996364978 | 0.058064720653933 | 0.0971278116171967 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 28 | 0.864834872981439 | 0.821373581218434 | -0.0509749102611333 | -0.193338570814149 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 7 | 0.874309701463579 | 0.948857014357379 | -0.0415000817789931 | -0.0658551376752036 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 8 | 0.875910112296746 | 0.934363085838179 | -0.0398996709458268 | -0.0803490661944034 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 19 | 0.945544766959976 | 1.11187657260692 | 0.029734983717404 | 0.0971644205743396 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 34 | 0.941729135278568 | 1.00330911142203 | 0.0259193520359956 | -0.0114030406105496 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 14 | 0.893711606349635 | 0.992851407379026 | -0.0220981768929372 | -0.0218607446535564 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 25 | 0.935420347245419 | 1.05925108650367 | 0.0196105640028462 | 0.0445389344710827 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 11 | 0.898761236384943 | 0.979769642666115 | -0.0170485468576292 | -0.0349425093664674 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 35 | 0.898960222956504 | 0.96591470989571 | -0.0168495602860688 | -0.0487974421368728 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 20 | 0.930649154985597 | 1.00068672488355 | 0.0148393717430244 | -0.0140254271490323 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 22 | 0.930242772404941 | 0.969512824464693 | 0.0144329891623682 | -0.0451993275678894 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 10 | 0.905668064114764 | 0.90670239904233 | -0.0101417191278083 | -0.108009752990253 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 24 | 0.924719981200367 | 0.935655621949496 | 0.00891019795779424 | -0.0790565300830863 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 17 | 0.912361989081488 | 1.00760845317102 | -0.00344779416108443 | -0.00710369886155959 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 30 | 0.913243872028096 | 1.00600600702565 | -0.00256591121447647 | -0.00870614500693079 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 16 | 0.917463880751177 | 1.01931292416001 | 0.00165409750860412 | 0.00460077212742904 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | 6 | 0.917078840209582 | 1.00942334335183 | 0.00126905696700919 | -0.00528880868074988 |
| Table truncated in rendered note; full CSV has 35 rows. |  |  |  |  |  |  |  |  |

Leave-one-state-out influence

``` r
analysis_table(first_stage_district_influence[order(first_stage_district_influence$cooks_distance, decreasing = TRUE), ], "Most influential districts in the expanded first stage", max_rows = 30)
```

| state_code_2001 | district_code_2001 | leverage | cooks_distance | studentized_residual | instrument_dfbeta |
|:---|:---|:---|:---|:---|:---|
| 30 | 1 | 0.502239274213472 | 0.150050716753567 | -2.71580243145313 | 0.00256591121447445 |
| 30 | 2 | 0.502239274213472 | 0.150050716753567 | 2.71580243145312 | 0.00256591121447401 |
| 34 | 3 | 0.295256454437258 | 0.122332769553427 | 3.8316172194494 | -0.0263145512910064 |
| 14 | 1 | 0.127748487289063 | 0.113828136978296 | 6.4022946061655 | -0.0318681557361565 |
| 1 | 12 | 0.135593568070074 | 0.0904166128184042 | -5.4585636255821 | 0.471293339579895 |
| 25 | 2 | 0.54096362442525 | 0.0815658155547534 | 1.84580934414687 | -0.0196105640028247 |
| 25 | 1 | 0.540963624425249 | 0.0815658155547528 | -1.84580934414687 | -0.0196105640028246 |
| 34 | 1 | 0.299942171761755 | 0.0736823784442475 | -2.92370583176274 | 0.00479951919144051 |
| 35 | 1 | 0.538212103904754 | 0.0707335077426914 | 1.72773145301843 | 0.0168495602860879 |
| 35 | 2 | 0.538212103904755 | 0.0707335077426913 | -1.72773145301843 | 0.0168495602860879 |
| 1 | 9 | 0.120574705727383 | 0.065935060488407 | 4.9625435870545 | 0.0921114611245535 |
| 1 | 10 | 0.156276923773901 | 0.0611125297111104 | 4.08043180529287 | -0.28107232847427 |
| 7 | 2 | 0.152260307231148 | 0.0594794257132555 | -4.08823056319682 | 0.0421323811946156 |
| 15 | 8 | 0.157081011016881 | 0.0478188556398601 | 3.5858155804205 | -0.00434585259579679 |
| 7 | 4 | 0.149719582830189 | 0.0384069807314699 | 3.29993188026434 | 0.02284604312446 |
| 14 | 2 | 0.150657118565714 | 0.0360548189677536 | -3.18330843737301 | 0.06272132596864 |
| 12 | 6 | 0.0996422352741479 | 0.0323813906617223 | 3.83566769966484 | 0.0903146975376278 |
| 1 | 5 | 0.140473832502644 | 0.0296815396851692 | 3.00592250172441 | 0.164345444452289 |
| 15 | 7 | 0.20889894676604 | 0.026806355381667 | 2.23883033687415 | -0.124589526029209 |
| 1 | 1 | 0.124317367019829 | 0.0265139400932627 | -3.04897384094197 | -0.0820015858577844 |
| 1 | 14 | 0.183934651554748 | 0.02606289978931 | -2.3910359970257 | 0.166170899247327 |
| 1 | 6 | 0.119127838708915 | 0.0233838439325388 | 2.93177562158128 | 0.119326128200949 |
| 32 | 10 | 0.100700738387286 | 0.0226713634435627 | 3.17693599506406 | -0.0112940738104486 |
| 14 | 5 | 0.17451434458917 | 0.0221193988510296 | -2.27323394039237 | -0.0553748523410217 |
| 5 | 5 | 0.126020848418863 | 0.0217075047918984 | 2.73272668447336 | -0.122728181470583 |
| 29 | 20 | 0.0938020730087223 | 0.0192340215709491 | 3.04109426911453 | 0.0218328188199316 |
| 1 | 13 | 0.200899542792867 | 0.0190889232493775 | -1.93389305489157 | 0.125902917047676 |
| 6 | 8 | 0.0596268732919032 | 0.0183483142865731 | 3.8138820974631 | 0.0271046604594203 |
| 28 | 5 | 0.179083844612659 | 0.0174406132219123 | 1.9847910965636 | 0.0399628677196792 |
| 14 | 9 | 0.142983980846962 | 0.0172191697445257 | -2.25759491954326 | 0.0297307074098981 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |

Most influential districts in the expanded first stage

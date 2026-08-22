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

| variable | n_nonmissing | mean | sd | min | max |
|:---|---:|---:|---:|---:|---:|
| dise_emi_enrollment_share_total_0708 | 538 | 12.688 | 23.801 | 0 | 100.000 |
| dise_emi_enrollment_share_total_0508_pooled | 475 | 13.205 | 24.543 | 0 | 99.879 |
| dise_hindi_enrollment_share_total_0708 | 502 | 46.688 | 46.616 | 0 | 100.000 |
| dise_english_share_english_hindi_0708 | 486 | 43.518 | 45.041 | 0 | 100.000 |
| dise_private_enrollment_share_0708 | 552 | 27.405 | 18.617 | 0 | 95.114 |
| dise_private_school_share_0708 | 552 | 19.488 | 14.639 | 0 | 95.481 |

DISE baseline treatment coverage and scale

``` r
analysis_table(dise_nss_validation, "NSS-DISE district EMI measurement agreement")
```

| dise_variable | nss_variable | comparison | n | pearson | spearman | mean_dise | mean_nss | mean_difference | rmse | state_residual_pearson | status |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| dise_emi_enrollment_share_total_0708 | emi_share_enrolled_0708 | enrolled_total_denominator | 520 | 0.896 | 0.753 | 12.627 | 18.894 | -6.267 | 12.841 | 0.607 | estimated |
| dise_emi_enrollment_share_total_0708 | emi_exposure_all_children_0708 | all_child_context | 520 | 0.882 | 0.759 | 12.627 | 14.770 | -2.144 | 11.425 | 0.562 | estimated |

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

| specification_id | sequence | adjustment_id | adjustment | construction_id | construction | fixed_effect | excluded_instruments | included_language_controls | n_excluded_instruments | joint_excluded_f | joint_excluded_p | partial_r_squared | n | n_states | n_regions | construct_id | treatment | analysis_scope |
|:---|---:|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|:---|:---|:---|
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 13.261 | 0.000 | 0.162 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 2.759 | 0.097 | 0.020 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 7.605 | 0.006 | 0.050 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.716 | 0.398 | 0.002 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.951 | 0.163 | 0.005 | 520 | 31 | 6 | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 11.795 | 0.001 | 0.155 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 3.196 | 0.075 | 0.032 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 8.976 | 0.003 | 0.077 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.724 | 0.190 | 0.025 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 3.149 | 0.077 | 0.038 | 458 | 30 | 6 | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 4.135 | 0.043 | 0.098 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.205 | 0.273 | 0.007 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.040 | 0.308 | 0.006 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.447 | 0.230 | 0.009 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.385 | 0.240 | 0.008 | 485 | 30 | 6 | hindi_share_0708 | dise_hindi_enrollment_share_total_0708 | relevance_only |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 7.165 | 0.008 | 0.159 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.275 | 0.600 | 0.002 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.040 | 0.841 | 0.000 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.352 | 0.554 | 0.003 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.255 | 0.614 | 0.002 | 469 | 30 | 6 | english_hindi_share_0708 | dise_english_share_english_hindi_0708 | relevance_only |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 1.516 | 0.219 | 0.027 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.119 | 0.291 | 0.018 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 1.234 | 0.267 | 0.019 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.841 | 0.175 | 0.004 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.853 | 0.174 | 0.004 | 534 | 31 | 6 | private_enrollment_share_0708 | dise_private_enrollment_share_0708 | relevance_only |
| unadjusted\_\_nonzero_mean | 1 | unadjusted | Unadjusted | nonzero_mean | Mean distance among speakers above zero | none | ling_distance_nonzero_mean |  | 1 | 1.573 | 0.210 | 0.030 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only |
| region_main\_\_nonzero_mean | 16 | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.919 | 0.338 | 0.012 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only |
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 0.981 | 0.323 | 0.012 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only |
| state_main\_\_nonzero_mean | 46 | state_main | State FE + main controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.059 | 0.304 | 0.002 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.202 | 0.273 | 0.002 | 534 | 31 | 6 | private_school_share_0708 | dise_private_school_share_0708 | relevance_only |

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

| specification_id | adjustment_id | construction_id | estimate_2sls | std_error_clustered | p_value_clustered | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_n_components | ar_95_disconnected | ar_95_contains_zero | ar_95_grid_accepted_min | ar_95_grid_accepted_max | ar_95_left_truncated | ar_95_right_truncated | ar_95_components | n | status | reason | construct_id | treatment | analysis_scope |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|:---|:---|---:|---:|:---|:---|:---|---:|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.004 | 0.002 | 0.025 | 3.401 | 0.066 | 3.401 | 0.066 | -0.007 | 0.000 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.000 | FALSE | FALSE | \[-0.0071908, 0\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.001 | 0.006 | 0.847 | 0.042 | 0.838 | 0.042 | 0.838 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958774, -0.0268457\] U \[-0.0124641, 0.0958774 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.000 | 0.004 | 0.906 | 0.013 | 0.908 | 0.013 | 0.908 | -0.008 | 0.016 | FALSE | 1 | FALSE | TRUE | -0.008 | 0.016 | FALSE | FALSE | \[-0.00767019, 0.0158198\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.038 | 0.050 | 0.450 | 3.324 | 0.069 | 3.324 | 0.069 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958774, -0.0158198\] U \[-0.00335571, 0.0958774 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.023 | 0.023 | 0.316 | 2.776 | 0.096 | 2.776 | 0.096 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.096 | 0.096 | TRUE | TRUE | \[grid\<= -0.0958774, -0.0536913\] U \[-0.00383509, 0.0958774 \<=grid\] | 520 | estimated | NA | emi_total_0708 | dise_emi_enrollment_share_total_0708 | structural_iv |
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.004 | 0.002 | 0.024 | 3.027 | 0.083 | 3.027 | 0.083 | -0.007 | 0.000 | FALSE | 1 | FALSE | TRUE | -0.007 | 0.000 | FALSE | FALSE | \[-0.00700096, 0.000466731\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | 0.002 | 0.005 | 0.767 | 0.106 | 0.744 | 0.106 | 0.744 | NA | NA | FALSE | 2 | TRUE | TRUE | -0.093 | 0.093 | TRUE | TRUE | \[grid\<= -0.0933462, -0.0536741\] U \[-0.00746769, 0.0933462 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| region_expanded\_\_nonzero_mean | region_expanded | nonzero_mean | 0.000 | 0.003 | 0.991 | 0.000 | 0.991 | 0.000 | 0.991 | -0.006 | 0.013 | FALSE | 1 | FALSE | TRUE | -0.006 | 0.013 | FALSE | FALSE | \[-0.00560077, 0.0126017\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | 0.015 | 0.014 | 0.286 | 4.397 | 0.037 | 4.397 | 0.037 | NA | NA | FALSE | 2 | TRUE | FALSE | -0.093 | 0.093 | TRUE | TRUE | \[grid\<= -0.0933462, -0.0280039\] U \[0.000933462, 0.0933462 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | 0.011 | 0.009 | 0.224 | 3.459 | 0.064 | 3.459 | 0.064 | NA | NA | FALSE | 1 | FALSE | TRUE | 0.000 | 0.093 | FALSE | TRUE | \[-0.000466731, 0.0933462 \<=grid\] | 458 | estimated | NA | emi_total_0508_pooled | dise_emi_enrollment_share_total_0508_pooled | structural_iv |

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
| 2005-06       |    547 |   503 |                       0 |
| 2006-07       |    550 |   528 |                       0 |
| 2007-08       |    552 |   538 |                       0 |
| 2008-09       |    539 |   460 |                       0 |
| 2009-10       |    539 |   457 |                       0 |
| 2010-11       |    532 |   448 |                       0 |
| 2011-12       |    539 |   477 |                       0 |
| 2012-13       |    553 |   486 |                       0 |
| 2013-14       |    553 |   506 |                       0 |
| 2014-15       |    543 |   530 |                       0 |
| 2015-16       |    543 |   543 |                       0 |

Longitudinal DISE EMI coverage and count-validity checks

``` r
analysis_table(
  dise_dynamic_summary[dise_dynamic_summary$construction_id == "nonzero_mean", ],
  "Preferred linguistic-distance trajectory: joint clustered tests",
  max_rows = 10
)
```

| instrument | dynamic_fe | reference_year | n | n_districts | n_years | joint_distance_year_f | joint_distance_year_p | cluster_status | construction_id | equivalent_construction_ids |
|:---|:---|:---|---:|---:|---:|---:|---:|:---|:---|:---|
| ling_distance_nonzero_mean | district_year | 2007-08 | 5294 | 553 | 11 | 2.357 | 0.009 | estimated | nonzero_mean | nonzero_mean;nonzero_mean_hindi_urdu;nonzero_mean_hindi_urdu_separate;nonzero_mean_shastry |
| ling_distance_nonzero_mean | district_state_year | 2007-08 | 5294 | 553 | 11 | NA | NA | estimated | nonzero_mean | nonzero_mean;nonzero_mean_hindi_urdu;nonzero_mean_hindi_urdu_separate;nonzero_mean_shastry |

Preferred linguistic-distance trajectory: joint clustered tests

``` r
analysis_table(
  dise_dynamic_event[dise_dynamic_event$construction_id == "nonzero_mean", ],
  "Preferred linguistic-distance trajectory: year coefficients relative to 2007-08",
  max_rows = 30
)
```

| academic_year | reference_year | estimate | std.error | statistic | p.value | construction_id | instrument | dynamic_fe |
|:---|:---|---:|---:|---:|---:|:---|:---|:---|
| 2005-06 | 2007-08 | -0.620 | 0.307 | -2.021 | 0.043 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2006-07 | 2007-08 | -0.643 | 0.339 | -1.897 | 0.058 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2008-09 | 2007-08 | 0.337 | 0.318 | 1.059 | 0.290 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2009-10 | 2007-08 | 0.055 | 0.465 | 0.118 | 0.906 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2010-11 | 2007-08 | 4.625 | 2.745 | 1.685 | 0.092 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2011-12 | 2007-08 | 0.444 | 0.475 | 0.935 | 0.350 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2012-13 | 2007-08 | 0.596 | 0.474 | 1.259 | 0.208 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2013-14 | 2007-08 | 0.423 | 0.537 | 0.787 | 0.431 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2014-15 | 2007-08 | 0.857 | 0.546 | 1.568 | 0.117 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2015-16 | 2007-08 | 0.992 | 0.572 | 1.734 | 0.083 | nonzero_mean | ling_distance_nonzero_mean | district_year |
| 2005-06 | 2007-08 | 2.390 | 1.483 | 1.612 | 0.107 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2006-07 | 2007-08 | 2.049 | 1.164 | 1.760 | 0.078 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2008-09 | 2007-08 | 0.671 | 1.156 | 0.581 | 0.561 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2009-10 | 2007-08 | 0.288 | 1.031 | 0.280 | 0.780 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2010-11 | 2007-08 | -10.331 | 10.806 | -0.956 | 0.339 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2011-12 | 2007-08 | 0.297 | 1.119 | 0.266 | 0.791 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2012-13 | 2007-08 | -0.489 | 1.017 | -0.481 | 0.631 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2013-14 | 2007-08 | -0.382 | 1.054 | -0.363 | 0.717 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2014-15 | 2007-08 | -0.285 | 1.094 | -0.260 | 0.795 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |
| 2015-16 | 2007-08 | -0.580 | 1.096 | -0.529 | 0.596 | nonzero_mean | ling_distance_nonzero_mean | district_state_year |

Preferred linguistic-distance trajectory: year coefficients relative to
2007-08

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
| Andaman & Nicobar Islands | 2 | 24.4068340671691 | 4.25457336795535 | 114699.0625 | 1716.59345353575 | 39.9965239648858 |
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
| instrument_only | Instrument only | 1 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none |  | 0 | 5.635 | 1.642 | 3.432 | 0.001 | 11.779 | 0.142 | 1.362 | 20.353 | 0.377 | 1.000 | 573 | 35 | 6 | estimated | NA |
| region_fe | Six-region fixed effects | 2 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region |  | 0 | 7.260 | 3.493 | 2.078 | 0.038 | 4.320 | 0.099 | 0.689 | 15.938 | 0.314 | 0.256 | 573 | 35 | 6 | estimated | NA |
| state_fe | State fixed effects | 3 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state |  | 0 | 0.328 | 0.958 | 0.342 | 0.732 | 0.117 | 0.000 | 0.538 | 8.448 | 0.021 | 0.156 | 573 | 35 | 6 | estimated | NA |
| census_controls | Main Census controls | 4 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 4.688 | 1.240 | 3.781 | 0.000 | 14.298 | 0.130 | 1.091 | 14.193 | 0.360 | 0.642 | 573 | 35 | 6 | estimated | NA |
| region_fe_census_controls | Six-region fixed effects + main Census controls | 5 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 4.070 | 2.248 | 1.811 | 0.071 | 3.279 | 0.036 | 0.587 | 12.553 | 0.190 | 0.186 | 573 | 35 | 6 | estimated | NA |
| state_fe_census_controls | State fixed effects + main Census controls | 6 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 10 | 0.847 | 0.962 | 0.881 | 0.379 | 0.775 | 0.004 | 0.492 | 6.627 | 0.063 | 0.130 | 573 | 35 | 6 | estimated | NA |
| expanded_controls | Expanded Census controls | 7 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | none | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.453 | 1.193 | 4.570 | 0.000 | 20.887 | 0.175 | 1.056 | 13.753 | 0.419 | 0.601 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_controls | Six-region fixed effects + expanded Census controls | 8 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.091 | 2.127 | 2.393 | 0.017 | 5.729 | 0.058 | 0.574 | 12.149 | 0.240 | 0.177 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.928 | 0.906 | 1.024 | 0.306 | 1.049 | 0.005 | 0.484 | 6.583 | 0.068 | 0.126 | 573 | 35 | 6 | estimated | NA |
| region_fe_main_without_human_capital | Six-region FE + main controls without human capital | 10 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 5.396 | 2.130 | 2.533 | 0.012 | 6.417 | 0.061 | 0.604 | 13.186 | 0.247 | 0.197 | 573 | 35 | 6 | estimated | NA |
| state_fe_main_without_human_capital | State FE + main controls without human capital | 11 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 9 | 1.253 | 0.982 | 1.275 | 0.203 | 1.626 | 0.008 | 0.496 | 6.813 | 0.091 | 0.133 | 573 | 35 | 6 | estimated | NA |
| region_fe_expanded_without_human_capital | Six-region FE + expanded controls without human capital | 12 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 6.018 | 2.101 | 2.864 | 0.004 | 8.202 | 0.078 | 0.599 | 12.893 | 0.280 | 0.193 | 573 | 35 | 6 | estimated | NA |
| state_fe_expanded_without_human_capital | State FE + expanded controls without human capital | 13 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;demography;economic_structure;basic_development | 11 | 1.386 | 1.015 | 1.366 | 0.173 | 1.866 | 0.010 | 0.490 | 6.804 | 0.100 | 0.129 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_scale_geography | Six-region FE + through basic scale geography | 14 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography | 3 | 5.751 | 2.821 | 2.039 | 0.042 | 4.156 | 0.071 | 0.651 | 14.080 | 0.266 | 0.228 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_social_composition | Six-region FE + through social composition | 15 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition | 6 | 4.909 | 2.347 | 2.091 | 0.037 | 4.374 | 0.049 | 0.621 | 13.784 | 0.221 | 0.208 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_human_capital | Six-region FE + through human capital | 16 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital | 8 | 3.212 | 2.289 | 1.404 | 0.161 | 1.970 | 0.023 | 0.605 | 12.689 | 0.153 | 0.197 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_demography | Six-region FE + through demography | 17 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography | 9 | 3.233 | 2.281 | 1.417 | 0.157 | 2.009 | 0.024 | 0.603 | 12.689 | 0.154 | 0.196 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_economic_structure | Six-region FE + through economic structure | 18 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 4.057 | 2.507 | 1.618 | 0.106 | 2.619 | 0.035 | 0.581 | 12.601 | 0.187 | 0.182 | 573 | 35 | 6 | estimated | NA |
| region_fe_plus_basic_development | Six-region FE + through basic development | 19 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | region | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 5.091 | 2.127 | 2.393 | 0.017 | 5.729 | 0.058 | 0.574 | 12.149 | 0.240 | 0.177 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_scale_geography | State FE + through basic scale geography | 20 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography | 3 | 1.631 | 0.820 | 1.990 | 0.047 | 3.961 | 0.015 | 0.519 | 6.995 | 0.121 | 0.146 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_social_composition | State FE + through social composition | 21 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition | 6 | 1.531 | 0.956 | 1.602 | 0.110 | 2.565 | 0.012 | 0.503 | 6.968 | 0.111 | 0.137 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_human_capital | State FE + through human capital | 22 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital | 8 | 0.894 | 0.866 | 1.032 | 0.303 | 1.065 | 0.005 | 0.497 | 6.615 | 0.067 | 0.133 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_demography | State FE + through demography | 23 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography | 9 | 0.853 | 0.859 | 0.993 | 0.321 | 0.987 | 0.004 | 0.496 | 6.605 | 0.064 | 0.133 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_economic_structure | State FE + through economic structure | 24 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure | 12 | 0.998 | 0.918 | 1.087 | 0.278 | 1.181 | 0.005 | 0.489 | 6.590 | 0.074 | 0.129 | 573 | 35 | 6 | estimated | NA |
| state_fe_plus_basic_development | State FE + through basic development | 25 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.928 | 0.906 | 1.024 | 0.306 | 1.049 | 0.005 | 0.484 | 6.583 | 0.068 | 0.126 | 573 | 35 | 6 | estimated | NA |

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
| region_expanded\_\_nonzero_mean | 31 | region_expanded | Six-region FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | region | ling_distance_nonzero_mean |  | 1 | 5.729 | 0.017 | 0.058 | 573 | 35 | 6 |
| region_expanded\_\_distant_share | 32 | region_expanded | Six-region FE + expanded controls | distant_share | Share speaking languages at distance three or higher | region | ling_share_distance_ge3 |  | 1 | 0.210 | 0.647 | 0.002 | 573 | 35 | 6 |
| region_expanded\_\_top3_legacy | 33 | region_expanded | Six-region FE + expanded controls | top3_legacy | Legacy top-three weighted mean | region | ling_distance_top3_legacy |  | 1 | 6.065 | 0.014 | 0.054 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu | 34 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | region | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 5.859 | 0.016 | 0.058 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_shastry | 35 | region_expanded | Six-region FE + expanded controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | region | ling_distance_nonzero_mean | hindi_urdu_share;native_english_share | 1 | 5.717 | 0.017 | 0.057 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_sensitivity_low | 36 | region_expanded | Six-region FE + expanded controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | region | ling_distance_nonzero_mean_sensitivity_low | hindi_urdu_share;native_english_share | 1 | 5.834 | 0.016 | 0.059 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_sensitivity_high | 37 | region_expanded | Six-region FE + expanded controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | region | ling_distance_nonzero_mean_sensitivity_high | hindi_urdu_share;native_english_share | 1 | 0.491 | 0.484 | 0.003 | 573 | 35 | 6 |
| region_expanded\_\_nonzero_mean_hindi_urdu_separate | 38 | region_expanded | Six-region FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | region | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 5.064 | 0.025 | 0.053 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all | 39 | region_expanded | Six-region FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 20.613 | 0.000 | 0.200 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_all_unmapped | 40 | region_expanded | Six-region FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | region | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 15.910 | 0.000 | 0.196 | 573 | 35 | 6 |
| region_expanded\_\_distance_shares_mapped | 41 | region_expanded | Six-region FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | region | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 14.735 | 0.000 | 0.193 | 573 | 35 | 6 |
| region_expanded\_\_glottolog_mean | 42 | region_expanded | Six-region FE + expanded controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | region | ling_distance_glottolog_nonhindi_mean |  | 1 | 0.002 | 0.963 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_glottolog_mean_shastry | 43 | region_expanded | Six-region FE + expanded controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | region | ling_distance_glottolog_nonhindi_mean | hindi_urdu_share;native_english_share | 1 | 0.013 | 0.910 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_dyen_noncognate | 44 | region_expanded | Six-region FE + expanded controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | region | ling_distance_dyen_noncognate_pct |  | 1 | 0.066 | 0.798 | 0.000 | 573 | 35 | 6 |
| region_expanded\_\_dyen_noncognate_shastry | 45 | region_expanded | Six-region FE + expanded controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | region | ling_distance_dyen_noncognate_pct | hindi_urdu_share;native_english_share | 1 | 0.033 | 0.856 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.049 | 0.306 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_distant_share | 62 | state_expanded | State FE + expanded controls | distant_share | Share speaking languages at distance three or higher | state | ling_share_distance_ge3 |  | 1 | 0.905 | 0.342 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_top3_legacy | 63 | state_expanded | State FE + expanded controls | top3_legacy | Legacy top-three weighted mean | state | ling_distance_top3_legacy |  | 1 | 2.119 | 0.146 | 0.012 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu | 64 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | state | ling_distance_nonzero_mean | hindi_urdu_share | 1 | 1.167 | 0.281 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_shastry | 65 | state_expanded | State FE + expanded controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | state | ling_distance_nonzero_mean | hindi_urdu_share;native_english_share | 1 | 1.124 | 0.290 | 0.005 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_sensitivity_low | 66 | state_expanded | State FE + expanded controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | state | ling_distance_nonzero_mean_sensitivity_low | hindi_urdu_share;native_english_share | 1 | 1.099 | 0.295 | 0.004 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_sensitivity_high | 67 | state_expanded | State FE + expanded controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | state | ling_distance_nonzero_mean_sensitivity_high | hindi_urdu_share;native_english_share | 1 | 1.209 | 0.272 | 0.001 | 573 | 35 | 6 |
| state_expanded\_\_nonzero_mean_hindi_urdu_separate | 68 | state_expanded | State FE + expanded controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | state | ling_distance_nonzero_mean | hindi_share;urdu_share | 1 | 1.031 | 0.310 | 0.004 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all | 69 | state_expanded | State FE + expanded controls | distance_shares_all | Five distance shares; all-speaker denominator | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 |  | 5 | 3.191 | 0.008 | 0.034 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 3.669 | 0.003 | 0.033 | 573 | 35 | 6 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 0.939 | 0.455 | 0.023 | 573 | 35 | 6 |
| state_expanded\_\_glottolog_mean | 72 | state_expanded | State FE + expanded controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | state | ling_distance_glottolog_nonhindi_mean |  | 1 | 0.290 | 0.590 | 0.001 | 573 | 35 | 6 |
| state_expanded\_\_glottolog_mean_shastry | 73 | state_expanded | State FE + expanded controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | state | ling_distance_glottolog_nonhindi_mean | hindi_urdu_share;native_english_share | 1 | 0.122 | 0.728 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_dyen_noncognate | 74 | state_expanded | State FE + expanded controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | state | ling_distance_dyen_noncognate_pct |  | 1 | 0.146 | 0.702 | 0.000 | 573 | 35 | 6 |
| state_expanded\_\_dyen_noncognate_shastry | 75 | state_expanded | State FE + expanded controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | state | ling_distance_dyen_noncognate_pct | hindi_urdu_share;native_english_share | 1 | 0.135 | 0.713 | 0.000 | 573 | 35 | 6 |

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
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 1.049 | 0.306 | 0.005 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.758 | 0.384 | 0.002 | 486 | 32 | 6 | 90 | ling_mapped_speaker_share | 486 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.849 | 0.357 | 0.004 | 461 | 32 | 6 | 95 | ling_mapped_speaker_share | 461 |
| state_expanded\_\_nonzero_mean | 61 | state_expanded | State FE + expanded controls | nonzero_mean | Mean distance among speakers above zero | state | ling_distance_nonzero_mean |  | 1 | 0.118 | 0.732 | 0.000 | 378 | 25 | 6 | 99 | ling_mapped_speaker_share | 378 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 3.669 | 0.003 | 0.033 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 2.820 | 0.016 | 0.027 | 486 | 32 | 6 | 90 | ling_mapped_speaker_share | 486 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 5.235 | 0.000 | 0.050 | 461 | 32 | 6 | 95 | ling_mapped_speaker_share | 461 |
| state_expanded\_\_distance_shares_all_unmapped | 70 | state_expanded | State FE + expanded controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | state | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_unmapped_speaker_share;native_english_share | 5 | 2.158 | 0.058 | 0.023 | 378 | 25 | 6 | 99 | ling_mapped_speaker_share | 378 |
| state_expanded\_\_distance_shares_mapped | 71 | state_expanded | State FE + expanded controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | state | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 |  | 5 | 0.939 | 0.455 | 0.023 | 573 | 35 | 6 | 0 | ling_mapped_speaker_share | 573 |
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
| Kachchhi | 4.006 | 0.001 | 0.032 | 573 |
| Kashmiri | 0.420 | 0.835 | 0.010 | 573 |
| Kishtwari | 3.639 | 0.003 | 0.030 | 573 |
| Others | 3.626 | 0.003 | 0.029 | 573 |
| Sindhi | 3.792 | 0.002 | 0.033 | 573 |
| Siraji | 4.258 | 0.001 | 0.030 | 573 |

Distance-four leave-one-language-out joint tests

``` r
analysis_table(weak_iv_outcomes, "Weak-IV-aware exploratory outcome estimates", max_rows = 10)
```

| specification_id | adjustment_id | construction_id | estimate_2sls | std_error_clustered | p_value_clustered | reduced_form_joint_f | reduced_form_joint_p | anderson_rubin_f_beta0 | anderson_rubin_p_beta0 | ar_95_lower | ar_95_upper | ar_95_empty | ar_95_n_components | ar_95_disconnected | ar_95_contains_zero | ar_95_grid_accepted_min | ar_95_grid_accepted_max | ar_95_left_truncated | ar_95_right_truncated | ar_95_components | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | -0.00486770726105556 | 0.00212786419938756 | 0.0225254636376184 | 4.24149416436273 | 0.0399002322438112 | 4.24149416436273 | 0.0399002322438112 | -0.0104726453877446 | -0.000551191862512873 | FALSE | 1 | FALSE | FALSE | -0.0104726453877446 | -0.000551191862512873 | FALSE | FALSE | \[-0.0104726, -0.000551192\] | 573 | estimated | NA |
| unadjusted\_\_distant_share | unadjusted | distant_share | -0.00581966472855426 | 0.00414347447219364 | 0.160702615779096 | 2.82304430326738 | 0.0934669682403346 | 2.82304430326738 | 0.0934669682403345 | NA | NA | FALSE | 1 | FALSE | TRUE | -0.110238372502574 | 0.00110238372502575 | TRUE | FALSE | \[grid\<= -0.110238, 0.00110238\] | 573 | estimated | NA |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | -0.00454992211694927 | 0.0018089831168735 | 0.0121705506989458 | 4.44636236117788 | 0.0354106004748819 | 4.44636236117788 | 0.0354106004748819 | -0.00826787793769307 | -0.000551191862512873 | FALSE | 1 | FALSE | FALSE | -0.00826787793769307 | -0.000551191862512873 | FALSE | FALSE | \[-0.00826788, -0.000551192\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | -0.00441152253225072 | 0.00268135726290363 | 0.100468063776657 | 2.36448432039669 | 0.124679825198526 | 2.36448432039669 | 0.124679825198525 | -0.0115750291127703 | 0.00165357558753862 | FALSE | 1 | FALSE | TRUE | -0.0115750291127703 | 0.00165357558753862 | FALSE | FALSE | \[-0.011575, 0.00165358\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_shastry | unadjusted | nonzero_mean_shastry | -0.00437741634876397 | 0.00263650874526516 | 0.0974034580219537 | 2.4496782270048 | 0.118104915857266 | 2.44967822700479 | 0.118104915857266 | -0.0115750291127703 | 0.00110238372502575 | FALSE | 1 | FALSE | TRUE | -0.0115750291127703 | 0.00110238372502575 | FALSE | FALSE | \[-0.011575, 0.00110238\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_sensitivity_low | unadjusted | nonzero_mean_sensitivity_low | -0.00433625242534929 | 0.00264725791659205 | 0.101971064194068 | 2.37485334182605 | 0.123859099964951 | 2.37485334182605 | 0.12385909996495 | -0.0115750291127703 | 0.00110238372502575 | FALSE | 1 | FALSE | TRUE | -0.0115750291127703 | 0.00110238372502575 | FALSE | FALSE | \[-0.011575, 0.00110238\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_sensitivity_high | unadjusted | nonzero_mean_sensitivity_high | -0.0037167968880124 | 0.00310408350849609 | 0.231653373418154 | 1.30529382013151 | 0.253729016834483 | 1.30529382013151 | 0.253729016834484 | -0.0121262209752832 | 0.00330715117507724 | FALSE | 1 | FALSE | TRUE | -0.0121262209752832 | 0.00330715117507724 | FALSE | FALSE | \[-0.0121262, 0.00330715\] | 573 | estimated | NA |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | -0.00455995546137914 | 0.00254078415357537 | 0.0732315291368704 | 2.73937569324302 | 0.0984544257181452 | 2.73937569324302 | 0.0984544257181453 | -0.0110238372502574 | 0.00110238372502575 | FALSE | 1 | FALSE | TRUE | -0.0110238372502574 | 0.00110238372502575 | FALSE | FALSE | \[-0.0110238, 0.00110238\] | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all | unadjusted | distance_shares_all | -0.00101785666306581 | 0.00131045142468129 | 0.437644343731445 | 16.3136517864742 | 4.72697094720407e-15 | 16.3136517864742 | 4.72697094720391e-15 | NA | NA | TRUE | 0 | FALSE | FALSE | NA | NA | FALSE | FALSE | NA | 573 | estimated | NA |
| unadjusted\_\_distance_shares_all_unmapped | unadjusted | distance_shares_all_unmapped | -0.00154785452819314 | 0.00118985776899343 | 0.193828294423888 | 22.3705737936252 | 1.71239807208146e-20 | 22.3705737936253 | 1.71239807208106e-20 | NA | NA | TRUE | 0 | FALSE | FALSE | NA | NA | FALSE | FALSE | NA | 573 | estimated | NA |
| Table truncated in rendered note; full CSV has 93 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

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
| unadjusted\_\_nonzero_mean_shastry | first_stage_joint_f | relevance | TRUE | TRUE | TRUE | NA |
| unadjusted\_\_nonzero_mean_shastry | partial_r_squared | relevance | TRUE | TRUE | TRUE | NA |
| Table truncated in rendered note; full CSV has 651 rows. |  |  |  |  |  |  |

IV diagnostic applicability and implementation status

``` r
analysis_table(iv_joint_balance, "Joint holdout-covariate balance tests", max_rows = 30)
```

| specification_id | adjustment_id | construction_id | fixed_effect | instrument | tested_covariates | n_tested_covariates | joint_f | joint_p | n | status | reason |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| unadjusted\_\_nonzero_mean | unadjusted | nonzero_mean | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 7.99307457370494 | 8.90174408009716e-15 | 573 | estimated |  |
| unadjusted\_\_distant_share | unadjusted | distant_share | none | ling_share_distance_ge3 | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 50.5411642024887 | 1.65742599834248e-85 | 573 | estimated |  |
| unadjusted\_\_top3_legacy | unadjusted | top3_legacy | none | ling_distance_top3_legacy | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 16.1760554308659 | 1.54201329453102e-31 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu | unadjusted | nonzero_mean_hindi_urdu | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.79296094809036 | 4.56458363405227e-10 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_shastry | unadjusted | nonzero_mean_shastry | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.48007529472653 | 1.5518690180035e-11 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_sensitivity_low | unadjusted | nonzero_mean_sensitivity_low | none | ling_distance_nonzero_mean_sensitivity_low | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.36114512118508 | 2.78930955335088e-11 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_sensitivity_high | unadjusted | nonzero_mean_sensitivity_high | none | ling_distance_nonzero_mean_sensitivity_high | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 7.13883140926004 | 6.02804613164417e-13 | 573 | estimated |  |
| unadjusted\_\_nonzero_mean_hindi_urdu_separate | unadjusted | nonzero_mean_hindi_urdu_separate | none | ling_distance_nonzero_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 5.79715437222931 | 4.4839106922886e-10 | 573 | estimated |  |
| unadjusted\_\_glottolog_mean | unadjusted | glottolog_mean | none | ling_distance_glottolog_nonhindi_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.9668761503136 | 1.39559463586044e-12 | 573 | estimated |  |
| unadjusted\_\_glottolog_mean_shastry | unadjusted | glottolog_mean_shastry | none | ling_distance_glottolog_nonhindi_mean | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 6.08232630991346 | 1.10194324361083e-10 | 573 | estimated |  |
| unadjusted\_\_dyen_noncognate | unadjusted | dyen_noncognate | none | ling_distance_dyen_noncognate_pct | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 17.3261028318373 | 9.29403984472188e-34 | 573 | estimated |  |
| unadjusted\_\_dyen_noncognate_shastry | unadjusted | dyen_noncognate_shastry | none | ling_distance_dyen_noncognate_pct | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001;literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 13 | 11.5781986268508 | 2.62746348234377e-22 | 573 | estimated |  |
| region_main\_\_nonzero_mean | region_main | nonzero_mean | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 13.256332255539 | 2.54250295822459e-10 | 573 | estimated |  |
| region_main\_\_distant_share | region_main | distant_share | region | ling_share_distance_ge3 | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.41287262333319 | 0.0480174902151429 | 573 | estimated |  |
| region_main\_\_top3_legacy | region_main | top3_legacy | region | ling_distance_top3_legacy | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 0.643356743376577 | 0.631773913601768 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 12.2917567781619 | 1.39389900296398e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_shastry | region_main | nonzero_mean_shastry | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 11.608678739785 | 4.66777041616539e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | nonzero_mean_sensitivity_low | region | ling_distance_nonzero_mean_sensitivity_low | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 11.6965008297178 | 3.99624285168773e-09 | 573 | estimated |  |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | nonzero_mean_sensitivity_high | region | ling_distance_nonzero_mean_sensitivity_high | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 5.32052853836261 | 0.000328479471183337 | 573 | estimated |  |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | region | ling_distance_nonzero_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 14.6490790761642 | 2.21897221832896e-11 | 573 | estimated |  |
| region_main\_\_glottolog_mean | region_main | glottolog_mean | region | ling_distance_glottolog_nonhindi_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.26374093934227 | 0.0611510902689427 | 573 | estimated |  |
| region_main\_\_glottolog_mean_shastry | region_main | glottolog_mean_shastry | region | ling_distance_glottolog_nonhindi_mean | literacy_share_2001;worker_share_2001;cultivator_share_workers_2001;agricultural_labourer_share_workers_2001 | 4 | 2.41873233711915 | 0.047565812153335 | 573 | estimated |  |
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
| region_main\_\_nonzero_mean | region_main | Six-region FE + main controls | nonzero_mean | Mean distance among speakers above zero | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 16 |
| region_main\_\_distant_share | region_main | Six-region FE + main controls | distant_share | Share speaking languages at distance three or higher | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_ge3 | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 17 |
| region_main\_\_top3_legacy | region_main | Six-region FE + main controls | top3_legacy | Legacy top-three weighted mean | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_top3_legacy | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 18 |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu | Nonzero mean with combined Hindi-Urdu share | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 19 |
| region_main\_\_nonzero_mean_shastry | region_main | Six-region FE + main controls | nonzero_mean_shastry | Nonzero mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 20 |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | Six-region FE + main controls | nonzero_mean_sensitivity_low | Shastry nonzero mean under joint lower-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_low | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 21 |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | Six-region FE + main controls | nonzero_mean_sensitivity_high | Shastry nonzero mean under joint upper-degree adjudication sensitivity | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_nonzero_mean_sensitivity_high | ling_sensitivity_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 22 |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | Six-region FE + main controls | nonzero_mean_hindi_urdu_separate | Nonzero mean with separate Hindi and Urdu shares | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_share;urdu_share | ling_distance_nonzero_mean | ling_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 23 |
| region_main\_\_distance_shares_all | region_main | Six-region FE + main controls | distance_shares_all | Five distance shares; all-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 24 |
| region_main\_\_distance_shares_all_unmapped | region_main | Six-region FE + main controls | distance_shares_all_unmapped | Five distance shares with unresolved and English shares controlled | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | ling_unmapped_speaker_share;native_english_share | ling_share_distance_1;ling_share_distance_2;ling_share_distance_3;ling_share_distance_4;ling_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 25 |
| region_main\_\_distance_shares_mapped | region_main | Six-region FE + main controls | distance_shares_mapped | Five distance shares; mapped-speaker denominator | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_mapped_share_distance_1;ling_mapped_share_distance_2;ling_mapped_share_distance_3;ling_mapped_share_distance_4;ling_mapped_share_distance_5 | ling_mapped_speaker_share | 1 | 5 | primary | alternative_distance_common_support | state_code_2001 | B | 26 |
| region_main\_\_glottolog_mean | region_main | Six-region FE + main controls | glottolog_mean | Glottolog edge-distance mean among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 27 |
| region_main\_\_glottolog_mean_shastry | region_main | Six-region FE + main controls | glottolog_mean_shastry | Glottolog edge-distance mean with Shastry composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_glottolog_nonhindi_mean | ling_glottolog_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 28 |
| region_main\_\_dyen_noncognate | region_main | Six-region FE + main controls | dyen_noncognate | Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 |  | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 29 |
| region_main\_\_dyen_noncognate_shastry | region_main | Six-region FE + main controls | dyen_noncognate_shastry | Dyen/Shastry noncognate percentage with composition controls | real_log_consumption_change | emi_exposure_all_children_0708 | region | log_population_2001;urban_share_2001;adult_secondary_plus_share_2001;sc_share_2001;st_share_2001;muslim_share_2001;agricultural_worker_share_2001;dependency_ratio_2001;electricity_access_share_2001;log_population_density_2001 | hindi_urdu_share;native_english_share | ling_distance_dyen_noncognate_pct | ling_dyen_mapped_speaker_share | 1 | 1 | primary | alternative_distance_common_support | state_code_2001 | B | 30 |
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
| unadjusted\_\_distance_shares_all | 1 | 5 | sargan | estimated | 15.494 | 4 | 0.004 | NA |
| unadjusted\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 19.395 | 4 | 0.001 | NA |
| unadjusted\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 20.718 | 4 | 0.000 | NA |
| region_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 10.595 | 4 | 0.032 | NA |
| region_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 18.160 | 4 | 0.001 | NA |
| region_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 10.085 | 4 | 0.039 | NA |
| region_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 11.017 | 4 | 0.026 | NA |
| region_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 18.293 | 4 | 0.001 | NA |
| region_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 10.882 | 4 | 0.028 | NA |
| state_main\_\_distance_shares_all | 1 | 5 | sargan | estimated | 10.589 | 4 | 0.032 | NA |
| state_main\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 8.921 | 4 | 0.063 | NA |
| state_main\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 8.412 | 4 | 0.078 | NA |
| state_expanded\_\_distance_shares_all | 1 | 5 | sargan | estimated | 9.764 | 4 | 0.045 | NA |
| state_expanded\_\_distance_shares_all_unmapped | 1 | 5 | sargan | estimated | 8.322 | 4 | 0.080 | NA |
| state_expanded\_\_distance_shares_mapped | 1 | 5 | sargan | estimated | 7.475 | 4 | 0.113 | NA |

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
| region_main\_\_nonzero_mean | region_main | nonzero_mean | ling_distance_nonzero_mean | region | 4.07038406394942 | 0.207409306311107 | 0.0626530414156398 | 10 | 0.777777777777778 | 2 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_distant_share | region_main | distant_share | ling_share_distance_ge3 | region | -0.0533459056841136 | -0.0634988176170656 | 0.0132199712468269 | 10 | 0.333333333333333 | 6 | 26 | 0.461538461538462 | 573 | estimated | NA |
| region_main\_\_top3_legacy | region_main | top3_legacy | ling_distance_top3_legacy | region | 3.87944999219917 | 0.284420980628362 | 0.101624018823417 | 10 | 0.666666666666667 | 3 | 26 | 0.346153846153846 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu | region_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | region | 4.0785251040729 | 0.208390893999373 | 0.0623218066599762 | 10 | 0.777777777777778 | 2 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_shastry | region_main | nonzero_mean_shastry | ling_distance_nonzero_mean | region | 4.03728543511109 | 0.206282352559967 | 0.0624045981333906 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_sensitivity_low | region_main | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | region | 4.0825347316473 | 0.210066972996356 | 0.0624910885986472 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_sensitivity_high | region_main | nonzero_mean_sensitivity_high | ling_distance_nonzero_mean_sensitivity_high | region | 0.53959052973382 | 0.109225278616161 | 0.0135160680822839 | 10 | 0.555555555555556 | 4 | 26 | 0.269230769230769 | 573 | estimated | NA |
| region_main\_\_nonzero_mean_hindi_urdu_separate | region_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | region | 3.9799750643284 | 0.199300890655762 | 0.0686304066848693 | 10 | 0.777777777777778 | 2 | 26 | 0.230769230769231 | 573 | estimated | NA |
| region_main\_\_glottolog_mean | region_main | glottolog_mean | ling_distance_glottolog_nonhindi_mean | region | -0.198636080357125 | 0.000426594220490275 | 0.00762977859041081 | 10 | 0.444444444444444 | 5 | 26 | 0.5 | 573 | estimated | NA |
| region_main\_\_glottolog_mean_shastry | region_main | glottolog_mean_shastry | ling_distance_glottolog_nonhindi_mean | region | -0.181445387314719 | 0.00591778289390344 | 0.00789427597848769 | 10 | 0.444444444444444 | 5 | 26 | 0.461538461538462 | 573 | estimated | NA |
| region_main\_\_dyen_noncognate | region_main | dyen_noncognate | ling_distance_dyen_noncognate_pct | region | -0.0234088208073109 | 0.0231140969871656 | 0.00924814986009304 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| region_main\_\_dyen_noncognate_shastry | region_main | dyen_noncognate_shastry | ling_distance_dyen_noncognate_pct | region | -0.0287165494018947 | 0.0194951900353509 | 0.00747684100966128 | 10 | 0.444444444444444 | 5 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean | state_main | nonzero_mean | ling_distance_nonzero_mean | state | 0.847230978551493 | 0.0671566334316941 | 0.0659370187603894 | 10 | 0.666666666666667 | 3 | 26 | 0.346153846153846 | 573 | estimated | NA |
| state_main\_\_distant_share | state_main | distant_share | ling_share_distance_ge3 | state | 0.0278694360992876 | 0.0794288714733102 | 0.0236997221882597 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_top3_legacy | state_main | top3_legacy | ling_distance_top3_legacy | state | 1.24979225458437 | 0.100332052062355 | 0.0409155790691569 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu | state_main | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.872139712431346 | 0.0714700316922255 | 0.0679330184323957 | 10 | 0.777777777777778 | 2 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_shastry | state_main | nonzero_mean_shastry | ling_distance_nonzero_mean | state | 0.854221402674301 | 0.0710564496160205 | 0.0655383285705257 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_sensitivity_low | state_main | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | state | 0.804231223467154 | 0.0675818881667432 | 0.0660275798863265 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_sensitivity_high | state_main | nonzero_mean_sensitivity_high | ling_distance_nonzero_mean_sensitivity_high | state | 0.445275719612497 | 0.0525286232092515 | 0.0115899218725664 | 10 | 0.555555555555556 | 4 | 26 | 0.307692307692308 | 573 | estimated | NA |
| state_main\_\_nonzero_mean_hindi_urdu_separate | state_main | nonzero_mean_hindi_urdu_separate | ling_distance_nonzero_mean | state | 0.869660296571507 | 0.07101014169077 | 0.06463079836133 | 10 | 0.555555555555556 | 4 | 26 | 0.269230769230769 | 573 | estimated | NA |
| state_main\_\_glottolog_mean | state_main | glottolog_mean | ling_distance_glottolog_nonhindi_mean | state | -0.118360493494532 | -0.0259467259850353 | 0.0155832398904854 | 10 | 0.333333333333333 | 6 | 26 | 0.461538461538462 | 573 | estimated | NA |
| state_main\_\_glottolog_mean_shastry | state_main | glottolog_mean_shastry | ling_distance_glottolog_nonhindi_mean | state | -0.0839373651539127 | -0.0088074357005528 | 0.0164578305572497 | 10 | 0.444444444444444 | 5 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_main\_\_dyen_noncognate | state_main | dyen_noncognate | ling_distance_dyen_noncognate_pct | state | 0.0123273121018981 | 0.0452265777894689 | 0.0120467956686527 | 10 | 0.555555555555556 | 4 | 26 | 0.461538461538462 | 573 | estimated | NA |
| state_main\_\_dyen_noncognate_shastry | state_main | dyen_noncognate_shastry | ling_distance_dyen_noncognate_pct | state | 0.0118283070992545 | 0.0419717557375583 | 0.0119849379709549 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean | state_expanded | nonzero_mean | ling_distance_nonzero_mean | state | 0.927946666623571 | 0.0558810363433017 | 0.0613315740228744 | 10 | 0.555555555555556 | 4 | 26 | 0.346153846153846 | 573 | estimated | NA |
| state_expanded\_\_distant_share | state_expanded | distant_share | ling_share_distance_ge3 | state | 0.0302570088453909 | 0.0741227231458364 | 0.024099475930109 | 10 | 0.555555555555556 | 4 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_top3_legacy | state_expanded | top3_legacy | ling_distance_top3_legacy | state | 1.24824639697791 | 0.0964339580735954 | 0.0446898863412952 | 10 | 0.666666666666667 | 3 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_hindi_urdu | state_expanded | nonzero_mean_hindi_urdu | ling_distance_nonzero_mean | state | 0.948160081321907 | 0.0597464724013058 | 0.0616048712342002 | 10 | 0.666666666666667 | 3 | 26 | 0.346153846153846 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_shastry | state_expanded | nonzero_mean_shastry | ling_distance_nonzero_mean | state | 0.932611572908321 | 0.0586270452826448 | 0.061968977711579 | 10 | 0.666666666666667 | 3 | 26 | 0.384615384615385 | 573 | estimated | NA |
| state_expanded\_\_nonzero_mean_sensitivity_low | state_expanded | nonzero_mean_sensitivity_low | ling_distance_nonzero_mean_sensitivity_low | state | 0.899038830825229 | 0.0555277311119477 | 0.0624856751701968 | 10 | 0.555555555555556 | 4 | 26 | 0.423076923076923 | 573 | estimated | NA |
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
| state_expanded\_\_distance_shares_all | ling_share_distance_1 | 0.033 | 0.018 | 1.782 | 0.075 |
| state_expanded\_\_distance_shares_all | ling_share_distance_2 | -0.018 | 0.044 | -0.411 | 0.681 |
| state_expanded\_\_distance_shares_all | ling_share_distance_3 | -0.006 | 0.013 | -0.470 | 0.639 |
| state_expanded\_\_distance_shares_all | ling_share_distance_4 | 0.262 | 0.071 | 3.692 | 0.000 |
| state_expanded\_\_distance_shares_all | ling_share_distance_5 | 0.025 | 0.050 | 0.498 | 0.619 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_1 | 0.031 | 0.018 | 1.748 | 0.081 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_2 | -0.019 | 0.051 | -0.374 | 0.708 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_3 | -0.010 | 0.020 | -0.484 | 0.629 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_4 | 0.260 | 0.067 | 3.882 | 0.000 |
| state_expanded\_\_distance_shares_all_unmapped | ling_share_distance_5 | 0.020 | 0.050 | 0.397 | 0.692 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_1 | 0.018 | 0.020 | 0.929 | 0.353 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_2 | 0.017 | 0.042 | 0.414 | 0.679 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_3 | -0.002 | 0.012 | -0.158 | 0.875 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_4 | 0.186 | 0.094 | 1.987 | 0.047 |
| state_expanded\_\_distance_shares_mapped | ling_mapped_share_distance_5 | 0.040 | 0.041 | 0.972 | 0.332 |

State-expanded distance-share coefficients

``` r
analysis_table(first_stage_vif, "Main and expanded-control VIF/GVIF diagnostics", max_rows = 40)
```

| term | model_scope | df | vif | gvif | gvif_scaled | status | reason | specification_id |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| ling_distance_nonzero_mean | model_regressors | 1 | 5.37411533753369 | 5.37411533753369 | 2.31821382480859 | estimated | NA | region_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 2.48475573348372 | 2.48475573348372 | 1.57631079850508 | estimated | NA | region_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 3.19290757268839 | 3.19290757268839 | 1.7868708886454 | estimated | NA | region_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 4.21760738483293 | 4.21760738483293 | 2.05368142242971 | estimated | NA | region_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 2.58155832527256 | 2.58155832527256 | 1.60672285266394 | estimated | NA | region_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 3.25847325853406 | 3.25847325853406 | 1.80512416706831 | estimated | NA | region_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 1.71778683284222 | 1.71778683284222 | 1.31064367119451 | estimated | NA | region_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 4.52464025205894 | 4.52464025205894 | 2.12712017809501 | estimated | NA | region_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 3.99036395707579 | 3.99036395707579 | 1.9975895366856 | estimated | NA | region_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.55561556737286 | 4.55561556737286 | 2.13438880417155 | estimated | NA | region_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 3.47885220788372 | 3.47885220788372 | 1.86516814466785 | estimated | NA | region_fe_census_controls |
| factor(region) | model_regressors | 5 | NA | 37.6732540146115 | 1.43748497291406 | estimated | NA | region_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 5.63483867613971 | 5.63483867613971 | 2.37378151398559 | estimated | NA | region_fe_expanded_controls |
| log_population_2001 | model_regressors | 1 | 2.53581194970406 | 2.53581194970406 | 1.59242329476307 | estimated | NA | region_fe_expanded_controls |
| urban_share_2001 | model_regressors | 1 | 3.21520779790046 | 3.21520779790046 | 1.79310005239542 | estimated | NA | region_fe_expanded_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 5.09829107444857 | 5.09829107444857 | 2.25793956394952 | estimated | NA | region_fe_expanded_controls |
| sc_share_2001 | model_regressors | 1 | 2.67197213144836 | 2.67197213144836 | 1.63461681486774 | estimated | NA | region_fe_expanded_controls |
| st_share_2001 | model_regressors | 1 | 3.30440860027322 | 3.30440860027322 | 1.81780323475155 | estimated | NA | region_fe_expanded_controls |
| muslim_share_2001 | model_regressors | 1 | 1.91633170718003 | 1.91633170718003 | 1.38431633204988 | estimated | NA | region_fe_expanded_controls |
| dependency_ratio_2001 | model_regressors | 1 | 5.22796013037652 | 5.22796013037652 | 2.28647329535609 | estimated | NA | region_fe_expanded_controls |
| electricity_access_share_2001 | model_regressors | 1 | 4.93749973225823 | 4.93749973225823 | 2.22204854408229 | estimated | NA | region_fe_expanded_controls |
| log_population_density_2001 | model_regressors | 1 | 3.64981918529371 | 3.64981918529371 | 1.9104499954968 | estimated | NA | region_fe_expanded_controls |
| literacy_share_2001 | model_regressors | 1 | 3.2209774544285 | 3.2209774544285 | 1.79470818085518 | estimated | NA | region_fe_expanded_controls |
| worker_share_2001 | model_regressors | 1 | 3.48477618595882 | 3.48477618595882 | 1.86675552388598 | estimated | NA | region_fe_expanded_controls |
| cultivator_share_workers_2001 | model_regressors | 1 | 5.19558827283551 | 5.19558827283551 | 2.27938330976506 | estimated | NA | region_fe_expanded_controls |
| agricultural_labourer_share_workers_2001 | model_regressors | 1 | 3.92464530053917 | 3.92464530053917 | 1.98107175552507 | estimated | NA | region_fe_expanded_controls |
| factor(region) | model_regressors | 5 | NA | 62.1277508951285 | 1.51122302956186 | estimated | NA | region_fe_expanded_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.66411813822476 | 7.66411813822476 | 2.76841437256505 | estimated | NA | state_fe_census_controls |
| log_population_2001 | model_regressors | 1 | 5.61926161428939 | 5.61926161428939 | 2.37049817850371 | estimated | NA | state_fe_census_controls |
| urban_share_2001 | model_regressors | 1 | 5.95646849482057 | 5.95646849482057 | 2.44058773553023 | estimated | NA | state_fe_census_controls |
| adult_secondary_plus_share_2001 | model_regressors | 1 | 8.07543067368855 | 8.07543067368855 | 2.84173022535366 | estimated | NA | state_fe_census_controls |
| sc_share_2001 | model_regressors | 1 | 4.16640503114339 | 4.16640503114339 | 2.04117736396017 | estimated | NA | state_fe_census_controls |
| st_share_2001 | model_regressors | 1 | 5.58710382884118 | 5.58710382884118 | 2.36370552921492 | estimated | NA | state_fe_census_controls |
| muslim_share_2001 | model_regressors | 1 | 3.49235365030964 | 3.49235365030964 | 1.86878400311797 | estimated | NA | state_fe_census_controls |
| agricultural_worker_share_2001 | model_regressors | 1 | 7.73071457612958 | 7.73071457612958 | 2.78041625950676 | estimated | NA | state_fe_census_controls |
| dependency_ratio_2001 | model_regressors | 1 | 7.78460752704723 | 7.78460752704723 | 2.79009095318544 | estimated | NA | state_fe_census_controls |
| electricity_access_share_2001 | model_regressors | 1 | 10.5297145744734 | 10.5297145744734 | 3.24495216828744 | estimated | NA | state_fe_census_controls |
| log_population_density_2001 | model_regressors | 1 | 6.7716631023344 | 6.7716631023344 | 2.60224193770187 | estimated | NA | state_fe_census_controls |
| factor(state_code_2001) | model_regressors | 34 | NA | 63977.1702744455 | 1.1767300351503 | estimated | NA | state_fe_census_controls |
| ling_distance_nonzero_mean | model_regressors | 1 | 7.92544671366014 | 7.92544671366014 | 2.81521699228677 | estimated | NA | state_fe_expanded_controls |
| Table truncated in rendered note; full CSV has 54 rows. |  |  |  |  |  |  |  |  |

Main and expanded-control VIF/GVIF diagnostics

``` r
analysis_table(first_stage_state_ranges, "State-by-state residual ranges", max_rows = 40)
```

| specification_id | state_code_2001 | n_districts | instrument_min | instrument_max | instrument_range | instrument_sd | treatment_min | treatment_max | treatment_range | treatment_sd |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| instrument_only | 1 | 11 | -1.95676929983649 | 0.886369467922415 | 2.8431387677589 | 1.28256593381945 | -1.72851030069023 | 63.6246894613877 | 65.3531997620779 | 22.1093140815969 |
| instrument_only | 2 | 12 | -2.02972739930956 | 1.86563277562347 | 3.89536017493303 | 1.32072078342093 | -13.1260780236274 | 22.8850966556704 | 36.0111746792977 | 9.35701241783805 |
| instrument_only | 3 | 16 | -2.04468945407418 | -2.02132802418258 | 0.0233614298915996 | 0.00767986612439207 | -4.46694830456123 | 20.4174860204924 | 24.8844343250536 | 6.95308526038906 |
| instrument_only | 4 | 1 | -1.74634422961262 | -1.74634422961262 | 0 | NA | 29.0887453324456 | 29.0887453324456 | 0 | NA |
| instrument_only | 5 | 12 | -1.18735057305129 | 1.05421770188751 | 2.2415682749388 | 0.684324584971317 | -13.3468176069717 | 29.5608828194392 | 42.9077004264109 | 11.4752595237954 |
| instrument_only | 6 | 19 | -2.03700438675809 | -1.07135970262856 | 0.96564468412953 | 0.283706846558088 | -9.95292621139165 | 27.2921562207821 | 37.2450824321737 | 9.28749951672546 |
| instrument_only | 7 | 7 | -1.54219120560884 | -0.348331060981955 | 1.19386014462688 | 0.394978586421688 | -14.8990140839211 | 36.7288772894966 | 51.6278913734177 | 15.4036748898376 |
| instrument_only | 8 | 27 | -2.04348893800603 | -1.87148387808073 | 0.1720050599253 | 0.051626934032749 | -14.8990140839211 | 2.06987459207157 | 16.9688886759927 | 5.26178839124252 |
| instrument_only | 9 | 68 | -1.97931926001089 | 0.352033951275515 | 2.3313532112864 | 0.656571422691291 | -14.8990140839211 | 0.467348741019265 | 15.3663628249404 | 3.88831210305508 |
| instrument_only | 10 | 37 | -0.131325259095095 | 1.79470023161785 | 1.92602549071295 | 0.354269274574158 | -14.8990140839211 | 2.27264783309537 | 17.1716619170165 | 3.31642856435554 |
| instrument_only | 11 | 4 | 1.4184102742707 | 1.88652404928696 | 0.46811377501626 | 0.223044167486406 | 64.0753614898039 | 67.016567645357 | 2.9412061555531 | 1.28367574500589 |
| instrument_only | 12 | 13 | 0.287793426568335 | 1.83807180629017 | 1.55027837972184 | 0.554193192453198 | 40.4173744253803 | 81.0761055759627 | 40.6587311505824 | 13.8367092842222 |
| instrument_only | 13 | 8 | 0.534652897756595 | 1.90061960341573 | 1.36596670565914 | 0.609924122099548 | 55.5792907532756 | 76.6256257720213 | 21.0463350187457 | 5.70471838138252 |
| instrument_only | 14 | 9 | 1.75799531552022 | 1.94545146128966 | 0.18745614576944 | 0.0682723160414595 | 13.4056159581393 | 71.3614853747201 | 57.9558694165808 | 18.9553593117256 |
| instrument_only | 15 | 8 | 0.695700887118485 | 1.94349322979479 | 1.2477923426763 | 0.418198484652358 | 9.03539901473087 | 51.3563871026949 | 42.320988087964 | 15.2211538043371 |
| instrument_only | 16 | 4 | 0.401764694367005 | 0.908068195782785 | 0.50630350141578 | 0.223585411471733 | -14.333598534694 | -10.0297447862768 | 4.30385374841714 | 1.87016207122847 |
| instrument_only | 17 | 7 | 1.19318472224951 | 1.9257558590041 | 0.7325711367546 | 0.290810858300757 | 22.2866699684821 | 46.0676336593552 | 23.7809636908731 | 7.33629548269078 |
| instrument_only | 18 | 22 | -0.00828309190438503 | 1.49376856470772 | 1.50205165661211 | 0.436489701136629 | -14.8670800789279 | -0.540576432260234 | 14.3265036466676 | 3.95040621978854 |
| instrument_only | 19 | 18 | -0.0727188824063454 | 0.254821318744065 | 0.32754020115041 | 0.101459004409985 | -14.6051920321776 | 6.47153441263196 | 21.0767264448095 | 5.52787650045653 |
| instrument_only | 20 | 18 | -0.0244261178589853 | 1.41653806134723 | 1.44096417920621 | 0.449724955673133 | -14.8398031319554 | 4.65520390120977 | 19.4950070331651 | 5.51420001295925 |
| instrument_only | 21 | 29 | -0.0452168136421451 | 1.09899343445338 | 1.14421024809553 | 0.342448598902929 | -14.4227421717253 | -5.1554817931903 | 9.26726037853496 | 2.7443591122409 |
| instrument_only | 22 | 16 | -0.324056047204455 | 1.81913827890782 | 2.14319432611228 | 0.55439624243688 | -14.5994089190088 | -6.49858222927209 | 8.10082668973675 | 2.40834676626716 |
| instrument_only | 23 | 44 | -2.0304881860506 | 1.89011139476202 | 3.92059958081261 | 1.17037230114088 | -14.8990140839211 | 8.04724742431587 | 22.946261508237 | 5.1416190138038 |
| instrument_only | 24 | 25 | -2.04056630140149 | -0.892500698500905 | 1.14806560290058 | 0.254460585338103 | -14.8990140839211 | 4.11818599081957 | 19.0172000747407 | 4.92266595157379 |
| instrument_only | 25 | 2 | -2.03160066219217 | -1.65989079648855 | 0.37170986570362 | 0.262838566672971 | -8.00692903225135 | 18.4026607387735 | 26.4095897710248 | 18.6744000154465 |
| instrument_only | 26 | 1 | -1.37632433588608 | -1.37632433588608 | 0 | NA | -10.582928800204 | -10.582928800204 | 0 | NA |
| instrument_only | 27 | 33 | -1.6951307391106 | 0.0658136109057046 | 1.7609443500163 | 0.256358588759681 | -14.8990140839211 | 21.6673498113684 | 36.5663638952895 | 8.93882168572837 |
| instrument_only | 28 | 23 | 1.28493137266284 | 1.94849004798783 | 0.66355867532499 | 0.192842579354385 | -11.789635028031 | 46.407083749861 | 58.1967187778919 | 11.2780079315081 |
| instrument_only | 29 | 27 | 0.955267114174135 | 1.930720087095 | 0.97545297292086 | 0.255007244602427 | -14.8990140839211 | 43.223416156469 | 58.1224302403901 | 12.2728643175785 |
| instrument_only | 30 | 2 | -0.828306803654225 | -0.709924014366335 | 0.11838278928789 | 0.0837092730812453 | 18.4573875387927 | 45.7309153131761 | 27.2735277743834 | 19.2852964361462 |
| instrument_only | 31 | 1 | 1.92350587994718 | 1.92350587994718 | 0 | NA | 12.7940798315329 | 12.7940798315329 | 0 | NA |
| instrument_only | 32 | 14 | 1.84725061127115 | 1.95315362242036 | 0.10590301114921 | 0.028109901791827 | 2.67500390922837 | 47.8516469308443 | 45.1766430216159 | 11.9908795765426 |
| instrument_only | 33 | 29 | 1.84749737853693 | 1.9541575057177 | 0.106660127180779 | 0.0248111146403481 | -9.81017002374155 | 40.6013531278147 | 50.4115231515562 | 11.0367470170818 |
| instrument_only | 34 | 4 | 1.93806869863449 | 1.95282719420095 | 0.0147584955664604 | 0.00681906182806368 | 12.5266930410341 | 58.2899068058393 | 45.7632137648052 | 19.287824065982 |
| instrument_only | 35 | 2 | 1.0970702084783 | 1.3214907240296 | 0.224420515551291 | 0.158689268383699 | -7.28812987104306 | 26.3037698375391 | 33.5918997085821 | 23.7530600768768 |
| region_fe | 1 | 11 | -0.466863291912915 | 2.37627547584608 | 2.84313876775899 | 1.28256593381946 | -5.55416262929903 | 59.7990371327789 | 65.3531997620779 | 22.1093140815968 |
| region_fe | 2 | 12 | -0.539821391385985 | 3.35553878354704 | 3.89536017493303 | 1.32072078342093 | -16.9517303522362 | 19.0594443270616 | 36.0111746792977 | 9.35701241783805 |
| region_fe | 3 | 16 | -0.554783446150604 | -0.531422016259005 | 0.0233614298915996 | 0.00767986612439202 | -8.29260063317003 | 16.5918336918836 | 24.8844343250536 | 6.95308526038906 |
| region_fe | 4 | 1 | -0.256438221689045 | -0.256438221689045 | 0 | NA | 25.2630930038368 | 25.2630930038368 | 0 | NA |
| region_fe | 5 | 12 | -0.726504443577987 | 1.51506383136081 | 2.2415682749388 | 0.684324584971318 | -3.06810590678395 | 39.8395945196269 | 42.9077004264109 | 11.4752595237954 |
| Table truncated in rendered note; full CSV has 875 rows. |  |  |  |  |  |  |  |  |  |  |

State-by-state residual ranges

``` r
analysis_table(first_stage_state_deletion[order(abs(first_stage_state_deletion$estimate_change), decreasing = TRUE), ], "Leave-one-state-out influence", max_rows = 30)
```

| specification_id | specification | sequence | treatment | instrument | fixed_effect | control_blocks | n_controls | estimate | std.error | statistic | p.value | excluded_instrument_f | partial_r_squared | residual_instrument_sd | residual_treatment_sd | residual_correlation | instrument_variance_remaining | n | n_states | n_regions | status | reason | omitted_state | estimate_change | f_change |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.83817893459659 | 1.18520364937826 | 1.55093931372964 | 0.121573913182609 | 2.40541275487216 | 0.0121899853574636 | 0.405937364030501 | 6.75842066518467 | 0.110408266707995 | 0.0883012476895495 | 529 | 34 | 6 | estimated | NA | 23 | 0.910232267972994 | 1.35617168142767 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.23533497437104 | 0.555785736771724 | 0.423427516758493 | 0.672160489696705 | 0.179290861948264 | 0.000324667905600943 | 0.458820196699089 | 5.9925176493585 | 0.0180185433817876 | 0.113179872466474 | 562 | 34 | 6 | estimated | NA | 1 | -0.692611692252554 | -0.869950211496225 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.2067133811655 | 1.01882346598171 | 1.1844185194564 | 0.23679588896634 | 1.4028472292313 | 0.0074371020382084 | 0.470728663645959 | 6.58677612679421 | 0.0862386342552366 | 0.119841420640956 | 561 | 34 | 6 | estimated | NA | 2 | 0.278766714541903 | 0.353606155786811 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.19267309392149 | 1.15844173689739 | 1.02954948525576 | 0.303766116843484 | 1.05997214259039 | 0.00612103632685947 | 0.45343910730037 | 6.9123849665503 | 0.0782370521355421 | 0.103397022821908 | 505 | 34 | 6 | estimated | NA | 9 | 0.264726427297897 | 0.0107310691459037 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.785505931859173 | 0.859943199726567 | 0.913439320305036 | 0.3614414627924 | 0.834371391879326 | 0.00349982606690843 | 0.481329699179119 | 6.39100118075541 | 0.0591593278098079 | 0.125186351781049 | 560 | 34 | 6 | estimated | NA | 12 | -0.142440734764422 | -0.214869681565163 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.787510628215314 | 0.847848060414648 | 0.928834616700278 | 0.353424781136101 | 0.862733745180753 | 0.00349608805708227 | 0.488190235255091 | 6.50211027432719 | 0.0591277266355041 | 0.132988390705925 | 546 | 34 | 6 | estimated | NA | 29 | -0.14043603840828 | -0.186507328263735 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.04389135281161 | 0.913235793540583 | 1.14306881113856 | 0.253539057962256 | 1.30660630699772 | 0.00612390811754814 | 0.483931025289132 | 6.45541895527418 | 0.0782554031204818 | 0.127420541401627 | 565 | 34 | 6 | estimated | NA | 15 | 0.115944686188018 | 0.257365233553228 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.816208991769823 | 0.864215444263393 | 0.944450827843643 | 0.345399143702028 | 0.891987366214542 | 0.00377149393900248 | 0.494761460908954 | 6.57567788875022 | 0.0614124900895674 | 0.140562001448408 | 544 | 34 | 6 | estimated | NA | 33 | -0.111737674853771 | -0.157253707229947 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.841205653583807 | 0.908040028508545 | 0.926397104944247 | 0.354671887863479 | 0.858211596049082 | 0.00380141257149797 | 0.482804101276602 | 6.58719667093323 | 0.0616555964329184 | 0.125966345391476 | 565 | 34 | 6 | estimated | NA | 13 | -0.0867410130397865 | -0.191029477395406 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 1.00823648909157 | 0.958283513912126 | 1.05212755354155 | 0.293236363091081 | 1.10697238892133 | 0.00540959150895003 | 0.47924667367462 | 6.56960536429703 | 0.0735499252817308 | 0.122120690451631 | 561 | 34 | 6 | estimated | NA | 5 | 0.080289822467972 | 0.0577313154768408 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.849063701544054 | 0.898433582438361 | 0.945048936438555 | 0.345087624031401 | 0.893117492263645 | 0.00389704933788292 | 0.48924659596689 | 6.65426565900795 | 0.0624263513100284 | 0.124934264957566 | 551 | 34 | 6 | estimated | NA | 18 | -0.0788829650795397 | -0.156123581180844 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.987600139728987 | 0.869375475712562 | 1.135988036607 | 0.256494057924833 | 1.29046881931422 | 0.00545656965042622 | 0.488491523989663 | 6.53097954635572 | 0.0738685971873575 | 0.132313133157774 | 559 | 34 | 6 | estimated | NA | 32 | 0.0596534731053926 | 0.241227745869728 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.985775595688534 | 0.920309129578898 | 1.0711353000916 | 0.284629521314999 | 1.14733083110233 | 0.00519836235876228 | 0.491838535156174 | 6.72461368487802 | 0.0720996696161802 | 0.124398132305444 | 544 | 34 | 6 | estimated | NA | 21 | 0.0578289290649396 | 0.0980897576578377 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.877419848559696 | 0.951216255151549 | 0.922418896657632 | 0.356753227270085 | 0.850856620911083 | 0.00421554648893958 | 0.491219576333563 | 6.63828936107294 | 0.0649272399608809 | 0.134767680099964 | 550 | 34 | 6 | estimated | NA | 28 | -0.0505268180638977 | -0.198384452533406 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.886067206992171 | 0.89432469978519 | 0.990766784373726 | 0.322262042055266 | 0.981618821018254 | 0.00441331800560233 | 0.484787716757746 | 6.46599940296906 | 0.0664328082019926 | 0.125947022194891 | 566 | 34 | 6 | estimated | NA | 7 | -0.0418794596314229 | -0.0676222524262347 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.890164309421475 | 0.902592946043894 | 0.986230075609505 | 0.32449913464147 | 0.97264976203673 | 0.00430058579487688 | 0.492714571226495 | 6.68808486934522 | 0.0655788517349779 | 0.139751723916902 | 546 | 34 | 6 | estimated | NA | 8 | -0.0377823572021189 | -0.0765913114077583 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.898960222956504 | 0.91468400688825 | 0.982809600022156 | 0.326155580336155 | 0.96591470989571 | 0.00443529396758883 | 0.484415898397561 | 6.53879405054279 | 0.0665980027297231 | 0.126494379203839 | 571 | 34 | 6 | estimated | NA | 35 | -0.0289864436670904 | -0.0833263635487785 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.956294826663482 | 0.893170091097998 | 1.07067493212618 | 0.284825043334671 | 1.1463448102834 | 0.0049857641960303 | 0.490408030956765 | 6.64176511390238 | 0.0706099440307722 | 0.125680520196063 | 555 | 34 | 6 | estimated | NA | 19 | 0.0283481600398877 | 0.097103736838908 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.95389437240071 | 0.936842659924841 | 1.01820125534979 | 0.309054903068425 | 1.03673379639589 | 0.00507609169440662 | 0.485234590457351 | 6.49661768136128 | 0.0712466960245141 | 0.127968571235986 | 569 | 34 | 6 | estimated | NA | 34 | 0.0259477057771162 | -0.0125072770485992 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.947834451575965 | 0.905676514125253 | 1.04654855988115 | 0.295791262559159 | 1.09526388818931 | 0.00487848188287529 | 0.484403427956857 | 6.57351033127072 | 0.0698461300493969 | 0.126961703496533 | 571 | 34 | 6 | estimated | NA | 25 | 0.019887784952371 | 0.046022814744823 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.909989027554914 | 0.895355375573469 | 1.01634395948321 | 0.309941924372017 | 1.032955043978 | 0.00496782187235204 | 0.485342179423984 | 6.26615573125466 | 0.070482777132797 | 0.129038261031902 | 564 | 34 | 6 | estimated | NA | 14 | -0.0179576390686801 | -0.0162860294664853 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.913110535928373 | 0.90500209474558 | 1.00895958277873 | 0.313462369333105 | 1.01799943968102 | 0.00448454390988557 | 0.483385533142306 | 6.59109941388496 | 0.0669667373394142 | 0.126515909613093 | 569 | 34 | 6 | estimated | NA | 11 | -0.0148361306952209 | -0.031241633763468 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.942589641646017 | 0.95315348306898 | 0.988916956596592 | 0.323182084595039 | 0.977956747044265 | 0.00481350566897305 | 0.488257290771746 | 6.63346826921502 | 0.0693794326077423 | 0.135451788255223 | 548 | 34 | 6 | estimated | NA | 24 | 0.0146429750224225 | -0.0712843264002233 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.941835795220652 | 0.927102715064654 | 1.01589152951081 | 0.310165584012295 | 1.03203559973181 | 0.0047963331460393 | 0.489327621560561 | 6.65457391299376 | 0.069255564007791 | 0.126222499961492 | 555 | 34 | 6 | estimated | NA | 20 | 0.0138891285970582 | -0.0172054737126834 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.941336626864736 | 0.941205998797373 | 1.00013878796728 | 0.317718564154554 | 1.00027759519666 | 0.0046857137400305 | 0.484499064195744 | 6.66269638456613 | 0.0684522734467709 | 0.124082297375275 | 557 | 34 | 6 | estimated | NA | 22 | 0.013389960241142 | -0.048963478247833 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.917329576265923 | 0.947199705660918 | 0.96846480291698 | 0.333292017460889 | 0.937924074489026 | 0.00445502902474746 | 0.493119034721512 | 6.77722484255781 | 0.066746003811057 | 0.123257246404768 | 536 | 34 | 6 | estimated | NA | 10 | -0.0106170903576714 | -0.111316998955463 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.92476316439154 | 0.905742572821507 | 1.02099999728486 | 0.30773103538201 | 1.04244099445568 | 0.00463080555869774 | 0.485869526642823 | 6.60270525423945 | 0.0680500224739146 | 0.12832946535548 | 566 | 34 | 6 | estimated | NA | 17 | -0.00318350223205366 | -0.0068000789888063 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.925389607643131 | 0.90723030314809 | 1.02001620143422 | 0.30819237319275 | 1.0404330511883 | 0.00468594887323207 | 0.484481701365782 | 6.54942576020169 | 0.0684539909226106 | 0.126317790280988 | 571 | 34 | 6 | estimated | NA | 30 | -0.00255705898046266 | -0.00880802225619148 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.930369937952401 | 0.909687029662741 | 1.02273629019128 | 0.306921251053011 | 1.04598951927423 | 0.00480948614564901 | 0.489788789036247 | 6.57075342661725 | 0.0693504588712017 | 0.13312449658211 | 554 | 34 | 6 | estimated | NA | 6 | 0.0024232713288066 | -0.0032515541702578 |
| state_fe_expanded_controls | State fixed effects + expanded Census controls | 9 | emi_exposure_all_children_0708 | ling_distance_nonzero_mean | state | basic_scale_geography;social_composition;human_capital;demography;economic_structure;basic_development | 13 | 0.929646572290831 | 0.905491407550976 | 1.02667630475389 | 0.305049331242248 | 1.05406423474309 | 0.00466461094828924 | 0.485272775801688 | 6.60535382901425 | 0.0682979571311527 | 0.126332845580375 | 569 | 34 | 6 | estimated | NA | 16 | 0.00169990566723666 | 0.00482316129860427 |
| Table truncated in rendered note; full CSV has 35 rows. |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |

Leave-one-state-out influence

``` r
analysis_table(first_stage_district_influence[order(first_stage_district_influence$cooks_distance, decreasing = TRUE), ], "Most influential districts in the expanded first stage", max_rows = 30)
```

| state_code_2001 | district_code_2001 | leverage | cooks_distance | studentized_residual | instrument_dfbeta |
|:---|:---|:---|:---|:---|:---|
| 35 | 1 | 0.538212103904754 | 0.207030866581889 | 2.97223129769918 | 0.0289864436670907 |
| 35 | 2 | 0.538212103904755 | 0.207030866581889 | -2.97223129769918 | 0.0289864436670908 |
| 30 | 1 | 0.502239274213472 | 0.147378283114397 | -2.69117133728197 | 0.00255705898045777 |
| 30 | 2 | 0.502239274213472 | 0.147378283114397 | 2.69117133728197 | 0.00255705898045733 |
| 34 | 3 | 0.295256454437258 | 0.120797365188863 | 3.80682532985172 | -0.0262938818435361 |
| 14 | 1 | 0.127748487289063 | 0.113070144238036 | 6.37927779898376 | -0.0319379841713681 |
| 1 | 12 | 0.135593568070074 | 0.0889055837120491 | -5.41018520978411 | 0.469929940144412 |
| 25 | 2 | 0.54096362442525 | 0.0829655931493574 | 1.86168430456566 | -0.0198877849523648 |
| 25 | 1 | 0.540963624425249 | 0.0829655931493572 | -1.86168430456565 | -0.0198877849523647 |
| 34 | 1 | 0.299942171761755 | 0.0737187125052722 | -2.9244383929818 | 0.00482732119699555 |
| 1 | 9 | 0.120574705727383 | 0.0646505018834312 | 4.91171282846487 | 0.0917155191161339 |
| 1 | 10 | 0.156276923773901 | 0.0605263132640452 | 4.06019415848783 | -0.281271987983322 |
| 7 | 2 | 0.152260307231148 | 0.0584679212116665 | -4.05221840128717 | 0.0420042136996978 |
| 15 | 8 | 0.157081011016881 | 0.0470532621733839 | 3.55629495528973 | -0.00433482613089334 |
| 7 | 4 | 0.149719582830189 | 0.0379346537248617 | 3.2791581123925 | 0.0228310235390359 |
| 14 | 2 | 0.150657118565714 | 0.0353559526321031 | -3.15171395403876 | 0.0624548621856319 |
| 12 | 6 | 0.0996422352741479 | 0.0318226171867034 | 3.8015069756262 | 0.0900285058040233 |
| 1 | 5 | 0.140473832502644 | 0.0292934188671501 | 2.98586760825639 | 0.164172684447142 |
| 15 | 7 | 0.20889894676604 | 0.0273020096670376 | 2.25963393965016 | -0.126433269162342 |
| 1 | 1 | 0.124317367019829 | 0.0262914853137848 | -3.03592992170607 | -0.0821096287142906 |
| 1 | 14 | 0.183934651554748 | 0.025446887644633 | -2.36230509734326 | 0.165105807820993 |
| 1 | 6 | 0.119127838708915 | 0.0230216847925964 | 2.90861383160478 | 0.119054978321943 |
| 14 | 5 | 0.17451434458917 | 0.0226349098500361 | -2.29983599114193 | -0.0563270124511651 |
| 32 | 10 | 0.100700738387286 | 0.0224794756402778 | 3.16320449199166 | -0.0113085338326199 |
| 5 | 5 | 0.126020848418863 | 0.0216314466380348 | 2.72786682145703 | -0.123192293278713 |
| 29 | 20 | 0.0938020730087223 | 0.018875048829695 | 3.01208499550616 | 0.0217480445819215 |
| 1 | 13 | 0.200899542792867 | 0.0187280907906811 | -1.91539848950645 | 0.125398759660574 |
| 6 | 8 | 0.0596268732919032 | 0.0180822521417005 | 3.78536604494359 | 0.0270566209653171 |
| 28 | 5 | 0.179083844612659 | 0.0172801982336996 | 1.97557373769786 | 0.0399992217452638 |
| 14 | 9 | 0.142983980846962 | 0.0167295175937743 | -2.22495615512314 | 0.0294674299324886 |
| Table truncated in rendered note; full CSV has 573 rows. |  |  |  |  |  |

Most influential districts in the expanded first stage

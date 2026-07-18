# Build named values for paper/report.qmd from targets outputs.

#' Build report values from current targets
#'
#' @return Named list of values used by paper/report.qmd.
build_report_values <- function(ame_results, first_stage_tests, iv_models, selection_data, district_panel, diag_spatial_autocorrelation = NULL, cfg = list()) {
  values <- list()

  model <- if (is.list(iv_models) && length(iv_models)) iv_models[[1]] else iv_models
  unavailable_ame <- "Full AME result is not available in the current draft pipeline."
  unavailable_iv <- "The requested IV coefficient is not available from the active model specification."
  unavailable_first_stage <- "The requested first-stage coefficient is not available from the active model specification."
  unavailable_selection <- "The requested relationship/age summary is not available from the active selection data."

  values <- set_report_value(values, "head_male_age", selection_summary_value(selection_data, 1, 1), unavailable_selection)
  values <- set_report_value(values, "married_child_male_age", selection_summary_value(selection_data, 3, 1), unavailable_selection)
  values <- set_report_value(values, "parent_in_law_male_age", selection_summary_value(selection_data, 7, 1), unavailable_selection)
  values <- set_report_value(values, "spouse_male_age", selection_summary_value(selection_data, 2, 1), unavailable_selection)
  values <- set_report_value(values, "spouse_male_child_share", selection_summary_value(selection_data, 2, 1, "share_children"), unavailable_selection)
  values <- set_report_value(values, "unmarried_child_male_age", selection_summary_value(selection_data, 5, 1), unavailable_selection)

  values <- set_report_value(values, "ame_age", lookup_ame(ame_results, "AGE", digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_age_abs_pct", abs(lookup_ame(ame_results, "AGE", multiply = 100, digits = 3)), unavailable_ame)
  values <- set_report_value(values, "ame_muslim", lookup_ame(ame_results, "RELIGION", contrast_pattern = "Muslim", digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_age_pct", lookup_ame(ame_results, "AGE", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_sex_pct", lookup_ame(ame_results, "SEX", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_hh_size_pct", lookup_ame(ame_results, "HH_SIZE", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_muslim_pct", lookup_ame(ame_results, "RELIGION", contrast_pattern = "Muslim", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_st_pct", lookup_ame(ame_results, "SOCIAL_GROUP", contrast_pattern = "Tribe", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_textbooks_s", lookup_ame_s_value(ame_results, "dmean_num.*RECD_TXT_BOOKS|RECD_TXT_BOOKS.*dmean_num", digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_edu_free_pct", lookup_ame(ame_results, "dmean_num.*IS_EDU_FREE|IS_EDU_FREE.*dmean_num", multiply = 100, digits = 3), unavailable_ame)
  values <- set_report_value(values, "ame_edu_free_s", lookup_ame_s_value(ame_results, "dmean_num.*IS_EDU_FREE|IS_EDU_FREE.*dmean_num", digits = 3), unavailable_ame)

  morans_unavailable <- "Moran's I diagnostic is not available from the active spatial-autocorrelation target."
  values <- set_report_value(
    values,
    "moran_iv_residual_p",
    spatial_p_value(diag_spatial_autocorrelation, legacy_name = "m_cons_resid", pattern = "consumption.*resid|iv_residual"),
    morans_unavailable
  )
  values <- set_report_value(
    values,
    "moran_consumption_growth_p",
    spatial_p_value(diag_spatial_autocorrelation, legacy_name = "m_cons", pattern = "consumption.*growth|consumption_pct_change"),
    morans_unavailable
  )

  values <- set_report_value(values, "kappa", condition_number_value(model), "The model design matrix condition number is not available from the active model specification.")

  values$partial_f <- value_or_status(first_available_number(first_stage_tests, c("partial_f", "model_f", "statistic", "f_stat", "F")), unavailable_first_stage)
  values$partial_p <- value_or_status(first_available_number(first_stage_tests, c("partial_p", "model_p", "p.value", "p_value", "p")), unavailable_first_stage)
  values$partial_f_report <- format_report_value(values$partial_f, function(x) round(x, 2))
  values$partial_p_report <- format_report_value(values$partial_p, function(x) signif(x, 2))

  values <- set_report_value(values, "first_stage_linguistic_distance_estimate", first_stage_report_value(first_stage_tests, iv_models, district_panel, c("wavg_ling_degrees", "linguistic_distance", "ling_degrees"), "estimate", 2), unavailable_first_stage)
  values$first_stage_linguistic_distance_p <- value_or_status(first_stage_report_value(first_stage_tests, iv_models, district_panel, c("wavg_ling_degrees", "linguistic_distance", "ling_degrees"), "p.value", 3), unavailable_first_stage)
  values <- set_report_value(values, "first_stage_gini_estimate", first_stage_report_value(first_stage_tests, iv_models, district_panel, "gini_cons_0708", "estimate", 2), unavailable_first_stage)
  values$first_stage_gini_p <- value_or_status(first_stage_report_value(first_stage_tests, iv_models, district_panel, "gini_cons_0708", "p.value", 3), unavailable_first_stage)

  iv_terms <- list(
    iv_emie = "EMIE",
    iv_pct_urban = "pct_urban",
    iv_pct_head_secondary_plus = "pct_head_secondary_plus",
    iv_pct_muslim = "pct_muslim",
    iv_pct_st = "pct_st",
    iv_pct_obc = "pct_obc",
    iv_pct_medium_land = "pct_medium_land",
    iv_pct_large_land = "pct_large_land",
    iv_gini_cons_0708 = "gini_cons_0708",
    iv_pct_fem_head = "pct_fem_head"
  )
  for (name in names(iv_terms)) {
    values[[paste0(name, "_estimate")]] <- value_or_status(coefficient_value(model, iv_terms[[name]], digits = 3), unavailable_iv)
    values[[paste0(name, "_p")]] <- value_or_status(p_value(model, iv_terms[[name]], digits = 3), unavailable_iv)
  }

  values$iv_emie_estimate_report <- value_or_status(coefficient_value(model, iv_terms$iv_emie, digits = 2), unavailable_iv)
  values$iv_pct_urban_estimate_report <- value_or_status(coefficient_value(model, iv_terms$iv_pct_urban, digits = 2), unavailable_iv)
  values$iv_pct_head_secondary_plus_estimate_report <- value_or_status(coefficient_value(model, iv_terms$iv_pct_head_secondary_plus, digits = 2), unavailable_iv)

  values
}

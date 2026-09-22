# Build named values for paper/paper.qmd from targets outputs.

#' Build report values from current targets
#'
#' @return Named list of values used by paper/paper.qmd.
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
  spatial_keys <- list(
    moran_emi_i = c("m_EMIE", "estimate", "rook"),
    moran_linguistic_distance_i = c("m_wavg_ling_degrees", "estimate", "rook"),
    moran_consumption_growth_i = c("m_cons", "estimate", "rook"),
    moran_consumption_growth_p = c("m_cons", "p.value", "rook"),
    moran_gini_change_i = c("m_gini", "estimate", "rook"),
    moran_iv_residual_i = c("m_cons_resid", "estimate", "rook"),
    moran_iv_residual_p = c("m_cons_resid", "p.value", "rook"),
    moran_first_stage_residual_i = c("m_fscons_resid", "estimate", "rook"),
    moran_first_stage_residual_p = c("m_fscons_resid", "p.value", "rook"),
    moran_iv_residual_p_queen = c("m_cons_resid", "p.value", "queen"),
    moran_first_stage_residual_p_queen = c("m_fscons_resid", "p.value", "queen")
  )
  for (key in names(spatial_keys)) {
    entry <- spatial_keys[[key]]
    values <- set_report_value(
      values, key,
      spatial_diagnostic_value(
        diag_spatial_autocorrelation,
        legacy_name = entry[[1L]], field = entry[[2L]], contiguity = entry[[3L]]
      ),
      morans_unavailable
    )
  }

  values <- set_report_value(values, "kappa", condition_number_value(model), "The model design matrix condition number is not available from the active model specification.")

  values$partial_f <- value_or_status(first_available_number(first_stage_tests, c("partial_f", "model_f", "statistic", "f_stat", "F")), unavailable_first_stage)
  values$partial_p <- value_or_status(first_available_number(first_stage_tests, c("partial_p", "model_p", "p.value", "p_value", "p")), unavailable_first_stage)
  values$partial_f_report <- format_report_value(values$partial_f, function(x) round(x, 2))
  values$partial_p_report <- format_report_value(values$partial_p, function(x) signif(x, 2))
  values$effective_f <- value_or_status(
    first_available_number(first_stage_tests, "effective_f"),
    unavailable_first_stage
  )
  values$effective_f_report <- format_report_value(
    values$effective_f, function(x) round(x, 2)
  )
  values$effective_f_critical_value <- value_or_status(
    first_available_number(first_stage_tests, "effective_f_critical_value"),
    unavailable_first_stage
  )
  values$effective_f_critical_value_report <- format_report_value(
    values$effective_f_critical_value, function(x) round(x, 2)
  )
  values$effective_f_p_value <- value_or_status(
    first_available_number(first_stage_tests, "effective_f_p_value"),
    unavailable_first_stage
  )

  spec <- preferred_iv_variables()
  instrument_terms <- c(spec$instrument, "linguistic_distance", "ling_degrees")
  values <- set_report_value(values, "first_stage_linguistic_distance_estimate", first_stage_report_value(first_stage_tests, iv_models, district_panel, instrument_terms, "estimate", 2), unavailable_first_stage)
  values$first_stage_linguistic_distance_p <- value_or_status(first_stage_report_value(first_stage_tests, iv_models, district_panel, instrument_terms, "p.value", 3), unavailable_first_stage)

  values$iv_emie_estimate <- value_or_status(coefficient_value(
    model, spec$treatment, digits = 3, data = district_panel
  ), unavailable_iv)
  values$iv_emie_p <- value_or_status(p_value(
    model, spec$treatment, digits = 3, data = district_panel
  ), unavailable_iv)
  values$iv_emie_estimate_report <- value_or_status(coefficient_value(
    model, spec$treatment, digits = 2, data = district_panel
  ), unavailable_iv)

  values
}

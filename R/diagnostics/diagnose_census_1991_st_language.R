# Descriptive pre-treatment ST language-acquisition diagnostic.

census_1991_st_language_outcomes <- function() {
  c(
    "english_acquisition_share",
    "hindi_acquisition_share",
    "english_minus_hindi_acquisition_share",
    "bilingual_share"
  )
}

census_1991_st_language_fit <- function(panel, outcome, sample_id, hindi_belt_only = FALSE) {
  x <- safe_df(panel)
  keep <- x$preferred_st_language_sample %in% TRUE
  if (hindi_belt_only) keep <- keep & x$hindi_belt_1991 %in% TRUE
  keep <- keep & is.finite(num(x[[outcome]])) & is.finite(num(x$shastry_distance_1991)) &
    is.finite(num(x$mother_tongue_speakers)) & num(x$mother_tongue_speakers) > 0
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 2L || length(unique(x$state_code_1991)) < 2L ||
      length(unique(x$shastry_distance_1991)) < 2L) {
    return(data.frame(
      sample = sample_id, outcome = outcome, n_language_cells = nrow(x),
      n_districts = length(unique(paste(x$state_code_1991, x$district_code_1991))),
      n_states = length(unique(x$state_code_1991)), estimate = NA_real_,
      std_error_state_clustered = NA_real_, p_value_state_clustered = NA_real_,
      weighted_outcome_mean = NA_real_, stringsAsFactors = FALSE
    ))
  }
  formula <- stats::as.formula(paste(outcome, "~ shastry_distance_1991 + factor(state_code_1991)"))
  fit <- stats::lm(formula, data = x, weights = mother_tongue_speakers)
  need_pkg("sandwich", "state-clustered ST language diagnostics")
  vcov <- sandwich::vcovCL(fit, cluster = x$state_code_1991, type = "HC1")
  coefficient <- "shastry_distance_1991"
  estimate <- unname(stats::coef(fit)[[coefficient]])
  standard_error <- sqrt(vcov[coefficient, coefficient])
  t_value <- estimate / standard_error
  degrees_freedom <- length(unique(x$state_code_1991)) - 1L
  data.frame(
    sample = sample_id,
    outcome = outcome,
    n_language_cells = nrow(x),
    n_districts = length(unique(paste(x$state_code_1991, x$district_code_1991))),
    n_states = length(unique(x$state_code_1991)),
    estimate = estimate,
    std_error_state_clustered = standard_error,
    p_value_state_clustered = 2 * stats::pt(abs(t_value), df = degrees_freedom, lower.tail = FALSE),
    weighted_outcome_mean = stats::weighted.mean(num(x[[outcome]]), num(x$mother_tongue_speakers)),
    stringsAsFactors = FALSE
  )
}

build_census_1991_st_language_diagnostic <- function(st17, st16) {
  panel <- build_census_1991_st_language_panel(st17, st16)
  outcomes <- census_1991_st_language_outcomes()
  estimates <- safe_bind_rows(lapply(outcomes, function(outcome) {
    safe_bind_rows(list(
      census_1991_st_language_fit(panel, outcome, "validated_all_states", FALSE),
      census_1991_st_language_fit(panel, outcome, "validated_hindi_belt", TRUE)
    ))
  }))
  validation <- unique(panel[c(
    "state_code_1991", "district_code_1991", "validation_status",
    "n_language_cells", "n_mismatched_cells", "absolute_speaker_difference"
  )])
  coverage <- data.frame(
    n_st17_districts = length(unique(paste(panel$state_code_1991, panel$district_code_1991))),
    n_exact_st16_districts = sum(validation$validation_status == "exact"),
    n_st16_mismatch_districts = sum(validation$validation_status == "speaker_counts_mismatch"),
    n_st16_unavailable_districts = sum(validation$validation_status == "st16_unavailable"),
    n_mapped_language_cells = sum(panel$distance_mapping_status == "mapped"),
    n_unresolved_language_cells = sum(panel$distance_mapping_status == "unresolved"),
    n_preferred_language_cells = sum(panel$preferred_st_language_sample),
    stringsAsFactors = FALSE
  )
  structure(
    list(panel = panel, validation = validation, coverage = coverage, estimates = estimates),
    class = "emi_census_1991_st_language"
  )
}

save_census_1991_st_language_diagnostic <- function(
    diagnostic, directory = "outputs/diagnostics/extended/instrument_relevance") {
  if (!inherits(diagnostic, "emi_census_1991_st_language")) {
    stop("Expected an emi_census_1991_st_language diagnostic.", call. = FALSE)
  }
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    panel = file.path(directory, "census_1991_st_language_panel.csv"),
    validation = file.path(directory, "census_1991_st_language_st16_validation.csv"),
    coverage = file.path(directory, "census_1991_st_language_coverage.csv"),
    estimates = file.path(directory, "census_1991_st_language_estimates.csv")
  )
  write_diagnostic_csv(diagnostic$panel, paths[["panel"]])
  write_diagnostic_csv(diagnostic$validation, paths[["validation"]])
  write_diagnostic_csv(diagnostic$coverage, paths[["coverage"]])
  write_diagnostic_csv(diagnostic$estimates, paths[["estimates"]])
  unname(paths)
}

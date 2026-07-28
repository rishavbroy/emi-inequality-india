# Fixed-sample diagnostics for the alternative district-consumption outcomes.

build_consumption_outcome_comparison_formulas <- function() {
  controls <- legacy_2007_iv_controls()
  state_fe <- "factor(state_code_2001)"
  list(
    nominal_log_change = make_iv_formula(
      "log_consumption_difference", "EMIE", "wavg_ling_degrees",
      controls = controls, fixed_effects = state_fe
    ),
    real_log_change_preferred = make_iv_formula(
      "real_log_consumption_change", "EMIE", "wavg_ling_degrees",
      controls = controls, fixed_effects = state_fe
    ),
    real_ancova = make_iv_formula(
      "log_real_consumption_1718", "EMIE", "wavg_ling_degrees",
      controls = c("log_real_consumption_0708", controls), fixed_effects = state_fe
    )
  )
}

consumption_outcome_common_sample <- function(panel, formulas = build_consumption_outcome_comparison_formulas()) {
  data <- as.data.frame(panel)
  required <- unique(unlist(lapply(formulas, all.vars), use.names = FALSE))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "Consumption-outcome comparison is missing variables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  keep <- stats::complete.cases(data[required])
  out <- data[keep, , drop = FALSE]
  rownames(out) <- NULL
  if (!nrow(out)) stop("Consumption-outcome comparison has no complete common sample.", call. = FALSE)
  out
}

consumption_outcome_model_rows <- function(models, common_sample) {
  tidy <- tidy_iv_models(models, common_sample)
  tidy <- tidy[tidy$term == "EMIE", , drop = FALSE]
  model_order <- c("nominal_log_change", "real_log_change_preferred", "real_ancova")
  labels <- c(
    nominal_log_change = "Nominal log change",
    real_log_change_preferred = "Person-weighted real log change",
    real_ancova = "Real endpoint ANCOVA"
  )
  tidy$outcome_specification <- unname(labels[tidy$model])
  tidy$preferred <- tidy$model == "real_log_change_preferred"
  tidy$common_sample_n <- nrow(common_sample)
  tidy$model <- factor(tidy$model, levels = model_order)
  tidy <- tidy[order(tidy$model), , drop = FALSE]
  tidy$model <- as.character(tidy$model)
  rownames(tidy) <- NULL
  tidy
}

summarise_consumption_outcome_sample <- function(common_sample) {
  vars <- c(
    nominal_log_change = "log_consumption_difference",
    real_log_change_preferred = "real_log_consumption_change",
    real_ancova_endpoint = "log_real_consumption_1718",
    real_ancova_baseline = "log_real_consumption_0708"
  )
  safe_bind_rows(lapply(names(vars), function(name) {
    x <- num(common_sample[[vars[[name]]]])
    data.frame(
      measure = name,
      n = sum(is.finite(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

compare_consumption_outcomes <- function(panel, cfg) {
  formulas <- build_consumption_outcome_comparison_formulas()
  common_sample <- consumption_outcome_common_sample(panel, formulas)
  models <- estimate_2sls(common_sample, formulas, cfg)
  list(
    coefficients = consumption_outcome_model_rows(models, common_sample),
    sample_summary = summarise_consumption_outcome_sample(common_sample),
    common_sample = common_sample,
    models = models,
    formulas = formulas
  )
}

summarise_household_price_assignments <- function(households, wave) {
  x <- safe_df(households)
  required <- c(
    ".price_state_code", ".price_sector", ".price_subround",
    "survey_weight_price", "price_deflator", "state_rule",
    "temporal_state_source"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Household price diagnostics are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$assignment_type <- ifelse(x$state_rule == "direct", "direct", "fallback_or_inheritance")
  group <- interaction(
    x$.price_state_code, x$.price_sector, x$.price_subround,
    x$assignment_type, x$temporal_state_source,
    drop = TRUE, sep = "\r"
  )
  total_weight <- sum(num(x$survey_weight_price), na.rm = TRUE)
  out <- safe_bind_rows(lapply(split(seq_len(nrow(x)), group), function(i) {
    w <- num(x$survey_weight_price[i])
    d <- num(x$price_deflator[i])
    data.frame(
      wave = as.integer(wave),
      state_code = x$.price_state_code[i[[1]]],
      sector = x$.price_sector[i[[1]]],
      subround = x$.price_subround[i[[1]]],
      assignment_type = x$assignment_type[i[[1]]],
      temporal_state_source = x$temporal_state_source[i[[1]]],
      households = length(i),
      survey_weight = sum(w, na.rm = TRUE),
      survey_weight_share_pct = 100 * sum(w, na.rm = TRUE) / total_weight,
      deflator_min = min(d, na.rm = TRUE),
      deflator_median = stats::median(d, na.rm = TRUE),
      deflator_max = max(d, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$wave, out$state_code, out$sector, out$subround, out$assignment_type), , drop = FALSE]
}

compare_district_consumption_constructions <- function(panel) {
  x <- as.data.frame(panel)
  variables <- c(
    "consumption_0708", "consumption_0708_household_weighted",
    "real_consumption_0708", "real_consumption_0708_household_weighted",
    "consumption_1718", "consumption_1718_household_weighted",
    "real_consumption_1718", "real_consumption_1718_household_weighted",
    "log_consumption_difference", "real_log_consumption_change"
  )
  missing <- setdiff(variables, names(x))
  if (length(missing)) {
    stop("District consumption comparison is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  safe_bind_rows(lapply(variables, function(variable) {
    value <- num(x[[variable]])
    data.frame(
      variable = variable,
      n = sum(is.finite(value)),
      mean = mean(value, na.rm = TRUE),
      sd = stats::sd(value, na.rm = TRUE),
      min = min(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

save_consumption_price_diagnostics <- function(comparison, households_2007, households_2017, panel) {
  dir <- "outputs/diagnostics/extended/consumption"
  unname(unlist(list(
    outcome_coefficients = write_diagnostic_csv(
      comparison$coefficients,
      file.path(dir, "outcome_fixed_sample_coefficients.csv")
    ),
    outcome_sample = write_diagnostic_csv(
      comparison$sample_summary,
      file.path(dir, "outcome_fixed_sample_summary.csv")
    ),
    price_assignments = write_diagnostic_csv(
      safe_bind_rows(list(
        summarise_household_price_assignments(households_2007, 2007),
        summarise_household_price_assignments(households_2017, 2017)
      )),
      file.path(dir, "household_price_assignments.csv")
    ),
    district_constructions = write_diagnostic_csv(
      compare_district_consumption_constructions(panel),
      file.path(dir, "district_consumption_constructions.csv")
    )
  ), use.names = FALSE))
}

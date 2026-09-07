# Bounded paper-facing synthesis of 2001-2011 household-capacity trajectories.
#
# These changes straddle the 2007-08 schooling measurement. Accordingly, the
# linguistic-distance rows are reduced-form co-movement and the EMI rows are
# descriptive schooling-capacity associations; neither is treated as a mediator
# or a causal treatment effect.

census_household_capacity_trajectory_registry <- function() {
  data.frame(
    outcome_id = c(
      "literacy_depth", "matriculate_access", "graduate_access", "female_graduate_access"
    ),
    variable = c(
      "two_plus_literate_share_households_change_2011_2001",
      "matriculate_access_share_households_age15_plus_change_2011_2001",
      "graduate_access_share_households_age15_plus_change_2011_2001",
      "female_graduate_access_share_households_age15_plus_change_2011_2001"
    ),
    baseline = c(
      "two_plus_literate_share_households_2001",
      "matriculate_access_share_households_age15_plus_2001",
      "graduate_access_share_households_age15_plus_2001",
      "female_graduate_access_share_households_age15_plus_2001"
    ),
    construct_id = c(
      "household_capacity_change__literacy_depth",
      "household_capacity_change__matriculate_access",
      "household_capacity_change__graduate_access",
      "household_capacity_change__female_graduate_access"
    ),
    baseline_construct_id = c(
      "household_capacity_2001__literacy_depth",
      "household_capacity_2001__matriculate_access",
      "household_capacity_2001__graduate_access",
      "household_capacity_2001__female_graduate_access"
    ),
    label = c(
      "Change in households with at least two literate members",
      "Change in age-15+ households with matriculate access",
      "Change in age-15+ households with graduate access",
      "Change in age-15+ households with female graduate access"
    ),
    unit = rep("share_change", 4L),
    denominator = c(
      "households", "households_age15_plus", "households_age15_plus", "households_age15_plus"
    ),
    tier = rep("core", 4L),
    stringsAsFactors = FALSE
  )
}

census_household_capacity_construct_registry <- function() {
  x <- census_household_capacity_trajectory_registry()
  changes <- x[c("construct_id", "variable", "label", "unit", "denominator", "tier")]
  baseline <- data.frame(
    construct_id = x$baseline_construct_id, variable = x$baseline,
    label = paste("Census-2001 baseline:", x$label),
    unit = rep("share", nrow(x)), denominator = x$denominator, tier = x$tier,
    stringsAsFactors = FALSE
  )
  safe_bind_rows(list(changes, baseline))
}

census_household_capacity_predictor_registry <- function() {
  vars <- preferred_iv_variables()
  data.frame(
    predictor_id = c("linguistic_opportunity", "schooling_exposure"),
    predictor = c(vars$instrument, vars$treatment),
    predictor_role = c("instrument", "treatment"),
    scale = c(1, 10),
    estimand = c("coevolving_capacity_reduced_form", "descriptive_schooling_capacity_change"),
    analysis_role = c("reduced_form_context", "descriptive_schooling_capacity_association"),
    stringsAsFactors = FALSE
  )
}

census_household_capacity_specifications <- function() {
  outcomes <- census_household_capacity_trajectory_registry()
  predictors <- census_household_capacity_predictor_registry()
  grid <- merge(
    predictors,
    outcomes[c("outcome_id", "variable", "baseline", "construct_id", "baseline_construct_id")],
    by = NULL, sort = FALSE
  )
  grid$specification_id <- paste(grid$predictor_id, grid$outcome_id, sep = "__")
  grid$analysis_id <- paste("census_household_capacity", grid$specification_id, sep = "__")
  grid <- grid[c(
    "analysis_id", "specification_id", "outcome_id", "variable", "baseline", "construct_id",
    "baseline_construct_id",
    "predictor_id", "predictor", "predictor_role", "scale", "estimand", "analysis_role"
  )]
  if (nrow(grid) != 8L || anyDuplicated(grid$analysis_id)) {
    stop("Census household-capacity synthesis must contain eight unique cells.", call. = FALSE)
  }
  grid
}

prepare_census_household_capacity_panel <- function(
    household_change, district_panel, control_registry = NULL) {
  changes <- safe_df(household_change)
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  specs <- census_household_capacity_specifications()
  controls <- census_2001_main_controls(control_registry)
  required_changes <- unique(c("target_unit_2001", specs$variable, specs$baseline))
  missing_changes <- setdiff(required_changes, names(changes))
  if (length(missing_changes)) {
    stop("Census household-capacity changes are missing columns: ", paste(missing_changes, collapse = ", "), call. = FALSE)
  }
  required_panel <- unique(c("state_code_2001", "district_code_2001", specs$predictor, controls))
  missing_panel <- setdiff(required_panel, names(panel))
  if (length(missing_panel)) {
    stop("District panel is missing household-capacity model columns: ", paste(missing_panel, collapse = ", "), call. = FALSE)
  }
  codes <- lineage_target_codes(changes$target_unit_2001)
  changes$state_code_2001 <- normalize_census_code(codes$state_code_2001, 2L)
  changes$district_code_2001 <- normalize_census_code(codes$district_code_2001, 2L)
  panel$state_code_2001 <- normalize_census_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- normalize_census_code(panel$district_code_2001, 2L)
  keys <- c("state_code_2001", "district_code_2001")
  if (anyDuplicated(changes[keys]) || anyDuplicated(panel[keys])) {
    stop("Household-capacity synthesis requires unique Census-2001 district keys.", call. = FALSE)
  }
  x <- merge(changes[c(keys, specs$variable, specs$baseline)], panel[c(keys, specs$predictor, controls)],
             by = keys, all = FALSE, sort = FALSE)
  needed <- unique(c(keys, specs$variable, specs$baseline, specs$predictor, controls))
  complete <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001)
  x <- x[complete, , drop = FALSE]
  if (!nrow(x)) stop("Household-capacity synthesis has no common complete district sample.", call. = FALSE)
  attr(x, "controls") <- controls
  rownames(x) <- NULL
  x
}

fit_census_household_capacity_specification <- function(panel, specification) {
  x <- safe_df(panel)
  spec <- safe_df(specification)
  if (nrow(spec) != 1L) stop("One household-capacity specification is required.", call. = FALSE)
  controls <- attr(panel, "controls", exact = TRUE)
  if (is.null(controls)) controls <- character()
  outcome <- spec$variable[[1L]]
  baseline <- spec$baseline[[1L]]
  predictor <- spec$predictor[[1L]]
  rhs <- unique(c(predictor, baseline, controls, "factor(state_code_2001)"))
  fit <- stats::lm(stats::reformulate(rhs, response = outcome), data = x)
  inference <- clustered_lm_term_inference(fit, predictor, x$state_code_2001)
  scale <- num(spec$scale[[1L]])
  data.frame(
    analysis_id = spec$analysis_id[[1L]], specification_id = spec$specification_id[[1L]],
    outcome_id = spec$outcome_id[[1L]], outcome = outcome,
    predictor_id = spec$predictor_id[[1L]], predictor = predictor,
    predictor_role = spec$predictor_role[[1L]], estimand = spec$estimand[[1L]],
    n = stats::nobs(fit), n_states = length(unique(x$state_code_2001)),
    estimate = scale * unname(stats::coef(fit)[[predictor]]),
    std_error_state_clustered = scale * inference[["std.error"]],
    p_value_state_clustered = inference[["p.value"]],
    scale = scale, status = "estimated", stringsAsFactors = FALSE
  )
}

diagnose_census_household_capacity <- function(
    household_change, district_panel, control_registry = NULL) {
  specs <- census_household_capacity_specifications()
  panel <- prepare_census_household_capacity_panel(
    household_change, district_panel, control_registry = control_registry
  )
  estimates <- safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    fit_census_household_capacity_specification(panel, specs[i, , drop = FALSE])
  }))
  estimates$p_value_holm_predictor_family <- NA_real_
  for (predictor in unique(estimates$predictor_id)) {
    idx <- which(estimates$predictor_id == predictor)
    estimates$p_value_holm_predictor_family[idx] <- holm_adjust_finite(
      estimates$p_value_state_clustered[idx]
    )
  }
  structure(list(specifications = specs, estimates = estimates), class = "emi_census_household_capacity")
}

save_census_household_capacity <- function(
    diagnostic, dir = "outputs/diagnostics/extended/census_households") {
  if (!inherits(diagnostic, "emi_census_household_capacity")) {
    stop("Expected an emi_census_household_capacity diagnostic.", call. = FALSE)
  }
  write_diagnostic_bundle(
    diagnostic[c("specifications", "estimates")], dir,
    filenames = c(
      specifications = "household_capacity_trajectory_specifications.csv",
      estimates = "household_capacity_trajectory_estimates.csv"
    )
  )
}

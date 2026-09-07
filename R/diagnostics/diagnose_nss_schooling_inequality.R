# NSS-64 social-group schooling-access diagnostics.

nss64_schooling_social_group_margin_registry <- function(
    construct_registry = read_analysis_construct_registry()) {
  outcomes <- c(
    "enrollment_rate_0708",
    "emi_share_enrolled_0708",
    "private_share_enrolled_0708",
    "emi_share_enrolled_public_0708",
    "emi_share_enrolled_private_0708",
    "public_emi_exposure_all_children_0708",
    "public_nonemi_exposure_all_children_0708",
    "private_emi_exposure_all_children_0708",
    "private_nonemi_exposure_all_children_0708"
  )
  constructs <- analysis_construct_rows(construct_registry, outcomes)
  if (any(constructs$domain != "education")) {
    stop("NSS schooling inequality margins must be canonical education constructs.", call. = FALSE)
  }
  data.frame(
    outcome = constructs$variable,
    label = constructs$label,
    model_distance_heterogeneity = c(rep(TRUE, 5L), rep(FALSE, 4L)),
    stringsAsFactors = FALSE
  )
}

nss64_schooling_disadvantaged_groups <- function() {
  setdiff(nss_2007_schooling_social_groups(), "Other")
}

nss64_schooling_social_group_crosscut_registry <- function() {
  data.frame(
    crosscut = c("sex", "sex", "sector", "sector"),
    stratum = c("Male", "Female", "Rural", "Urban"),
    variable = c("SEX", "SEX", "SECTOR", "SECTOR"),
    label = c("Male children", "Female children", "Rural children", "Urban children"),
    stringsAsFactors = FALSE
  )
}

nss64_schooling_social_group_crosscut_outcomes <- function() {
  c(
    "enrollment_rate_0708",
    "emi_share_enrolled_0708",
    "private_share_enrolled_0708",
    "private_emi_exposure_all_children_0708"
  )
}

nss64_schooling_social_group_crosscut_specifications <- function() {
  crosscuts <- nss64_schooling_social_group_crosscut_registry()
  grid <- merge(
    expand.grid(
      social_group = nss64_schooling_disadvantaged_groups(),
      outcome = nss64_schooling_social_group_crosscut_outcomes(),
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    ),
    crosscuts[c("crosscut", "stratum")],
    by = NULL, sort = FALSE
  )
  grid$specification_id <- paste(
    grid$crosscut, grid$stratum, grid$social_group, grid$outcome, sep = "__"
  )
  grid$analysis_id <- paste("nss64_social_group_crosscut", grid$specification_id, sep = "__")
  grid <- grid[c(
    "analysis_id", "specification_id", "crosscut", "stratum",
    "social_group", "outcome"
  )]
  if (nrow(grid) != 48L || anyDuplicated(grid$specification_id)) {
    stop("NSS-64 social-group access cross-cut family must contain 48 unique cells.", call. = FALSE)
  }
  grid
}

nss64_schooling_social_group_specifications <- function() {
  outcomes <- nss64_schooling_social_group_margin_registry()
  outcomes <- outcomes$outcome[outcomes$model_distance_heterogeneity %in% TRUE]
  grid <- expand.grid(
    social_group = nss64_schooling_disadvantaged_groups(),
    outcome = outcomes,
    sample = c("all_states", "hindi_belt"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid$hindi_belt_only <- grid$sample == "hindi_belt"
  grid$specification_id <- paste(
    grid$social_group, grid$outcome, grid$sample, sep = "__"
  )
  grid$analysis_id <- paste("nss64_social_group", grid$specification_id, sep = "__")
  grid <- grid[c(
    "analysis_id", "specification_id", "social_group", "outcome", "sample", "hindi_belt_only"
  )]
  if (nrow(grid) != 30L || anyDuplicated(grid$specification_id)) {
    stop("NSS-64 social-group schooling specification family must contain 30 unique cells.", call. = FALSE)
  }
  grid
}

prepare_nss64_schooling_social_group_panel <- function(
    margins, district_panel, control_registry = NULL) {
  x <- safe_df(margins)
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else {
    safe_df(district_panel)
  }
  registry <- nss64_schooling_social_group_margin_registry()
  controls <- census_2001_main_controls(control_registry)
  required_margin <- c("district_code_0708", "social_group", registry$outcome)
  missing_margin <- setdiff(required_margin, names(x))
  if (length(missing_margin)) {
    stop(
      "NSS-64 social-group schooling margins are missing columns: ",
      paste(missing_margin, collapse = ", "), call. = FALSE
    )
  }
  required_panel <- unique(c(
    "district_code_0708", "state_code_2001", "district_code_2001",
    preferred_iv_variables()$instrument, controls
  ))
  missing_panel <- setdiff(required_panel, names(panel))
  if (length(missing_panel)) {
    stop(
      "District panel is missing NSS-64 social-group diagnostic columns: ",
      paste(missing_panel, collapse = ", "), call. = FALSE
    )
  }

  panel <- panel[required_panel]
  panel$district_code_0708 <- plain_chr(panel$district_code_0708)
  panel <- panel[!is.na(panel$district_code_0708) & nzchar(panel$district_code_0708), , drop = FALSE]
  if (anyDuplicated(panel$district_code_0708)) {
    stop("NSS-64 social-group diagnostic requires unique 2007 district linkage in the analysis panel.", call. = FALSE)
  }
  x$district_code_0708 <- plain_chr(x$district_code_0708)
  out <- merge(x, panel, by = "district_code_0708", all.x = TRUE, sort = FALSE)
  out$hindi_belt_2001 <- plain_chr(out$state_code_2001) %in% shastry_hindi_belt_state_codes()
  out
}

build_nss64_schooling_social_group_gaps <- function(
    panel, covariates = character(), strata = character()) {
  x <- safe_df(panel)
  registry <- nss64_schooling_social_group_margin_registry()
  missing_covariates <- setdiff(covariates, names(x))
  if (length(missing_covariates)) {
    stop(
      "NSS-64 schooling-gap covariates are missing from the prepared panel: ",
      paste(missing_covariates, collapse = ", "), call. = FALSE
    )
  }
  strata <- plain_chr(strata)
  missing_strata <- setdiff(strata, names(x))
  if (length(missing_strata)) {
    stop(
      "NSS-64 schooling-gap strata are missing from the prepared panel: ",
      paste(missing_strata, collapse = ", "), call. = FALSE
    )
  }
  reference <- x[x$social_group == "Other", , drop = FALSE]
  key_columns <- c("district_code_0708", strata)
  reference_key <- interaction(reference[key_columns], drop = TRUE, lex.order = TRUE)
  if (anyDuplicated(reference_key)) {
    stop("NSS-64 reference social group must be unique by district and registered stratum.", call. = FALSE)
  }

  groups <- intersect(
    nss64_schooling_disadvantaged_groups(),
    unique(plain_chr(x$social_group))
  )
  safe_bind_rows(lapply(groups, function(group) {
    group_rows <- x[x$social_group == group, , drop = FALSE]
    group_key <- interaction(group_rows[key_columns], drop = TRUE, lex.order = TRUE)
    ref_i <- match(group_key, reference_key)
    metadata <- data.frame(
      district_code_0708 = group_rows$district_code_0708,
      state_code_2001 = plain_chr(group_rows$state_code_2001),
      district_code_2001 = plain_chr(group_rows$district_code_2001),
      social_group = group,
      reference_group = "Other",
      ling_distance_nonzero_mean = num(group_rows$ling_distance_nonzero_mean),
      hindi_belt_2001 = group_rows$hindi_belt_2001 %in% TRUE,
      stringsAsFactors = FALSE
    )
    for (column in covariates) metadata[[column]] <- group_rows[[column]]
    for (column in strata) metadata[[column]] <- plain_chr(group_rows[[column]])

    safe_bind_rows(lapply(seq_len(nrow(registry)), function(j) {
      outcome <- registry$outcome[[j]]
      out <- metadata
      out$outcome <- outcome
      out$group_value <- num(group_rows[[outcome]])
      out$reference_value <- num(reference[[outcome]][ref_i])
      out$gap_percentage_points <- out$group_value - out$reference_value
      out
    }))
  }))
}

nss64_schooling_social_group_access_summary <- function(gaps, strata = character()) {
  x <- safe_df(gaps)
  if (!nrow(x)) return(data.frame())
  strata <- plain_chr(strata)
  missing <- setdiff(strata, names(x))
  if (length(missing)) stop("NSS-64 access-summary strata are unavailable.", call. = FALSE)
  grouping <- c("social_group", "outcome", strata)
  split_i <- split(
    seq_len(nrow(x)),
    interaction(x[grouping], drop = TRUE, lex.order = TRUE)
  )
  safe_bind_rows(lapply(split_i, function(i) {
    z <- x[i, , drop = FALSE]
    gap <- num(z$gap_percentage_points)
    gap <- gap[is.finite(gap)]
    out <- data.frame(
      social_group = z$social_group[[1L]],
      reference_group = "Other",
      outcome = z$outcome[[1L]],
      n_common_districts = length(gap),
      mean_district_gap_percentage_points = if (length(gap)) mean(gap) else NA_real_,
      median_district_gap_percentage_points = if (length(gap)) stats::median(gap) else NA_real_,
      share_districts_group_below_other = if (length(gap)) mean(gap < 0) else NA_real_,
      stringsAsFactors = FALSE
    )
    for (column in strata) out[[column]] <- plain_chr(z[[column]][[1L]])
    out
  }))
}

build_nss64_schooling_social_group_crosscut_summary <- function(
    margins, district_panel, control_registry = NULL) {
  x <- safe_df(margins)
  required <- c("district_code_0708", "social_group", "crosscut", "stratum")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "NSS-64 schooling cross-cut margins are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  x <- prepare_nss64_schooling_social_group_panel(
    x, district_panel, control_registry = control_registry
  )
  gaps <- build_nss64_schooling_social_group_gaps(
    x, strata = c("crosscut", "stratum")
  )
  gaps <- gaps[gaps$outcome %in% nss64_schooling_social_group_crosscut_outcomes(), , drop = FALSE]
  summary <- nss64_schooling_social_group_access_summary(
    gaps, strata = c("crosscut", "stratum")
  )
  specs <- nss64_schooling_social_group_crosscut_specifications()
  key <- paste(summary$crosscut, summary$stratum, summary$social_group, summary$outcome, sep = "|")
  spec_key <- paste(specs$crosscut, specs$stratum, specs$social_group, specs$outcome, sep = "|")
  idx <- match(key, spec_key)
  if (anyNA(idx) || anyDuplicated(key)) {
    stop("NSS-64 access cross-cut summaries do not match the registered family.", call. = FALSE)
  }
  summary$specification_id <- specs$specification_id[idx]
  summary$analysis_id <- specs$analysis_id[idx]
  summary <- summary[match(spec_key, key), , drop = FALSE]
  if (nrow(summary) != nrow(specs) || anyNA(summary$analysis_id)) {
    stop("NSS-64 access cross-cut family is incomplete on observed district support.", call. = FALSE)
  }
  rownames(summary) <- NULL
  summary
}

fit_nss64_schooling_social_group_gap <- function(
    gaps, social_group, outcome, sample_id = "all_states",
    hindi_belt_only = FALSE, controls = character()) {
  x <- safe_df(gaps)
  x <- x[x$social_group == social_group & x$outcome == outcome, , drop = FALSE]
  if (hindi_belt_only) x <- x[x$hindi_belt_2001 %in% TRUE, , drop = FALSE]
  instrument <- preferred_iv_variables()$instrument
  needed <- unique(c("gap_percentage_points", instrument, "state_code_2001", controls))
  if (!all(needed %in% names(x))) {
    stop("NSS-64 schooling-gap regression lacks required columns.", call. = FALSE)
  }
  for (column in setdiff(needed, "state_code_2001")) x[[column]] <- num(x[[column]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001)
  x <- x[keep, , drop = FALSE]
  if (nrow(x) < 3L || length(unique(x$state_code_2001)) < 2L ||
      !first_stage_positive_variation(x[[instrument]])) {
    return(data.frame(
      sample = sample_id, social_group = social_group, outcome = outcome,
      n_districts = nrow(x), n_states = length(unique(x$state_code_2001)),
      estimate = NA_real_, std_error_state_clustered = NA_real_,
      p_value_state_clustered = NA_real_, stringsAsFactors = FALSE
    ))
  }
  fit <- stats::lm(
    stats::reformulate(c(instrument, controls, "factor(state_code_2001)"), response = "gap_percentage_points"),
    data = x
  )
  inference <- clustered_lm_term_inference(fit, instrument, x$state_code_2001)
  data.frame(
    sample = sample_id,
    social_group = social_group,
    outcome = outcome,
    n_districts = stats::nobs(fit),
    n_states = length(unique(x$state_code_2001)),
    estimate = unname(stats::coef(fit)[[instrument]]),
    std_error_state_clustered = unname(inference[["std.error"]]),
    p_value_state_clustered = unname(inference[["p.value"]]),
    stringsAsFactors = FALSE
  )
}

build_nss64_schooling_social_group_diagnostic <- function(
    margins, district_panel, control_registry = NULL, crosscut_margins = NULL) {
  panel <- prepare_nss64_schooling_social_group_panel(
    margins, district_panel, control_registry = control_registry
  )
  controls <- census_2001_main_controls(control_registry)
  gaps <- build_nss64_schooling_social_group_gaps(panel, covariates = controls)
  specifications <- nss64_schooling_social_group_specifications()
  estimates <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    specification <- specifications[i, , drop = FALSE]
    result <- fit_nss64_schooling_social_group_gap(
      gaps,
      specification$social_group[[1L]],
      specification$outcome[[1L]],
      specification$sample[[1L]],
      specification$hindi_belt_only[[1L]],
      controls
    )
    result$analysis_id <- specification$analysis_id[[1L]]
    result$specification_id <- specification$specification_id[[1L]]
    result
  }))
  if (nrow(estimates) != nrow(specifications) ||
      !setequal(estimates$specification_id, specifications$specification_id)) {
    stop("NSS-64 social-group estimates do not match the canonical specification grid.", call. = FALSE)
  }
  estimates$p_value_holm_family <- holm_adjust_finite(estimates$p_value_state_clustered)

  structure(
    list(
      margins = panel,
      access_summary = nss64_schooling_social_group_access_summary(gaps),
      access_crosscuts = if (is.null(crosscut_margins)) data.frame() else
        build_nss64_schooling_social_group_crosscut_summary(
          crosscut_margins, district_panel, control_registry
        ),
      gaps = gaps,
      specifications = specifications,
      estimates = estimates
    ),
    class = "emi_nss64_schooling_social_group"
  )
}

save_nss64_schooling_social_group_diagnostic <- function(
    diagnostic, directory = "outputs/diagnostics/extended/schooling_access") {
  if (!inherits(diagnostic, "emi_nss64_schooling_social_group")) {
    stop("Expected an emi_nss64_schooling_social_group diagnostic.", call. = FALSE)
  }
  write_diagnostic_bundle(
    diagnostic[c("margins", "access_summary", "access_crosscuts", "gaps", "estimates")],
    directory,
    filenames = c(
      margins = "nss64_social_group_schooling_margins.csv",
      access_summary = "nss64_social_group_access_summary.csv",
      access_crosscuts = "nss64_social_group_access_crosscuts.csv",
      gaps = "nss64_social_group_district_gaps.csv",
      estimates = "nss64_social_group_distance_heterogeneity.csv"
    )
  )
}

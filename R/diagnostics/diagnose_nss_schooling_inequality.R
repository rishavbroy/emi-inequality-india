# NSS-64 social-group schooling-access diagnostics.

nss64_schooling_social_group_margin_registry <- function() {
  data.frame(
    outcome = c(
      "enrollment_rate_0708",
      "emi_share_enrolled_0708",
      "private_share_enrolled_0708",
      "emi_share_enrolled_public_0708",
      "emi_share_enrolled_private_0708",
      "public_emi_exposure_all_children_0708",
      "public_nonemi_exposure_all_children_0708",
      "private_emi_exposure_all_children_0708",
      "private_nonemi_exposure_all_children_0708"
    ),
    label = c(
      "Enrollment",
      "English medium among enrolled children with known medium",
      "Private school among enrolled children with known management",
      "English medium among public-school children with known medium",
      "English medium among private-school children with known medium",
      "Public English-medium exposure among all age-eligible children",
      "Public non-English-medium exposure among all age-eligible children",
      "Private English-medium exposure among all age-eligible children",
      "Private non-English-medium exposure among all age-eligible children"
    ),
    model_distance_heterogeneity = c(rep(TRUE, 5L), rep(FALSE, 4L)),
    stringsAsFactors = FALSE
  )
}

nss64_schooling_disadvantaged_groups <- function() {
  setdiff(nss_2007_schooling_social_groups(), "Other")
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

build_nss64_schooling_social_group_gaps <- function(panel) {
  x <- safe_df(panel)
  registry <- nss64_schooling_social_group_margin_registry()
  reference <- x[x$social_group == "Other", , drop = FALSE]
  if (anyDuplicated(reference$district_code_0708)) {
    stop("NSS-64 reference social group must be unique by district.", call. = FALSE)
  }

  groups <- intersect(
    nss64_schooling_disadvantaged_groups(),
    unique(plain_chr(x$social_group))
  )
  safe_bind_rows(lapply(groups, function(group) {
    group_rows <- x[x$social_group == group, , drop = FALSE]
    ref_i <- match(group_rows$district_code_0708, reference$district_code_0708)
    safe_bind_rows(lapply(seq_len(nrow(registry)), function(j) {
      outcome <- registry$outcome[[j]]
      data.frame(
        district_code_0708 = group_rows$district_code_0708,
        state_code_2001 = plain_chr(group_rows$state_code_2001),
        district_code_2001 = plain_chr(group_rows$district_code_2001),
        social_group = group,
        reference_group = "Other",
        outcome = outcome,
        group_value = num(group_rows[[outcome]]),
        reference_value = num(reference[[outcome]][ref_i]),
        gap_percentage_points = num(group_rows[[outcome]]) - num(reference[[outcome]][ref_i]),
        ling_distance_nonzero_mean = num(group_rows$ling_distance_nonzero_mean),
        hindi_belt_2001 = group_rows$hindi_belt_2001 %in% TRUE,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

nss64_schooling_social_group_access_summary <- function(gaps) {
  x <- safe_df(gaps)
  if (!nrow(x)) return(data.frame())
  split_i <- split(
    seq_len(nrow(x)),
    interaction(x$social_group, x$outcome, drop = TRUE, lex.order = TRUE)
  )
  safe_bind_rows(lapply(split_i, function(i) {
    z <- x[i, , drop = FALSE]
    gap <- num(z$gap_percentage_points)
    gap <- gap[is.finite(gap)]
    data.frame(
      social_group = z$social_group[[1L]],
      reference_group = "Other",
      outcome = z$outcome[[1L]],
      n_common_districts = length(gap),
      mean_district_gap_percentage_points = if (length(gap)) mean(gap) else NA_real_,
      median_district_gap_percentage_points = if (length(gap)) stats::median(gap) else NA_real_,
      share_districts_group_below_other = if (length(gap)) mean(gap < 0) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
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
    margins, district_panel, control_registry = NULL) {
  panel <- prepare_nss64_schooling_social_group_panel(
    margins, district_panel, control_registry = control_registry
  )
  gaps <- build_nss64_schooling_social_group_gaps(panel)
  registry <- nss64_schooling_social_group_margin_registry()
  model_outcomes <- registry$outcome[registry$model_distance_heterogeneity]
  controls <- census_2001_main_controls(control_registry)
  estimates <- safe_bind_rows(lapply(nss64_schooling_disadvantaged_groups(), function(group) {
    safe_bind_rows(lapply(model_outcomes, function(outcome) {
      safe_bind_rows(list(
        fit_nss64_schooling_social_group_gap(
          gaps, group, outcome, "all_states", FALSE, controls
        ),
        fit_nss64_schooling_social_group_gap(
          gaps, group, outcome, "hindi_belt", TRUE, controls
        )
      ))
    }))
  }))
  estimates$p_value_holm_family <- holm_adjust_finite(estimates$p_value_state_clustered)

  structure(
    list(
      margins = panel,
      access_summary = nss64_schooling_social_group_access_summary(gaps),
      gaps = gaps,
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
    diagnostic,
    directory,
    filenames = c(
      margins = "nss64_social_group_schooling_margins.csv",
      access_summary = "nss64_social_group_access_summary.csv",
      gaps = "nss64_social_group_district_gaps.csv",
      estimates = "nss64_social_group_distance_heterogeneity.csv"
    )
  )
}

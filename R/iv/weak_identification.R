# Weak-identification-robust inference shared by IV specifications.

anderson_rubin_test <- function(
  data, outcome, treatment, excluded, included = character(),
  controls = character(), fixed_effect = "none", beta0 = 0
) {
  transformed <- ".ar_outcome"
  data[[transformed]] <- num(data[[outcome]]) - beta0 * num(data[[treatment]])
  rhs <- unique(c(excluded, included, controls, iv_fixed_effect_terms(fixed_effect)))
  fit <- stats::lm(stats::reformulate(rhs, response = transformed), data = data)
  test <- clustered_joint_wald_test(fit, excluded, data$state_code_2001)
  c(statistic = test[["statistic"]], p.value = test[["p.value"]])
}

anderson_rubin_acceptance_components <- function(grid) {
  if (!nrow(grid) || !all(c("beta", "accepted") %in% names(grid))) {
    return(data.frame())
  }

  ord <- order(grid$beta)
  beta <- grid$beta[ord]
  accepted <- as.logical(grid$accepted[ord])
  accepted[is.na(accepted)] <- FALSE
  if (!any(accepted)) return(data.frame())

  run_start <- accepted & c(TRUE, !accepted[-length(accepted)])
  run_id <- cumsum(run_start)
  ids <- unique(run_id[accepted])

  rows <- lapply(seq_along(ids), function(index) {
    positions <- which(accepted & run_id == ids[[index]])
    data.frame(
      component = index,
      lower = min(beta[positions]),
      upper = max(beta[positions]),
      touches_left_grid_edge = min(positions) == 1L,
      touches_right_grid_edge = max(positions) == length(beta),
      contains_zero = min(beta[positions]) <= 0 && max(beta[positions]) >= 0,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

format_anderson_rubin_components <- function(components) {
  if (!nrow(components)) return(NA_character_)
  paste(
    sprintf(
      "%s%.6g, %.6g%s",
      ifelse(components$touches_left_grid_edge, "[grid<= ", "["),
      components$lower,
      components$upper,
      ifelse(components$touches_right_grid_edge, " <=grid]", "]")
    ),
    collapse = " U "
  )
}

anderson_rubin_grid <- function(
  data, outcome, treatment, excluded, included = character(),
  controls = character(), fixed_effect = "none", level = 0.95, points = 401L
) {
  scale <- stats::sd(num(data[[outcome]])) / stats::sd(num(data[[treatment]]))
  if (!is.finite(scale) || scale <= 0) scale <- 1
  beta <- seq(-10 * scale, 10 * scale, length.out = points)
  rows <- safe_bind_rows(lapply(beta, function(value) {
    test <- anderson_rubin_test(
      data, outcome, treatment, excluded, included, controls, fixed_effect, value
    )
    data.frame(
      beta = value, statistic = test[["statistic"]], p.value = test[["p.value"]],
      stringsAsFactors = FALSE
    )
  }))
  rows$accepted <- is.finite(rows$p.value) & rows$p.value >= 1 - level

  components <- anderson_rubin_acceptance_components(rows)
  rows$acceptance_component <- NA_integer_
  if (nrow(components)) {
    for (i in seq_len(nrow(components))) {
      inside <- rows$accepted &
        rows$beta >= components$lower[[i]] &
        rows$beta <= components$upper[[i]]
      rows$acceptance_component[inside] <- components$component[[i]]
    }
  }
  rows
}

estimate_anderson_rubin_spec <- function(data, specification, level = 0.95, points = 401L) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  outcome <- specification$outcome[[1]]
  treatment <- specification$treatment[[1]]
  fixed_effect <- specification$fixed_effect[[1]]
  needed <- iv_specification_variables(specification)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated",
        reason = paste0("Missing columns: ", paste(missing, collapse = ", ")),
        stringsAsFactors = FALSE
      ),
      grid = data.frame()
    ))
  }
  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  if (!nrow(x)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated", reason = "No complete observations.", stringsAsFactors = FALSE
      ),
      grid = data.frame()
    ))
  }
  ar0 <- anderson_rubin_test(
    x, outcome, treatment, excluded, included, controls, fixed_effect, beta0 = 0
  )
  grid <- anderson_rubin_grid(
    x, outcome, treatment, excluded, included, controls, fixed_effect,
    level = level, points = points
  )
  components <- anderson_rubin_acceptance_components(grid)
  grid$specification_id <- specification$specification_id

  bounded_interval <- nrow(components) == 1L &&
    !components$touches_left_grid_edge[[1]] &&
    !components$touches_right_grid_edge[[1]]
  contains_zero <- if (nrow(components)) any(components$contains_zero) else FALSE

  list(
    summary = data.frame(
      specification_id = specification$specification_id,
      anderson_rubin_f_beta0 = ar0[["statistic"]],
      anderson_rubin_p_beta0 = ar0[["p.value"]],
      ar_95_lower = if (bounded_interval) components$lower[[1]] else NA_real_,
      ar_95_upper = if (bounded_interval) components$upper[[1]] else NA_real_,
      ar_95_empty = !nrow(components),
      ar_95_n_components = nrow(components),
      ar_95_disconnected = nrow(components) > 1L,
      ar_95_contains_zero = contains_zero,
      ar_95_grid_accepted_min = if (nrow(components)) min(components$lower) else NA_real_,
      ar_95_grid_accepted_max = if (nrow(components)) max(components$upper) else NA_real_,
      ar_95_left_truncated = nrow(components) && any(components$touches_left_grid_edge),
      ar_95_right_truncated = nrow(components) && any(components$touches_right_grid_edge),
      ar_95_components = format_anderson_rubin_components(components),
      n = nrow(x), status = "estimated", reason = NA_character_,
      stringsAsFactors = FALSE
    ),
    grid = grid
  )
}

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
  accepted <- grid$beta[grid$accepted]
  grid$specification_id <- specification$specification_id
  list(
    summary = data.frame(
      specification_id = specification$specification_id,
      anderson_rubin_f_beta0 = ar0[["statistic"]],
      anderson_rubin_p_beta0 = ar0[["p.value"]],
      ar_95_lower = if (length(accepted)) min(accepted) else NA_real_,
      ar_95_upper = if (length(accepted)) max(accepted) else NA_real_,
      ar_95_empty = !length(accepted),
      ar_95_left_truncated = length(accepted) && min(accepted) == min(grid$beta),
      ar_95_right_truncated = length(accepted) && max(accepted) == max(grid$beta),
      n = nrow(x), status = "estimated", reason = NA_character_,
      stringsAsFactors = FALSE
    ),
    grid = grid
  )
}

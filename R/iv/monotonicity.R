# First-stage shape diagnostics for scalar IV specifications.

monotonicity_bin_summary <- function(z, treatment, bins = 10L) {
  n <- length(z)
  bins <- max(2L, min(as.integer(bins), n))
  group <- ceiling(rank(z, ties.method = "average") * bins / n)
  group[group < 1L] <- 1L
  group[group > bins] <- bins
  out <- aggregate(
    cbind(instrument = z, treatment = treatment),
    list(bin = group),
    mean
  )
  counts <- as.integer(table(factor(group, levels = sort(unique(group)))))
  out$n <- counts[seq_len(nrow(out))]
  out[order(out$instrument), , drop = FALSE]
}

monotonicity_state_slopes <- function(z, treatment, state, minimum_n = 5L) {
  x <- data.frame(
    instrument = z,
    treatment = treatment,
    state_code_2001 = plain_chr(state),
    stringsAsFactors = FALSE
  )
  rows <- lapply(split(x, x$state_code_2001), function(group) {
    if (nrow(group) < minimum_n || stats::sd(group$instrument) <= 0) {
      return(data.frame(
        state_code_2001 = group$state_code_2001[[1]],
        n = nrow(group),
        slope = NA_real_,
        status = "not_estimated",
        stringsAsFactors = FALSE
      ))
    }
    fit <- stats::lm(treatment ~ instrument, data = group)
    data.frame(
      state_code_2001 = group$state_code_2001[[1]],
      n = nrow(group),
      slope = unname(stats::coef(fit)[["instrument"]]),
      status = "estimated",
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

estimate_iv_monotonicity_shape <- function(data, specification, bins = 10L) {
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  if (length(excluded) != 1L) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_applicable",
        reason = "Shape diagnostic is defined only for scalar excluded instruments.",
        stringsAsFactors = FALSE
      ),
      bins = data.frame(),
      state_slopes = data.frame()
    ))
  }

  needed <- iv_specification_variables(specification, include_outcome = FALSE)
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated",
        reason = paste0("Missing columns: ", paste(missing, collapse = ", ")),
        stringsAsFactors = FALSE
      ),
      bins = data.frame(),
      state_slopes = data.frame()
    ))
  }

  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  if (!nrow(x)) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated",
        reason = "No complete observations.",
        stringsAsFactors = FALSE
      ),
      bins = data.frame(),
      state_slopes = data.frame()
    ))
  }

  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  instrument <- excluded[[1]]
  nuisance <- unique(c(included, controls))
  z <- residualize_iv_variable(x, instrument, nuisance, fixed_effect)
  treatment <- residualize_iv_variable(
    x, specification$treatment[[1]], nuisance, fixed_effect
  )
  finite <- is.finite(z) & is.finite(treatment) & nzchar(plain_chr(x$state_code_2001))
  z <- z[finite]
  treatment <- treatment[finite]
  states <- x$state_code_2001[finite]

  if (length(z) < 3L || stats::sd(z) <= 0 || stats::sd(treatment) <= 0) {
    return(list(
      summary = data.frame(
        specification_id = specification$specification_id,
        status = "not_estimated",
        reason = "Residualized first-stage variables have insufficient variation.",
        stringsAsFactors = FALSE
      ),
      bins = data.frame(),
      state_slopes = data.frame()
    ))
  }

  linear_fit <- stats::lm(treatment ~ z)
  isotonic <- stats::isoreg(z, treatment)
  isotonic_order <- isotonic$ord %||% seq_along(z)
  sst <- sum((treatment - mean(treatment))^2)
  isotonic_r_squared <- if (sst > 0) {
    max(0, 1 - sum((treatment[isotonic_order] - isotonic$yf)^2) / sst)
  } else {
    NA_real_
  }

  bins_out <- monotonicity_bin_summary(z, treatment, bins = bins)
  changes <- diff(bins_out$treatment)
  state_slopes <- monotonicity_state_slopes(z, treatment, states)
  valid_state_slopes <- state_slopes$slope[state_slopes$status == "estimated" & is.finite(state_slopes$slope)]

  bins_out$specification_id <- specification$specification_id
  state_slopes$specification_id <- specification$specification_id

  list(
    summary = data.frame(
      specification_id = specification$specification_id,
      adjustment_id = specification$adjustment_id,
      construction_id = specification$construction_id,
      instrument = instrument,
      fixed_effect = fixed_effect,
      linear_slope = unname(stats::coef(linear_fit)[["z"]]),
      spearman_rho = suppressWarnings(stats::cor(z, treatment, method = "spearman")),
      isotonic_r_squared = isotonic_r_squared,
      n_bins = nrow(bins_out),
      share_nondecreasing_bin_steps = if (length(changes)) mean(changes >= 0) else NA_real_,
      n_negative_bin_steps = sum(changes < 0),
      n_state_slopes = length(valid_state_slopes),
      share_negative_state_slopes = if (length(valid_state_slopes)) mean(valid_state_slopes < 0) else NA_real_,
      n = length(z),
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    ),
    bins = bins_out,
    state_slopes = state_slopes
  )
}

run_iv_monotonicity_diagnostics <- function(
  panel,
  specifications = iv_diagnostic_specification_registry(),
  bins = 10L
) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  applicable <- iv_diagnostic_applicability(specifications)
  ids <- applicable$specification_id[
    applicable$diagnostic_id == "monotonicity_shape" & applicable$will_run
  ]
  specs <- specifications[specifications$specification_id %in% ids, , drop = FALSE]
  results <- lapply(seq_len(nrow(specs)), function(i) {
    estimate_iv_monotonicity_shape(data, specs[i, , drop = FALSE], bins = bins)
  })
  list(
    summary = safe_bind_rows(lapply(results, `[[`, "summary")),
    bins = safe_bind_rows(lapply(results, `[[`, "bins")),
    state_slopes = safe_bind_rows(lapply(results, `[[`, "state_slopes"))
  )
}

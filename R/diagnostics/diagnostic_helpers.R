# Shared helpers for optional diagnostics and benchmarking outputs.

write_diagnostic_csv <- function(x, path, row.names = FALSE, na = "NA") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  utils::write.csv(x, path, row.names = row.names, na = na)
  normalizePath(path, mustWork = FALSE)
}

write_diagnostic_bundle <- function(objects, directory, filenames = NULL, stale = character()) {
  if (!is.list(objects) || is.null(names(objects)) || any(!nzchar(names(objects)))) {
    stop("Diagnostic bundles must be named lists.", call. = FALSE)
  }
  if (anyDuplicated(names(objects))) {
    stop("Diagnostic bundle object names must be unique.", call. = FALSE)
  }
  if (is.null(filenames)) filenames <- paste0(names(objects), ".csv")
  if (is.null(names(filenames))) names(filenames) <- names(objects)
  missing <- setdiff(names(objects), names(filenames))
  if (length(missing)) {
    stop(
      "Diagnostic bundle filenames are missing objects: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  filenames <- filenames[names(objects)]
  if (any(is.na(filenames)) || any(!nzchar(filenames)) || anyDuplicated(filenames)) {
    stop("Diagnostic bundle filenames must be nonempty and unique.", call. = FALSE)
  }
  if (length(stale)) unlink(stale[file.exists(stale)])
  paths <- file.path(directory, unname(filenames))
  names(paths) <- names(objects)
  for (name in names(objects)) {
    write_diagnostic_csv(objects[[name]], paths[[name]], na = "")
  }
  unname(paths)
}

collapse_diagnostic_list_columns <- function(x, columns, sep = ";", empty = "none") {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  for (column in intersect(columns, names(out))) {
    if (is.list(out[[column]])) {
      out[[column]] <- vapply(out[[column]], function(value) {
        value <- unlist(value, use.names = FALSE)
        if (!length(value)) return(empty)
        paste(value, collapse = sep)
      }, FUN.VALUE = character(1))
    }
  }
  out
}

write_diagnostic_matrix <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.matrix(x), path, row.names = TRUE)
  normalizePath(path, mustWork = FALSE)
}

output_manifest <- function(paths, description = names(paths)) {
  paths <- unlist(paths, use.names = FALSE)
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(data.frame(path = character(), description = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    path = paths,
    description = rep_len(description %||% basename(paths), length(paths)),
    stringsAsFactors = FALSE
  )
}

present_cols <- function(df, cols) {
  intersect(cols, names(as.data.frame(df)))
}

numeric_like <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

holm_adjust_finite <- function(p) {
  x <- num(p)
  out <- rep(NA_real_, length(x))
  keep <- is.finite(x)
  if (any(keep)) out[keep] <- stats::p.adjust(x[keep], method = "holm")
  out
}

fit_standardized_modifier_interaction <- function(
    sample, outcome, predictor, modifier, controls = character(),
    fixed_effect = "none", cluster, predictor_scale = 1) {
  x <- safe_df(sample)
  needed <- unique(c(outcome, predictor, modifier))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "Interaction sample is missing fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  cluster <- plain_chr(cluster)
  if (length(cluster) != nrow(x) || any(is.na(cluster) | !nzchar(cluster))) {
    stop("Interaction fit requires one nonempty cluster identifier per row.", call. = FALSE)
  }
  predictor_scale <- num(predictor_scale)[[1L]]
  if (!is.finite(predictor_scale) || predictor_scale <= 0) {
    stop("Interaction predictor scale must be positive and finite.", call. = FALSE)
  }

  modifier_values <- num(x[[modifier]])
  modifier_mean <- mean(modifier_values)
  modifier_sd <- stats::sd(modifier_values)
  if (!is.finite(modifier_sd) || modifier_sd <= 0) {
    stop("Interaction modifier must vary on the analysis sample.", call. = FALSE)
  }
  x$.modifier_z <- (modifier_values - modifier_mean) / modifier_sd

  controls <- setdiff(unique(plain_chr(controls)), modifier)
  interaction_term <- paste0(predictor, ":.modifier_z")
  rhs <- c(
    paste0(predictor, " * .modifier_z"), controls,
    iv_fixed_effect_terms(fixed_effect)
  )
  fit <- stats::lm(stats::reformulate(rhs, response = outcome), data = x)
  predictor_inference <- clustered_lm_term_inference(fit, predictor, cluster)
  interaction_inference <- clustered_lm_term_inference(fit, interaction_term, cluster)
  coefficients <- stats::coef(fit)

  data.frame(
    n = stats::nobs(fit),
    n_clusters = length(unique(cluster)),
    modifier_mean = modifier_mean,
    modifier_sd = modifier_sd,
    predictor_slope_at_mean_modifier =
      predictor_scale * unname(coefficients[[predictor]]),
    predictor_slope_std_error_clustered =
      predictor_scale * unname(predictor_inference[["std.error"]]),
    predictor_slope_p_value_clustered =
      unname(predictor_inference[["p.value"]]),
    interaction_per_predictor_scale_per_modifier_sd =
      predictor_scale * unname(coefficients[[interaction_term]]),
    interaction_std_error_clustered =
      predictor_scale * unname(interaction_inference[["std.error"]]),
    interaction_p_value_clustered =
      unname(interaction_inference[["p.value"]]),
    status = "estimated",
    stringsAsFactors = FALSE
  )
}

safe_pairwise_cor <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || ncol(df) < 2L) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  keep <- vapply(df, function(x) {
    y <- suppressWarnings(as.numeric(as.character(x)))
    sum(is.finite(y)) > 1L && stats::sd(y, na.rm = TRUE) > 0
  }, logical(1))
  if (sum(keep) < 2L) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  num_df <- as.data.frame(lapply(df[keep], numeric_like), check.names = FALSE)
  stats::cor(num_df, use = "pairwise.complete.obs")
}

elapsed_seconds <- function(expr) {
  unname(system.time(force(expr))[["elapsed"]])
}

diagnostic_status_table <- function(diagnostic, status, reason = NA_character_) {
  data.frame(
    diagnostic = diagnostic,
    status = status,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

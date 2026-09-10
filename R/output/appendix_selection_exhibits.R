# Final-paper Appendix E exhibit builders.
#
# These functions consolidate already-estimated selection and missingness evidence.
# They do not fit new models: the appendix layer owns presentation while the
# selection modules continue to own estimation and diagnostic construction.

appendix_selection_sample_summary <- function(selection_data) {
  numeric <- attr(
    make_selection_summary_numeric_table(selection_data),
    "csv_data", exact = TRUE
  ) %||% data.frame()
  categorical <- attr(
    make_selection_summary_categorical_table(selection_data),
    "csv_data", exact = TRUE
  ) %||% data.frame()

  numeric <- safe_df(numeric)
  categorical <- safe_df(categorical)
  numeric_rows <- if (nrow(numeric)) {
    data.frame(
      variable_id = plain_chr(numeric$var),
      variable = plain_chr(numeric$label),
      type = "Numeric",
      n = as.integer(numeric$N),
      summary = sprintf(
        "Mean %.2f; SD %.2f; range %.2f to %.2f",
        num(numeric$Mean), num(numeric$SD), num(numeric$Min), num(numeric$Max)
      ),
      stringsAsFactors = FALSE
    )
  } else data.frame()
  categorical_rows <- if (nrow(categorical)) {
    data.frame(
      variable_id = plain_chr(categorical$var),
      variable = plain_chr(categorical$label),
      type = "Categorical",
      n = as.integer(categorical$N),
      summary = paste0(
        "Mode ", plain_chr(categorical$Mode), " (",
        sprintf("%.1f", num(categorical$`% Mode`)), "%); levels: ",
        plain_chr(categorical$Values)
      ),
      stringsAsFactors = FALSE
    )
  } else data.frame()

  csv <- safe_bind_rows(list(numeric_rows, categorical_rows))
  if (!nrow(csv) || any(!is.finite(csv$n))) {
    stop("Appendix E sample summary requires nonempty finite selection summaries.", call. = FALSE)
  }
  out <- data.frame(
    Variable = csv$variable,
    Type = csv$type,
    N = formatC(csv$n, format = "d", big.mark = ","),
    Summary = csv$summary,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_missingness_diagnostics_table <- function(missingness) {
  if (!inherits(missingness, "emi_missingness_diagnostics")) {
    stop("Appendix E missingness exhibit requires canonical missingness diagnostics.", call. = FALSE)
  }
  counts <- safe_df(missingness$missing_counts)
  logits <- safe_df(missingness$logit_summary)
  required <- c("missing_var", "n_missing", "pct_missing")
  if (length(setdiff(required, names(counts)))) {
    stop("Appendix E missingness counts are missing required columns.", call. = FALSE)
  }
  counts <- counts[!grepl("^Total probit-model", plain_chr(counts$missing_var)), , drop = FALSE]
  if (!nrow(counts)) stop("Appendix E missingness exhibit has no variable rows.", call. = FALSE)

  pseudo <- rep(NA_real_, nrow(counts))
  n_sig <- rep(NA_integer_, nrow(counts))
  if (nrow(logits) && all(c("missing_var", "pseudoR2", "n_sig") %in% names(logits))) {
    idx <- match(plain_chr(counts$missing_var), plain_chr(logits$missing_var))
    hit <- !is.na(idx)
    pseudo[hit] <- num(logits$pseudoR2[idx[hit]])
    n_sig[hit] <- as.integer(logits$n_sig[idx[hit]])
  }

  csv <- data.frame(
    variable = plain_chr(counts$missing_var),
    n_missing = as.integer(counts$n_missing),
    pct_missing = num(counts$pct_missing),
    pseudo_r_squared = pseudo,
    n_significant_predictors = n_sig,
    stringsAsFactors = FALSE
  )
  if (any(!is.finite(csv$n_missing)) || any(!is.finite(csv$pct_missing))) {
    stop("Appendix E missingness exhibit contains non-finite counts or rates.", call. = FALSE)
  }
  fmt_pseudo <- ifelse(
    is.finite(csv$pseudo_r_squared),
    sprintf("%.3f", csv$pseudo_r_squared), ""
  )
  fmt_sig <- ifelse(
    is.finite(csv$n_significant_predictors),
    as.character(csv$n_significant_predictors), ""
  )
  out <- data.frame(
    Variable = csv$variable,
    `Missing N` = formatC(csv$n_missing, format = "d", big.mark = ","),
    `Missing %` = sprintf("%.1f", 100 * csv$pct_missing),
    `Pseudo R2` = fmt_pseudo,
    `Significant predictors` = fmt_sig,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_selection_exhibits <- function(selection_data, missingness) {
  list(
    appendix_e1_selection_sample = appendix_selection_sample_summary(selection_data),
    appendix_e4_missingness = appendix_missingness_diagnostics_table(missingness),
    missingness_plot_data = safe_df(missingness$logit_summary)
  )
}

save_appendix_selection_exhibits <- function(exhibits, cfg) {
  if (!is.list(exhibits) ||
      !all(c("appendix_e1_selection_sample", "appendix_e4_missingness", "missingness_plot_data") %in% names(exhibits))) {
    stop("Appendix E exhibit bundle is incomplete.", call. = FALSE)
  }
  written <- save_appendix_tables(
    exhibits, c("appendix_e1_selection_sample", "appendix_e4_missingness"), cfg
  )

  figure_path <- paste0(
    appendix_figure_path_base("appendix_e5_missingness_predictability"), ".png"
  )
  written <- c(
    written,
    save_missingness_logit_plot(
      exhibits$missingness_plot_data,
      figure_path,
      title = "Predictability of missingness in the enrollment sample"
    )
  )
  unique(normalizePath(written, mustWork = FALSE))
}

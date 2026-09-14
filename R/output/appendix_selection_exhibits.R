# Final-paper education-selection appendix tables.
#
# The selection domain owns estimation and missingness diagnostics. This module
# only selects reader-facing summaries from those already-estimated results.

appendix_selection_display_terms <- function() {
  lookup <- ame_label_lookup()
  district_context <- c(
    "dmean_num_IS_EDU_FREE", "dmean_num_TUTION_FEE_WAIVED",
    "dmean_num_RECD_SCHOLARSHIP_STIPEND", "dmean_num_RECD_TXT_BOOKS",
    "dmean_num_RECD_STATIONERY", "dmean_num_MID_DAY_MEAL_ETC_RECD",
    "dmean_num_ENROLLMENT_COST"
  )
  unique(plain_chr(lookup$Term[!lookup$term %in% district_context]))
}

appendix_selection_ame_table <- function(ame_results, selection_model) {
  n <- selection_model_observations(selection_model)
  table <- make_probit_ame_table(
    ame_results,
    n = n,
    selection_model = selection_model
  )
  keep <- appendix_selection_display_terms()
  display <- table[plain_chr(table$Term) %in% keep, , drop = FALSE]
  if (!nrow(display)) {
    stop("Education-selection appendix requires nonempty AME rows.", call. = FALSE)
  }

  for (nm in c("marginaleffects_object", "marginaleffects_n", "selection_model")) {
    attr(display, nm) <- attr(table, nm, exact = TRUE)
  }
  attr(display, "ame_keep_terms") <- keep

  csv <- safe_df(attr(table, "csv_data", exact = TRUE))
  if (!nrow(csv) || !"Term" %in% names(csv)) {
    stop("Education-selection appendix requires labeled AME data.", call. = FALSE)
  }
  csv <- csv[plain_chr(csv$Term) %in% keep, , drop = FALSE]
  attr(display, "csv_data") <- csv
  display
}

appendix_selection_missingness_table <- function(missingness) {
  if (!inherits(missingness, "emi_missingness_diagnostics")) {
    stop("Education-selection missingness summary requires registered missingness diagnostics.", call. = FALSE)
  }
  counts <- safe_df(missingness$missing_counts)
  logits <- safe_df(missingness$logit_summary)
  required <- c("missing_var", "n_missing", "pct_missing")
  if (length(setdiff(required, names(counts)))) {
    stop("Education-selection missingness counts are missing required columns.", call. = FALSE)
  }

  keep_ids <- c(
    "DIST_FROM_NEAREST_PRIMARY_CLASS",
    "dmean_num_ENROLLMENT_COST",
    "father_educ",
    "Total probit-model with NA"
  )
  counts <- counts[match(keep_ids, plain_chr(counts$missing_var)), , drop = FALSE]
  if (nrow(counts) != length(keep_ids) || any(is.na(counts$missing_var))) {
    stop("Education-selection missingness summary requires all registered model-missingness rows.", call. = FALSE)
  }

  labels <- c(
    DIST_FROM_NEAREST_PRIMARY_CLASS = "Distance to nearest primary class",
    dmean_num_ENROLLMENT_COST = "District-average enrollment cost",
    father_educ = "Father's education",
    `Total probit-model with NA` = "Any model covariate"
  )
  pseudo <- rep(NA_real_, nrow(counts))
  if (nrow(logits) && all(c("missing_var", "pseudoR2") %in% names(logits))) {
    idx <- match(plain_chr(counts$missing_var), plain_chr(logits$missing_var))
    hit <- !is.na(idx)
    pseudo[hit] <- num(logits$pseudoR2[idx[hit]])
  }

  csv <- data.frame(
    variable_id = plain_chr(counts$missing_var),
    variable = unname(labels[plain_chr(counts$missing_var)]),
    n_missing = as.integer(counts$n_missing),
    pct_missing = num(counts$pct_missing),
    pseudo_r_squared = pseudo,
    stringsAsFactors = FALSE
  )
  if (any(!is.finite(csv$n_missing)) || any(!is.finite(csv$pct_missing))) {
    stop("Education-selection missingness summary contains non-finite counts or rates.", call. = FALSE)
  }

  out <- data.frame(
    Variable = csv$variable,
    `Missing N` = formatC(csv$n_missing, format = "d", big.mark = ","),
    `Missing %` = sprintf("%.1f", 100 * csv$pct_missing),
    `Missingness pseudo-R2` = ifelse(
      is.finite(csv$pseudo_r_squared),
      sprintf("%.3f", csv$pseudo_r_squared),
      ""
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_selection_exhibits <- function(ame_results, selection_model, missingness) {
  list(
    appendix_selection_ame = appendix_selection_ame_table(ame_results, selection_model),
    appendix_selection_missingness = appendix_selection_missingness_table(missingness)
  )
}

save_appendix_selection_exhibits <- function(exhibits, cfg) {
  required <- c("appendix_selection_ame", "appendix_selection_missingness")
  if (!is.list(exhibits) || !all(required %in% names(exhibits))) {
    stop("Education-selection appendix table bundle is incomplete.", call. = FALSE)
  }
  save_appendix_tables(exhibits, required, cfg)
}

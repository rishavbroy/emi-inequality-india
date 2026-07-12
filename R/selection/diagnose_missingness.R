# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-missingness-logit-parallel

legacy_missingness_variables <- function(selection_data) {
  df <- as.data.frame(selection_data, stringsAsFactors = FALSE)
  all_child_missing_vars <- c("DIST_FROM_NEAREST_PRIMARY_CLASS", "dmean_num_ENROLLMENT_COST", "father_educ")
  enrolled_only_missing_vars <- c("TUTION_FEE", "EXAMINATION_FEE", "OTHER_FEES_PAYMENTS", "BOOKS", "STATIONERY", "UNIFORM", "TRANSPORT")

  # Legacy Chunk 8 distinguished variables used in the probit from
  # enrolled-only expenditure variables.  Keep that distinction so the total
  # row labelled "probit-model" is not inflated by fee variables that are
  # undefined for non-enrolled children.
  probit_vars <- c(
    "enrolled", "ENROLLED", "AGE", "age", "SEX", "HH_SIZE", "RELIGION", "SOCIAL_GROUP",
    "SECTOR", "state_0708", "region_0708", all_child_missing_vars
  )
  probit_vars <- intersect(probit_vars, names(df))
  if (!length(probit_vars)) probit_vars <- setdiff(names(df), intersect(enrolled_only_missing_vars, names(df)))

  list(
    probit_vars = probit_vars,
    miss_vars_all = intersect(all_child_missing_vars, names(df)),
    miss_vars_enrolled = intersect(enrolled_only_missing_vars, names(df)),
    group_vars = intersect(c("SECTOR", "SEX", "RELIGION", "SOCIAL_GROUP", "state_0708"), names(df)),
    cts_vars = intersect(c("AGE", "HH_SIZE"), names(df)),
    regional_vars = intersect(c("state_0708", "region_0708"), names(df))
  )
}

#' diagnose missingness
#'
#' Port the legacy Chunk 8 missingness diagnostics into an opt-in diagnostic
#' object.  The returned list preserves the legacy sequence: variable counts,
#' regional rankings, missingness correlation matrices, BH-adjusted logit screens,
#' and notes for commented case-study / chi-square checks.
diagnose_missingness <- function(selection_data, cfg) {
  df <- as.data.frame(selection_data, stringsAsFactors = FALSE)
  vars <- legacy_missingness_variables(df)
  probit_vars <- vars$probit_vars

  counts <- summarize_missingness_by_variable(df[probit_vars])
  if (length(probit_vars)) {
    total_na <- sum(!stats::complete.cases(df[probit_vars]))
    total_complete <- sum(stats::complete.cases(df[probit_vars]))
  } else {
    total_na <- 0L
    total_complete <- nrow(df)
  }
  counts <- safe_bind_rows(list(
    counts,
    data.frame(missing_var = "Total probit-model with NA", n_missing = total_na, pct_missing = if (nrow(df)) total_na / nrow(df) else NA_real_),
    data.frame(missing_var = "Total probit-model with no NA", n_missing = total_complete, pct_missing = if (nrow(df)) total_complete / nrow(df) else NA_real_)
  ))

  regional <- summarize_missingness_regions(df, probit_vars, vars)
  corr_all <- missingness_correlation_matrix(df, vars$miss_vars_all, vars$group_vars, vars$cts_vars)
  enrolled_rows <- legacy_enrolled_rows(df)
  corr_enrolled <- missingness_correlation_matrix(df[enrolled_rows, , drop = FALSE], vars$miss_vars_enrolled, vars$group_vars, vars$cts_vars)

  covars <- intersect(c("SECTOR", "SEX", "AGE", "HH_SIZE", "RELIGION", "SOCIAL_GROUP", "state_0708"), names(df))
  miss_all <- intersect(vars$miss_vars_all, names(df))
  miss_enrolled <- intersect(vars$miss_vars_enrolled, names(df))
  logit_all <- if (length(miss_all) && length(covars)) run_missingness_logits(df, miss_all, covars) else data.frame()
  logit_enrolled <- if (length(miss_enrolled) && length(covars) && any(enrolled_rows)) run_missingness_logits(df[enrolled_rows, , drop = FALSE], miss_enrolled, covars) else data.frame()
  logit_summary <- summarize_missingness_logits(safe_bind_rows(list(logit_all, logit_enrolled)))
  case_study <- summarize_missingness_case_study(df, vars)
  chi_square <- summarize_missingness_chisq(df, probit_vars)

  notes <- data.frame(
    diagnostic = c(
      "rajasthan_southern_case_study",
      "chi_square_any_na_by_state",
      "chi_square_any_na_by_region",
      "parallel_missingness_logit"
    ),
    legacy_status = c(
      "commented diagnostic View() code preserved as documented note",
      "commented diagnostic test not run by default",
      "commented diagnostic test not run by default",
      "ported using lapply for reproducible targets execution; legacy mclapply/parLapply choice documented"
    ),
    stringsAsFactors = FALSE
  )

  out <- list(
    missing_counts = counts,
    regional_cost = regional$cost,
    regional_distance = regional$distance,
    regional_father_education = regional$father_education,
    corr_all = corr_all,
    corr_enrolled = corr_enrolled,
    logit_all = logit_all,
    logit_enrolled = logit_enrolled,
    logit_summary = logit_summary,
    case_study = case_study,
    chi_square = chi_square,
    notes = notes
  )
  class(out) <- c("emi_missingness_diagnostics", class(out))
  out
}

legacy_enrolled_rows <- function(df) {
  if (!"enrolled" %in% names(df)) return(rep(TRUE, nrow(df)))
  value <- tolower(as.character(df$enrolled))
  value %in% c("yes", "1", "true", "enrolled")
}

summarize_missingness_by_variable <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!length(names(df))) {
    return(data.frame(missing_var = character(), n_missing = integer(), pct_missing = numeric(), stringsAsFactors = FALSE))
  }
  data.frame(
    missing_var = names(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = if (nrow(df)) vapply(df, function(x) mean(is.na(x)), numeric(1)) else NA_real_,
    stringsAsFactors = FALSE
  )
}


summarize_missingness_case_study <- function(df, vars) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df)) return(data.frame())
  state_col <- intersect(c("state_0708", "state"), names(df))
  region_col <- intersect(c("region_0708", "region"), names(df))
  if (!length(state_col)) return(data.frame(note = "No state column available for Rajasthan/Southern case study.", stringsAsFactors = FALSE))

  keep <- grepl("rajasthan", as.character(df[[state_col[[1]]]]), ignore.case = TRUE)
  case_scope <- "Rajasthan"
  if (length(region_col)) {
    region_keep <- grepl("southern", as.character(df[[region_col[[1]]]]), ignore.case = TRUE)
    if (any(keep & region_keep, na.rm = TRUE)) {
      keep <- keep & region_keep
      case_scope <- "Rajasthan / Southern"
    }
  }
  vars_to_check <- intersect(c(vars$miss_vars_all, vars$miss_vars_enrolled), names(df))
  if (!length(vars_to_check) || !any(keep, na.rm = TRUE)) {
    return(data.frame(case_scope = case_scope, n_rows = sum(keep, na.rm = TRUE), note = "No matching rows or missingness variables available.", stringsAsFactors = FALSE))
  }
  case_df <- df[keep, vars_to_check, drop = FALSE]
  any_missing <- !stats::complete.cases(case_df)
  data.frame(
    case_scope = case_scope,
    n_rows = nrow(case_df),
    n_rows_with_any_missing = sum(any_missing),
    n_rows_with_partial_missing = sum(any_missing & rowSums(is.na(case_df)) < ncol(case_df)),
    n_missing_cells = sum(is.na(case_df)),
    n_observed_cells_in_rows_with_missing = sum(!is.na(case_df[any_missing, , drop = FALSE])),
    interpretation = "Current analog of the legacy Rajasthan/Southern View() check: rows with partial cost-variable missingness preserve the legacy concern that a child was not necessarily excluded wholesale when one cost field was missing.",
    stringsAsFactors = FALSE
  )
}

summarize_missingness_chisq <- function(df, probit_vars) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || !length(probit_vars)) return(data.frame())
  probit_vars <- intersect(probit_vars, names(df))
  if (!length(probit_vars)) return(data.frame())
  any_na <- !stats::complete.cases(df[probit_vars])
  safe_test <- function(group_col, label) {
    if (!group_col %in% names(df)) {
      return(data.frame(test = label, status = "not_available", reason = paste("Missing column", group_col), stringsAsFactors = FALSE))
    }
    group <- as.character(df[[group_col]])
    keep <- !is.na(group) & nzchar(group)
    if (length(unique(group[keep])) < 2L || length(unique(any_na[keep])) < 2L) {
      return(data.frame(test = label, status = "not_estimated", reason = "Insufficient variation for chi-square test.", stringsAsFactors = FALSE))
    }
    tab <- table(any_na = any_na[keep], group = group[keep])
    fit <- suppressWarnings(stats::chisq.test(tab))
    data.frame(
      test = label,
      status = "estimated",
      statistic = unname(fit$statistic),
      parameter = unname(fit$parameter),
      p.value = unname(fit$p.value),
      n = sum(tab),
      method = fit$method,
      stringsAsFactors = FALSE
    )
  }
  safe_bind_rows(list(
    safe_test("state_0708", "any_probit_model_na_by_state"),
    safe_test("region_0708", "any_probit_model_na_by_region")
  ))
}

summarize_missingness_regions <- function(df, probit_vars, vars, top_n = 20L) {
  empty <- data.frame()
  if (!"state_0708" %in% names(df)) {
    return(list(cost = empty, distance = empty, father_education = empty))
  }
  if (!length(probit_vars)) probit_vars <- names(df)
  temp <- df
  region_cols <- intersect(c("state_0708", "region_0708"), names(temp))
  # Legacy Chunk 8 inspected both state and region.  Some cleaned selection
  # inputs no longer expose region_0708, so keep the diagnostic alive at the
  # state level instead of writing empty CSVs.
  if (!"region_0708" %in% region_cols) {
    temp$region_0708 <- "all_regions_available_in_state_only_input"
    region_cols <- c("state_0708", "region_0708")
  }

  temp$any_na_row <- !stats::complete.cases(temp[intersect(probit_vars, names(temp))])
  for (nm in c("dmean_num_ENROLLMENT_COST", "DIST_FROM_NEAREST_PRIMARY_CLASS", "father_educ")) {
    temp[[paste0("miss_", nm)]] <- if (nm %in% names(temp)) as.integer(is.na(temp[[nm]])) else NA_integer_
  }
  temp$is_urban <- if ("SECTOR" %in% names(temp)) as.integer(temp$SECTOR == "Urban") else NA_integer_
  temp$is_female <- if ("SEX" %in% names(temp)) as.integer(temp$SEX == "Female") else NA_integer_
  temp$is_hindu <- if ("RELIGION" %in% names(temp)) as.integer(temp$RELIGION == "Hindu") else NA_integer_
  temp$is_muslim <- if ("RELIGION" %in% names(temp)) as.integer(temp$RELIGION == "Muslim") else NA_integer_
  temp$is_st_sc_obc <- if ("SOCIAL_GROUP" %in% names(temp)) as.integer(temp$SOCIAL_GROUP %in% c("Scheduled Tribe", "Scheduled Caste", "Other Backward Class")) else NA_integer_

  measure_cols <- c("any_na_row", "miss_dmean_num_ENROLLMENT_COST", "miss_DIST_FROM_NEAREST_PRIMARY_CLASS", "miss_father_educ", "is_urban", "is_female", "is_hindu", "is_muslim", "is_st_sc_obc")
  grouped <- stats::aggregate(
    temp[measure_cols],
    temp[region_cols],
    function(x) mean(as.numeric(x), na.rm = TRUE)
  )
  n <- stats::aggregate(temp$any_na_row, temp[region_cols], length)
  names(n)[names(n) == "x"] <- "n"
  out <- merge(n, grouped, by = region_cols, all = TRUE)
  names(out) <- sub("^any_na_row$", "pct_any_na", names(out))
  out$region_diagnostic_level <- if ("region_0708" %in% names(df)) "state_region" else "state_only_fallback"
  pct_cols <- setdiff(names(out), c(region_cols, "n", "region_diagnostic_level"))
  out[pct_cols] <- lapply(out[pct_cols], function(x) round(100 * x, 2))
  rank_one <- function(col) {
    if (!col %in% names(out)) return(empty)
    out[order(-out[[col]]), , drop = FALSE][seq_len(min(top_n, nrow(out))), , drop = FALSE]
  }
  list(
    cost = rank_one("miss_dmean_num_ENROLLMENT_COST"),
    distance = rank_one("miss_DIST_FROM_NEAREST_PRIMARY_CLASS"),
    father_education = rank_one("miss_father_educ")
  )
}

missingness_correlation_matrix <- function(df, miss_vars, group_vars, cts_vars) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  miss_vars <- intersect(miss_vars, names(df))
  if (!length(miss_vars) || !nrow(df)) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  miss_df <- as.data.frame(lapply(df[miss_vars], function(x) as.integer(is.na(x))), check.names = FALSE)
  names(miss_df) <- paste0("miss_", names(miss_df))
  cts <- intersect(cts_vars, names(df))
  cts_df <- if (length(cts)) as.data.frame(lapply(df[cts], numeric_like), check.names = FALSE) else data.frame()
  grp <- intersect(group_vars, names(df))
  grp_df <- if (length(grp)) {
    grp_data <- as.data.frame(lapply(df[grp], function(x) {
      x <- as.character(x)
      x[is.na(x) | !nzchar(x)] <- "missing"
      factor(x)
    }), check.names = FALSE)
    # Legacy Chunk 8 used model.matrix-style expansions of sector, sex,
    # religion, social group, state, and related grouping variables before
    # computing missingness correlations.  Small diagnostic subsets, especially
    # enrolled-only slices used in tests or regional filters, may contain only a
    # single observed level for one or more grouping variables.  model.matrix()
    # cannot apply contrasts to one-level factors, so drop constant grouping
    # variables while preserving all grouping variables with real variation.
    varying <- vapply(grp_data, function(x) length(unique(as.character(x))) > 1L, logical(1))
    grp_data <- grp_data[varying]
    if (length(grp_data)) {
      stats::model.matrix(~ . - 1, data = grp_data)
    } else {
      matrix(numeric(), nrow = nrow(df), ncol = 0L)
    }
  } else matrix(numeric(), nrow = nrow(df), ncol = 0L)
  legacy_safe_cor(cbind(miss_df, cts_df, grp_df))
}

#' check missing logit parallel
#'
#' Legacy Chunk 8 used mclapply/parLapply after defining the same one-logit-per-
#' missing-variable problem.  Targets already parallelizes at the target level,
#' so this implementation keeps deterministic in-process lapply while preserving
#' the model specification and BH adjustment.
check_missing_logit_parallel <- function(df, miss_vars, covars, method_p = "BH") {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  miss_vars <- intersect(miss_vars, names(df))
  covars <- intersect(covars, names(df))
  if (!length(miss_vars) || !length(covars)) return(data.frame())
  rhs <- paste(covars, collapse = " + ")
  fit_one <- function(m) {
    y <- is.na(df[[m]])
    if (length(unique(y)) < 2L) return(data.frame())
    f <- stats::as.formula(paste0("is.na(`", m, "`) ~ ", rhs))
    tryCatch({
      fit <- stats::glm(f, data = df, family = stats::binomial)
      pseudoR2 <- 1 - fit$deviance / fit$null.deviance
      out <- broom::tidy(fit)
      out$missing_var <- m
      out$pseudoR2 <- pseudoR2
      out$nobs <- stats::nobs(fit)
      out
    }, error = function(e) {
      data.frame(term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, missing_var = m, pseudoR2 = NA_real_, nobs = length(y), status = "failed", reason = conditionMessage(e), stringsAsFactors = FALSE)
    })
  }
  out <- safe_bind_rows(lapply(miss_vars, fit_one))
  adjust_missingness_pvalues_bh(out, method_p = method_p)
}

run_missingness_logits <- function(df, miss_vars, covars) {
  check_missing_logit_parallel(df, miss_vars, covars)
}

adjust_missingness_pvalues_bh <- function(df, method_p = "BH") {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || !"p.value" %in% names(df)) return(df)
  df$p_adj <- NA_real_
  idx <- !is.na(df$p.value) & df$term != "(Intercept)"
  df$p_adj[idx] <- stats::p.adjust(df$p.value[idx], method = method_p)
  df
}

summarize_missingness_logits <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!nrow(df) || !all(c("missing_var", "p_adj", "pseudoR2") %in% names(df))) {
    return(data.frame(missing_var = character(), n_sig = integer(), pseudoR2 = numeric(), stringsAsFactors = FALSE))
  }
  split_df <- split(df, df$missing_var)
  out <- safe_bind_rows(lapply(split_df, function(x) {
    pmax <- suppressWarnings(max(x$pseudoR2, na.rm = TRUE))
    if (!is.finite(pmax)) pmax <- NA_real_
    data.frame(
      missing_var = x$missing_var[[1]],
      n_sig = sum(x$p_adj < 0.05 & x$term != "(Intercept)", na.rm = TRUE),
      pseudoR2 = pmax,
      stringsAsFactors = FALSE
    )
  }))
  out[order(-out$pseudoR2), , drop = FALSE]
}


missingness_correlation_pairs <- function(mat, top_n = 50L) {
  mat <- as.matrix(mat)
  if (!length(mat) || nrow(mat) < 2L || ncol(mat) < 2L) {
    return(data.frame(var1 = character(), var2 = character(), correlation = numeric(), abs_correlation = numeric(), stringsAsFactors = FALSE))
  }
  mat[lower.tri(mat, diag = TRUE)] <- NA_real_
  idx <- which(is.finite(mat), arr.ind = TRUE)
  if (!nrow(idx)) {
    return(data.frame(var1 = character(), var2 = character(), correlation = numeric(), abs_correlation = numeric(), stringsAsFactors = FALSE))
  }
  out <- data.frame(
    var1 = rownames(mat)[idx[, "row"]],
    var2 = colnames(mat)[idx[, "col"]],
    correlation = mat[idx],
    stringsAsFactors = FALSE
  )
  out$abs_correlation <- abs(out$correlation)
  out <- out[order(-out$abs_correlation), , drop = FALSE]
  utils::head(out, top_n)
}

save_missingness_correlation_heatmap <- function(mat, path, max_vars = 35L, title = "Missingness correlation matrix") {
  mat <- as.matrix(mat)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!length(mat) || nrow(mat) < 2L || ncol(mat) < 2L) {
    grDevices::png(path, width = 900, height = 500, res = 120)
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No correlation matrix available")
    grDevices::dev.off()
    return(normalizePath(path, mustWork = FALSE))
  }
  score <- apply(abs(mat), 1L, function(x) max(x[is.finite(x) & x < 1], na.rm = TRUE))
  score[!is.finite(score)] <- 0
  keep <- names(sort(score, decreasing = TRUE))[seq_len(min(max_vars, length(score)))]
  mat <- mat[keep, keep, drop = FALSE]
  grDevices::png(path, width = 1400, height = 1200, res = 140)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(old); grDevices::dev.off() }, add = TRUE)
  graphics::par(mar = c(11, 11, 4, 2))
  graphics::image(
    x = seq_len(ncol(mat)),
    y = seq_len(nrow(mat)),
    z = t(mat[nrow(mat):1, , drop = FALSE]),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title,
    zlim = c(-1, 1)
  )
  graphics::axis(1, at = seq_len(ncol(mat)), labels = colnames(mat), las = 2, cex.axis = 0.55)
  graphics::axis(2, at = seq_len(nrow(mat)), labels = rev(rownames(mat)), las = 2, cex.axis = 0.55)
  graphics::box()
  normalizePath(path, mustWork = FALSE)
}


save_missingness_logit_plot <- function(summary_df, path, title = "Structured missingness logit screen", top_n = 25L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(summary_df, stringsAsFactors = FALSE)
  grDevices::png(path, width = 1100, height = 850, res = 130)
  old <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(old); grDevices::dev.off() }, add = TRUE)
  if (!nrow(df) || !all(c("missing_var", "pseudoR2") %in% names(df))) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No missingness-logit summary available")
    return(normalizePath(path, mustWork = FALSE))
  }
  df <- df[is.finite(df$pseudoR2), , drop = FALSE]
  if (!nrow(df)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "No finite pseudo-R² values available")
    return(normalizePath(path, mustWork = FALSE))
  }
  df <- df[order(df$pseudoR2, decreasing = TRUE), , drop = FALSE]
  df <- utils::head(df, top_n)
  graphics::par(mar = c(5, 13, 4, 2))
  graphics::barplot(
    rev(df$pseudoR2),
    names.arg = rev(df$missing_var),
    horiz = TRUE,
    las = 1,
    xlab = "Pseudo-R² (predictability of missingness)",
    main = title
  )
  normalizePath(path, mustWork = FALSE)
}

save_missingness_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/missingness") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!inherits(diagnostics, "emi_missingness_diagnostics")) diagnostics <- list(missing_counts = as.data.frame(diagnostics))
  paths <- c(
    missing_counts = write_diagnostic_csv(diagnostics$missing_counts %||% data.frame(), file.path(dir, "missingness_counts.csv")),
    regional_cost = write_diagnostic_csv(diagnostics$regional_cost %||% data.frame(), file.path(dir, "regional_missingness_cost.csv")),
    regional_distance = write_diagnostic_csv(diagnostics$regional_distance %||% data.frame(), file.path(dir, "regional_missingness_distance.csv")),
    regional_father = write_diagnostic_csv(diagnostics$regional_father_education %||% data.frame(), file.path(dir, "regional_missingness_father_education.csv")),
    logit_all = write_diagnostic_csv(diagnostics$logit_all %||% data.frame(), file.path(dir, "missingness_logits_all.csv")),
    logit_enrolled = write_diagnostic_csv(diagnostics$logit_enrolled %||% data.frame(), file.path(dir, "missingness_logits_enrolled.csv")),
    logit_summary = write_diagnostic_csv(diagnostics$logit_summary %||% data.frame(), file.path(dir, "missingness_logit_summary.csv")),
    case_study = write_diagnostic_csv(diagnostics$case_study %||% data.frame(), file.path(dir, "missingness_rajasthan_southern_case_study.csv")),
    chi_square = write_diagnostic_csv(diagnostics$chi_square %||% data.frame(), file.path(dir, "missingness_chi_square_tests.csv")),
    notes = write_diagnostic_csv(diagnostics$notes %||% data.frame(), file.path(dir, "missingness_legacy_notes.csv"))
  )
  if (length(diagnostics$corr_all)) {
    paths <- c(
      paths,
      corr_all = write_diagnostic_matrix(diagnostics$corr_all, file.path(dir, "missingness_correlation_all.csv")),
      corr_all_pairs = write_diagnostic_csv(missingness_correlation_pairs(diagnostics$corr_all), file.path(dir, "missingness_correlation_all_top_pairs.csv")),
      corr_all_heatmap = save_missingness_correlation_heatmap(diagnostics$corr_all, file.path(dir, "missingness_correlation_all.png"), title = "Missingness correlations: all observations")
    )
  }
  if (length(diagnostics$corr_enrolled)) {
    paths <- c(
      paths,
      corr_enrolled = write_diagnostic_matrix(diagnostics$corr_enrolled, file.path(dir, "missingness_correlation_enrolled.csv")),
      corr_enrolled_pairs = write_diagnostic_csv(missingness_correlation_pairs(diagnostics$corr_enrolled), file.path(dir, "missingness_correlation_enrolled_top_pairs.csv")),
      corr_enrolled_heatmap = save_missingness_correlation_heatmap(diagnostics$corr_enrolled, file.path(dir, "missingness_correlation_enrolled.png"), title = "Missingness correlations: enrolled observations")
    )
  }
  if (nrow(diagnostics$logit_summary %||% data.frame())) {
    paths <- c(
      paths,
      logit_pseudo_r2_plot = save_missingness_logit_plot(
        diagnostics$logit_summary,
        file.path(dir, "missingness_logit_pseudo_r2.png"),
        title = "How structured is missingness?"
      )
    )
  }
  legacy_output_manifest(paths)
}

# sample-end: code-missingness-logit-parallel

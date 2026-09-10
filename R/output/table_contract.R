# Shared public-table captions and notes.
# This module is sourced by both the targets table writer and standalone Quarto helpers.

regression_star_note <- function() "* p < 0.05, ** p < 0.01, *** p < 0.001"

public_table_caption_text <- function(name) {
  captions <- c(
    selection_n = "Enrollment Participation Model Sample Size",
    sum_tbl_probit_quant = "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
    sum_tbl_probit_cat = "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
    probit_mfx = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
    sum_tbl_iv = "Summary Statistics for 2SLS Model",
    paper_core_summary = "Core Variables and Summary Statistics",
    paper_schooling_welfare = "Schooling Margins and Later Household Consumption",
    paper_schooling_market_geography = "How the Schooling Market Is Geographically Organized",
    paper_language_behavior = "Linguistic Distance and Language Behavior",
    paper_conversion_complements = "Predetermined Complements to Economic Conversion",
    paper_local_development = "Broader Local-Development Constellation",
    paper_identification_boundary = "Identification Boundary: Linguistic Distance Does Not Isolate EMI",
    fs_cons = "First-Stage Regression: EMI Exposure on Linguistic Distance",
    cons_iv = "Second-Stage Regression: Real Log Consumption Growth on EMI Exposure (Fitted)",
    ame_results = "Average Marginal Effects Results",
    first_stage = "First-Stage Diagnostic Results",
    english_opportunity_mechanism = "Linguistic-Distance Association Across Mechanism Stages"
  )
  captions[[name]] %||% name
}

regression_caption <- function(cap) cap

table_caption <- function(name) public_table_caption_text(name)

public_table_note <- function(name) {
  switch(name,
    sum_tbl_probit_quant = "Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.",
    sum_tbl_iv = "Min. = minimum; 1Q = first quartile; Med. = median; 3Q = third quartile; Max. = maximum; Mean = arithmetic mean; SD = standard deviation; N = number of observations.",
    paper_core_summary = paste(
      "District-level descriptive statistics; N is the number of districts with finite values.",
      "Schooling measures use NSS 2007-08 children age 5-19; linguistic distance and predetermined capacity use Census-2001 geography.",
      "Modern consumption rows summarize preferred-eligible small-domain district estimates after the registered survey-design, price, and lineage gates.",
      "p10 and p90 are the 10th and 90th percentiles. Sources and construction details are documented in the data appendix."
    ),
    paper_schooling_market_geography = paste(
      "Panel A reports standardized linguistic-distance associations under the canonical raw, region-plus-controls, and state-plus-controls specifications.",
      "Panel B reports R-squared from regressions of each district measure on state indicators alone.",
      "Panel C reports raw and state-residual Pearson correlations between DISE 2007-08 English-medium enrollment shares and the corresponding NSS measures.",
      "Each Panel-A outcome uses one fixed complete-case sample across specifications; DISE is independent administrative validation rather than a replacement treatment definition."
    ),
    paper_language_behavior = paste(
      "Census-2001 C-17 state-by-native-language regressions are weighted by native speakers and include state fixed effects, native-language state share, and a modal-language indicator.",
      "Standard errors and p-values use HC1 heteroskedasticity-robust inference; partial R-squared is the one-degree-of-freedom model-based partial R-squared for the reported distance coefficient.",
      "Continuous-distance coefficients are percentage-point changes per one Shastry distance degree; distant-language rows compare languages at least three degrees from Hindi with the remaining mapped languages.",
      "Panel B applies the predeclared Hindi-belt sample restriction; the table is descriptive mechanism evidence rather than an instrumental-variable first stage."
    ),
    paper_conversion_complements = paste(
      "Entries are interaction coefficients with state-clustered standard errors in parentheses; stars use Holm-adjusted p-values within each predeclared panel family.",
      "Panel A reports the change in the 2004-05 to 2022-23 schooling-consumption association for a one-standard-deviation increase in each predetermined Census-2001 complement, per 10 percentage points of schooling exposure.",
      "Panel B reports the corresponding 2005 IT-employment-environment interaction for all-child EMI per 10 percentage points and for linguistic distance per one Shastry degree.",
      regression_star_note(),
      "These are descriptive heterogeneity estimates and do not make the schooling exposure or linguistic-distance interaction causal."
    ),
    paper_local_development = paste(
      "Entries are reduced-form associations of one Shastry linguistic-distance degree with the representative outcome in its native unit; raw and Holm-adjusted p-values use state-clustered inference within the registered outcome/specification family.",
      "Household-capacity, migration, finance, asset, and labor rows use state fixed effects plus predetermined controls; Economic Census rows use the registered region-plus-controls specification, which is the predeclared comparison carrying the sector-composition signal.",
      "The 2001-11 Census and 2005-13 Economic Census windows overlap the 2007 schooling measurement, so these rows are co-evolving development margins rather than post-treatment EMI mechanisms.",
      "PLFS 2017-18 supplies the displayed labor endpoint; the semantic CSV also retains matched NSS66 Holm p-values so the broad-labor null is auditable across survey waves.",
      "Heterogeneous units are intentionally not standardized onto a common axis. Null rows are retained by design because the table asks whether the pattern reflects selective transformation or broad local expansion."
    ),
    paper_identification_boundary = paste(
      "Panel A compares excluded-instrument F statistics on one common district support for the unadjusted and state-fixed-effects plus predetermined-controls specifications; partial R-squared refers to the within-state specification.",
      "The five-share language vector is a joint test, while the other rows contain one excluded scalar instrument. Historical 1991 reconstructions use a much smaller validated geography and are kept in the appendix rather than mixing noncomparable first-stage support into this panel.",
      "Panel B reports conventional 2SLS coefficients for the registered 2004-05 to 2022-23 and 2023-24 long changes, alongside Montiel Olea-Pflueger effective F and Anderson-Rubin confidence-set topology.",
      "Disconnected AR sets that span both signs do not identify the sign of an EMI effect even when beta=0 is rejected on the finite grid. These diagnostics define an identification boundary, not preferred causal estimates."
    ),
    paper_schooling_welfare = paste(
      "Entries are percent changes in real mean MPCE associated with a 10 percentage-point increase in the schooling margin; state-clustered standard errors are in parentheses.",
      "All columns use the registered state fixed-effects specification with predetermined Census-2001 controls.",
      "Within each welfare endpoint and estimand, all five schooling margins use one common complete-case district sample.",
      regression_star_note(),
      "Stars use the Holm-adjusted p-value for the predeclared schooling-welfare family within that endpoint; the estimates are descriptive conditional associations, not causal returns."
    ),
    sum_tbl_probit_cat = "Values = all possible values; Mode = most frequent value; Pct. Mode = percent of observations taking the modal value; Least Freq. = least frequent value; Pct. Least Freq. = percent of observations taking the least frequent value; N = number of observations.",
    probit_mfx = "NSS 64th round; design-based SEs in parentheses.",
    fs_cons = "Standard errors clustered by state in parentheses.",
    cons_iv = "Standard errors clustered by state in parentheses.",
    english_opportunity_mechanism = paste(
      "Entries are signed partial correlations with linguistic distance.",
      "District rows use a fixed outcome-specific sample across columns; region and state specifications add predetermined Census-2001 controls.",
      "The C-17 state-by-language row is reported only within state and uses its own language-prevalence and modal-language controls.",
      "Rows are cross-source mechanism evidence, not a sequential mediation model."
    ),
    NULL
  )
}

table_note <- public_table_note

table_contract_cell_string <- function(value) {
  if (length(value) == 0L) return("")
  if (is.data.frame(value) || is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  if (length(value) == 0L || all(is.na(value))) return("")
  paste(as.character(value), collapse = "; ")
}

table_contract_column_strings <- function(column) {
  if (is.factor(column)) column <- as.character(column)
  out <- if (is.list(column)) {
    vapply(column, table_contract_cell_string, character(1))
  } else {
    as.character(column)
  }
  out[is.na(out)] <- ""
  out
}

summary_table_groups <- function(df) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || !length(names(df))) {
    return(list(data = df, groups = data.frame()))
  }
  empty_rest <- if (ncol(df) > 1L) {
    apply(df[-1], 1, function(x) {
      all(!nzchar(table_contract_column_strings(x)))
    })
  } else {
    rep(TRUE, nrow(df))
  }
  first_col <- table_contract_column_strings(df[[1L]])
  group_row <- grepl(":$", first_col) & empty_rest
  group_idx <- which(group_row)
  if (!length(group_idx)) return(list(data = df, groups = data.frame()))

  groups <- lapply(seq_along(group_idx), function(i) {
    start_orig <- group_idx[[i]] + 1L
    end_orig <- if (i < length(group_idx)) group_idx[[i + 1L]] - 1L else nrow(df)
    start <- start_orig - sum(group_idx < start_orig)
    end <- end_orig - sum(group_idx <= end_orig)
    if (start > end) return(NULL)
    data.frame(
      label = first_col[[group_idx[[i]]]],
      start = start,
      end = end,
      stringsAsFactors = FALSE
    )
  })
  groups <- do.call(rbind, Filter(Negate(is.null), groups))
  if (is.null(groups)) groups <- data.frame()
  list(data = df[!group_row, , drop = FALSE], groups = groups)
}

table_header_labels <- function(df, name) {
  labels <- names(df)
  wrap <- if (identical(name, "sum_tbl_probit_cat")) {
    c("Adjusted R-squared" = "Adjusted\nR-squared")
  } else {
    c(
      "Pct. Mode" = "Pct.\nMode",
      "Least Freq." = "Least\nFreq.",
      "Pct. Least Freq." = "Pct. Least\nFreq.",
      "Adjusted R-squared" = "Adjusted\nR-squared"
    )
  }
  labels <- ifelse(labels %in% names(wrap), unname(wrap[labels]), labels)
  vapply(labels, function(label) {
    if (grepl("\n", label, fixed = TRUE)) {
      kableExtra::linebreak(label, align = "c")
    } else {
      label
    }
  }, character(1))
}

caption_for_latex <- function(name) table_caption(name)

latex_escape_text <- function(x) {
  # Public table cells are ordinary text while kableExtra styling is raw LaTeX.
  # Escape character-by-character instead of chaining gsub() replacements:
  # replacement backslashes have their own semantics in sub()/gsub(), which can
  # silently double or consume escapes (notably for ~ and ^). A lookup table is
  # explicit, deterministic, and leaves all non-LaTeX characters untouched.
  x <- table_contract_column_strings(x)
  latex_escapes <- c(
    "\\" = "\\textbackslash{}",
    "#" = "\\#",
    "$" = "\\$",
    "%" = "\\%",
    "&" = "\\&",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )
  vapply(x, function(value) {
    chars <- strsplit(value, "", fixed = TRUE)[[1L]]
    escaped <- latex_escapes[chars]
    escaped[is.na(escaped)] <- chars[is.na(escaped)]
    paste0(escaped, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

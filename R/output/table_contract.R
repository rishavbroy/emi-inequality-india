# Shared public-table captions and notes.
# This module is sourced by both the targets table writer and standalone Quarto helpers.

# Reader-facing vocabulary shared across manuscript tables.  These labels are
# deliberately descriptive rather than implementation-specific so table text stays
# synchronized with the definitions in paper-new.qmd.
paper_schooling_display_labels <- function() {
  c(
    enrollment = "Enrollment",
    emi_enrolled = "English-medium share among enrolled",
    emi_all_children = "English-medium exposure among all children",
    public_emi = "Public-school English-medium exposure",
    private_emi = "Private-school English-medium exposure",
    private_enrollment = "Private-school enrollment",
    dise_emi = "DISE English-medium enrollment share"
  )
}

paper_linguistic_distance_display_labels <- function() {
  c(
    nonzero_mean = "Speaker-weighted distance from Hindi",
    top3_legacy = "Top-three-language distance from Hindi",
    glottolog_mean = "Genealogical distance from Hindi",
    dyen_noncognate = "Lexical noncognacy with Hindi",
    distant_share = "Share speaking languages distant from Hindi",
    distance_shares_all = "Language-distance composition",
    historical_1991 = "Historical speaker-weighted distance from Hindi"
  )
}

regression_star_levels <- function() c("*" = 0.10, "**" = 0.05, "***" = 0.01)

significance_stars <- function(p) {
  cutoffs <- regression_star_levels()
  ifelse(is.finite(p) & p < cutoffs[["***"]], "***",
    ifelse(is.finite(p) & p < cutoffs[["**"]], "**",
      ifelse(is.finite(p) & p < cutoffs[["*"]], "*", "")))
}

regression_star_note <- function() "* p < 0.10, ** p < 0.05, *** p < 0.01"

public_table_caption_text <- function(name) {
  captions <- c(
    selection_n = "Enrollment Participation Model Sample Size",
    sum_tbl_probit_quant = "Summary Statistics for Enrollment Participation Model (Numeric Variables)",
    sum_tbl_probit_cat = "Summary Statistics for Enrollment Participation Model (Categorical Variables)",
    probit_mfx = "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit",
    sum_tbl_iv = "Summary Statistics for 2SLS Model",
    paper_core_summary = "Core Variables and Summary Statistics. Sources: NSS 64th Round; Census of India 2001; NSS 61st Round; HCES 2022-23 and 2023-24.",
    paper_schooling_market_geography = paste(
      "Linguistic Distance and English-Learning Opportunities across Geographic Specifications.",
      "Sources: NSS 64th Round, 2007-08; DISE 2007-08; Census of India 2001."
    ),
    paper_language_behavior = paste(
      "Linguistic Distance and Language-Learning Behavior.",
      "Source: Census of India 2001 bilingualism and trilingualism tables."
    ),
    paper_economic_conversion = paste(
      "English-Intensive Schooling, Later Consumption, and Predetermined Complements.",
      "Sources: NSS 64th Round, 2007-08; Census of India 2001; NSS 61st Round, 2004-05; HCES 2022-23 and 2023-24."
    ),
    paper_local_development = paste(
      "Linguistic Conditions and Selective Local Development.",
      "Sources: Census of India 2001 and 2011; Economic Census 2005 and 2013; NSS 66th Round; PLFS 2017-18."
    ),
    appendix_a3_lineage_source_hierarchy = paste(
      "District-Lineage Sources and Roles.",
      "Sources: Census of India; Ministry of Panchayati Raj Local Government Directory; Kumar and Somanathan (2016); India State and District Evolution Database; India District Changes Tracker; Development Data Lab SHRUG; Deshpande, Khanna, and Walia concordances."
    ),
    appendix_iv_relevance_summary = paste(
      "First-Stage Relevance Across Linguistic Measures.",
      "Sources: Census of India 1991 and 2001; NSS 64th Round, 2007-08."
    ),
    appendix_iv_weak_inference = paste(
      "Weak-IV-Robust Long-Run Inference.",
      "Sources: Census of India 2001; NSS 61st Round, 2004-05; NSS 64th Round, 2007-08; HCES 2022-23 and 2023-24."
    ),
    appendix_migration_summary = paste(
      "Linguistic Distance and Migration Composition.",
      "Source: Census of India 2011 D-02, D-03, and D-07 migration tables."
    ),
    appendix_selection_ame = paste(
      "Correlates of School Enrollment: Average Marginal Effects from a Survey-Weighted Probit.",
      "Source: NSS 64th Round, 2007-08, Participation and Expenditure in Education."
    ),
    appendix_selection_missingness = paste(
      "Missingness in the Child-Enrollment Probit.",
      "Source: NSS 64th Round, 2007-08, Participation and Expenditure in Education."
    ),
    fs_cons = "First-Stage Regression: EMI Exposure on Linguistic Distance",
    cons_iv = "Second-Stage Regression: Real Log Consumption Growth on EMI Exposure (Fitted)",
    ame_results = "Average Marginal Effects Results",
    first_stage = "First-Stage Diagnostic Results",
    english_opportunity_mechanism = "Linguistic-Distance Association Across Mechanism Stages"
  )
  if (name %in% names(captions)) {
    return(unname(captions[[name]]))
  }
  name
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
      "Modern consumption rows summarize districts meeting the survey-design, price, and district-lineage requirements used for the preferred estimates.",
      "p10 and p90 are the 10th and 90th percentiles.",
      "Sources: NSS 64th Round; Census of India 2001; NSS 61st Round; HCES 2022-23 and 2023-24. Construction details are documented in the data appendix."
    ),
    paper_schooling_market_geography = paste(
      "Each outcome block reports standardized coefficients from district regressions on speaker-weighted linguistic distance from Hindi. State-clustered standard errors are in parentheses.",
      "Columns (1)-(3) are unadjusted, region fixed effects plus predetermined Census-2001 controls, and state fixed effects plus the same controls, respectively; the rows below each coefficient indicate these specification choices explicitly.",
      "Each outcome uses one fixed complete-case sample across its three specifications. State-membership R-squared is calculated from state indicators alone on the available state-coded support for each schooling measure, including DISE, and is descriptive rather than causal.",
      regression_star_note(),
      "Sources: NSS 64th Round, 2007-08; DISE 2007-08; Census of India 2001."
    ),
    paper_language_behavior = paste(
      "Each column is a weighted state-by-language regression. Standard errors in parentheses use HC1 heteroskedasticity-robust inference.",
      "All specifications include state fixed effects, native-language state share, an indicator for the state's most common native language, and a Hindi/Urdu reference indicator where required by the registered specification.",
      "Continuous-distance coefficients are percentage-point changes per one Shastry distance degree; the distant-language indicator equals one for languages at least three degrees from Hindi.",
      "Partial R-squared is the one-degree-of-freedom model-based partial R-squared for the reported linguistic-distance coefficient.",
      "Columns (5)-(7) apply the predeclared Hindi-belt sample restriction. These are descriptive associations with language-learning behavior rather than instrumental-variable first stages.",
      regression_star_note(),
      "Source: Census of India 2001 bilingualism and trilingualism tables."
    ),
    paper_economic_conversion = paste(
      "Each schooling-margin coefficient comes from a separate district regression and reports the percent difference in real mean MPCE associated with a 10 percentage-point increase in that schooling margin; coefficients shown in the same column are assembled for comparison rather than jointly estimated.",
      "Columns use state fixed effects, predetermined Census-2001 controls, and common schooling support within each endpoint/estimand. State-clustered standard errors are in parentheses; stars use Holm-adjusted p-values within the predeclared schooling-welfare family.",
      "The six interaction rows appear only for the 2004-05 to 2022-23 long change and report how the all-child or private-EMI association differs with a one-standard-deviation increase in predetermined human capital, urbanization, or ST concentration. Those rows also come from separate registered regressions and use state-clustered standard errors and Holm-adjusted p-values within the predeclared complement family.",
      regression_star_note(),
      "These are descriptive conditional associations, not causal returns or treatment-effect heterogeneity. Sources are identified in the caption."
    ),
    paper_local_development = paste(
      "Each column is a separate district regression. The coefficient reports the association of one Shastry linguistic-distance degree with the listed outcome in its native unit; state-clustered standard errors are in parentheses and stars use Holm-adjusted p-values within the registered outcome/specification family.",
      "Household-capacity, migration, finance, asset, and labor regressions use state fixed effects plus predetermined controls; Economic Census regressions use region fixed effects plus the same controls, as reported in the table.",
      regression_star_note(),
      "The 2001-11 Census and 2005-13 Economic Census windows overlap the 2007 schooling measurement, so these are co-evolving development margins rather than post-treatment EMI mechanisms.",
      "PLFS 2017-18 supplies the displayed labor endpoint; the semantic CSV retains the raw and Holm-adjusted p-values and matched NSS66 adjusted p-values. Heterogeneous outcomes remain in native units, and null outcomes are retained by design. Sources are identified in the caption."
    ),
    appendix_a3_lineage_source_hierarchy = paste(
      "Rows summarize source families rather than individual files. Detailed source IDs and adjudication ledgers remain available in the lineage CSV outputs."
    ),
    appendix_iv_relevance_summary = paste(
      "The first three columns report the state-clustered joint excluded-instrument F statistic under no geographic fixed effects, six-region fixed effects plus the main predetermined controls, and state fixed effects plus the same controls.",
      "The final relevance column is the state-FE partial R-squared. The historical row uses its validated 1991 common support and is therefore not numerically pooled with the modern district sample.",
      "These conventional first-stage statistics describe relevance only; they are not substituted for the Montiel Olea-Pflueger effective-F statistic used for weak-IV assessment."
    ),
    appendix_iv_weak_inference = paste(
      "Columns report conventional 2SLS coefficients with state-clustered standard errors in parentheses. No significance stars are shown because weak-IV-robust inference takes precedence over conventional 2SLS t tests.",
      "MOP effective F is the Montiel Olea-Pflueger weak-instrument statistic. AR denotes Anderson-Rubin inference; the reported 95% sets are the accepted values on the registered inversion grid and are disconnected across positive and negative values.",
      "The direct-effect row reports the smallest bounded exclusion violation, as a share of the absolute reduced form, needed for beta=0 to enter the 95% AR set."
    ),
    appendix_migration_summary = paste(
      "Each column is a separate district regression using speaker-weighted linguistic distance from Hindi, predetermined Census-2001 controls, state fixed effects, and state-clustered standard errors.",
      "National significance stars use Holm-adjusted p-values across the eight registered migration outcomes in the preferred specification; the predeclared Hindi-belt restriction uses its single raw p-value.",
      regression_star_note(),
      "The dependent variables are shares, so coefficients are changes in shares associated with a one-degree increase in linguistic distance. These are conditional associations, not causal migration effects."
    ),
    appendix_selection_ame = paste(
      "Entries are average changes in predicted enrollment probability.",
      "Continuous covariates are reported as average slopes; categorical covariates are average discrete comparisons against the stated reference category, evaluated over the observed covariate distribution.",
      "The probit uses NSS survey weights and design-based standard errors clustered at the primary sampling unit.",
      "District-level schooling-context controls constructed from enrolled children remain in the fitted model but are omitted from the table and are not interpreted causally."
    ),
    appendix_selection_missingness = paste(
      "The final row reports children missing at least one covariate required by the probit.",
      "Pseudo-R2 values come from the registered missingness-logit screen; the blank aggregate cell has no corresponding missingness regression.",
      "No missing-value imputation is used."
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

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
    paper_identification_boundary = "Identification Boundary: Linguistic Distance Does Not Isolate EMI",
    appendix_a1_data_source_timing = "Appendix A1. Data Sources, Timing, and Analytical Roles",
    appendix_a3_lineage_source_hierarchy = "Appendix A3. District-Lineage Evidence Hierarchy",
    appendix_a4_nss_schooling_constructs = "Appendix A4. NSS Schooling Construct Definitions",
    appendix_a5_dise_construction = "Appendix A5. DISE Schooling Constructs",
    appendix_a6_linguistic_measures = "Appendix A6. Linguistic-Measure Construction",
    appendix_a7_consumption_construction = "Appendix A7. Consumption Survey Construction",
    appendix_a8_other_outcome_panels = "Appendix A8. Other Outcome Panels",
    appendix_b1_lineage_readiness = "Appendix B1. District-Lineage Readiness and Coverage",
    appendix_b2_lineage_sensitivity = "Appendix B2. District-Lineage Sensitivity",
    appendix_b3_consumption_reconstruction = "Appendix B3. Consumption Reconstruction Benchmarks",
    appendix_b4_hces_consistency_summary = "Appendix B4. HCES Cross-Round Consistency Summary",
    appendix_b6_language_source_validation = paste(
      "Appendix B6. Historical Language-Source Validation.",
      "Sources: Census of India 1991 Language Atlas; Census of India 2001 language tables; Helms and Lim replication data."
    ),
    appendix_b8_census_universe_reconciliation = "Appendix B8. Census Universe Reconciliation",
    appendix_b9_dise_publication_validation = "Appendix B9. DISE Publication Validation",
    appendix_c1_full_absorption_ladder = "Appendix C1. Full First-Stage Absorption Ladder",
    appendix_c3_control_block_absorption = "Appendix C3. Control-Block Absorption",
    appendix_c4_geographic_scale_sensitivity = "Appendix C4. Geographic and Scale Sensitivity",
    appendix_c5_alternative_scalar_distances = "Appendix C5. Alternative Linguistic-Distance First Stages",
    appendix_c6_mapping_composition_sensitivity = "Appendix C6. Mapping and Language-Composition Sensitivity",
    appendix_c7_historical_balance = "Appendix C7. Historical Balance Across Baseline Domains",
    appendix_c9_historical_first_stage = "Appendix C9. Historical First-Stage Comparison",
    appendix_c11_multiple_instruments = "Appendix C11. Multiple Instruments, Overidentification, and Falsification-Adaptive Sets",
    appendix_c13_robustness_family_census = "Appendix C13. Registered Consumption-IV Robustness Families",
    appendix_c14_exclusion_sensitivity = "Appendix C14. Sensitivity to Imperfect Exclusion",
    appendix_d1_migration = "Appendix D1. Census Migration Outcomes",
    appendix_d2_migration_context = "Appendix D2. Migration Context and Hindi-Belt Sensitivity",
    appendix_d3_housing_assets = "Appendix D3. Housing, Finance, and Durable-Asset Outcomes",
    appendix_d4_economic_census = "Appendix D4. Economic Census Outcomes",
    appendix_d5_labor = "Appendix D5. NSS and PLFS Labor Outcomes",
    appendix_d6_household_capacity = "Appendix D6. Household-Capacity Trajectories",
    appendix_d7_social_heterogeneity = "Appendix D7. Social-Group and ST-Concentration Heterogeneity",
    appendix_d9_residual_spatial_diagnostics = "Appendix D9. Residual Spatial Diagnostics",
    appendix_e1_selection_sample = "Appendix E1. Enrollment Selection Sample and Covariates",
    appendix_e4_missingness = "Appendix E4. Missingness in the Enrollment Selection Sample",
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
      "Entries are reduced-form associations of one Shastry linguistic-distance degree with the representative outcome in its native unit; raw and Holm-adjusted p-values use state-clustered inference within the registered outcome/specification family.",
      "Household-capacity, migration, finance, asset, and labor rows use state fixed effects plus predetermined controls; Economic Census rows use the registered region-plus-controls specification, which is the predeclared comparison carrying the sector-composition signal.",
      "The 2001-11 Census and 2005-13 Economic Census windows overlap the 2007 schooling measurement, so these rows are co-evolving development margins rather than post-treatment EMI mechanisms.",
      "PLFS 2017-18 supplies the displayed labor endpoint; the semantic CSV also retains matched NSS66 Holm p-values so the broad-labor null is auditable across survey waves.",
      "Heterogeneous units are intentionally not standardized onto a common axis. Null rows are retained by design because the table asks whether the pattern reflects selective transformation or broad local expansion."
    ),
    appendix_a1_data_source_timing = paste(
      "Final-paper source families are shown once at their primary analytical role.",
      "All raw microdata and administrative files remain local unless redistribution terms explicitly permit otherwise."
    ),
    appendix_a3_lineage_source_hierarchy = paste(
      "The lineage registry separates official administrative evidence from independent concordances/geometry and QA/adjudication evidence.",
      "Accepted mappings must cite registered evidence and satisfy the lineage-readiness gates."
    ),
    appendix_a4_nss_schooling_constructs = paste(
      "All NSS-64 schooling measures use children age 5-19.",
      "Conditional EMI uses enrolled children with known medium as its denominator; all-child exposure uses all eligible children, so participation and medium choice remain distinct margins."
    ),
    appendix_a5_dise_construction = paste(
      "DISE constructs are derived from the canonical construct registry.",
      "The main paper uses total-enrollment EMI as independent administrative validation; age-scaled and pooled alternatives remain diagnostic robustness constructs."
    ),
    appendix_a6_linguistic_measures = paste(
      "The preferred measure is the speaker-weighted Shastry distance from Hindi on Census-2001 geography.",
      "Historical, genealogical, lexicostatistical, and nonlinear measures are validation or sensitivity constructions rather than interchangeable instruments."
    ),
    appendix_a7_consumption_construction = paste(
      "Rows come directly from the registered household-consumption survey contract.",
      "Historical NSS and modern HCES rounds retain their actual schedule/recall and district-identity rules before harmonization to Census-2001 districts."
    ),
    appendix_a8_other_outcome_panels = paste(
      "These panels provide the broader local-development outcomes used in the paper.",
      "Their observation windows differ, so they are interpreted as co-evolving development margins rather than mechanically post-treatment mediators of 2007-08 schooling."
    ),
    appendix_b1_lineage_readiness = paste(
      "All lineage readiness gates must pass before this table is produced.",
      "Panel-variant counts show common two-wave Census-2001 districts under increasingly permissive but still reviewed mapping rules."
    ),
    appendix_b2_lineage_sensitivity = paste(
      "The three registered lineage variants are evaluated with the same preferred consumption first-stage specification.",
      "District counts and complete-IV rows are reported alongside the state-clustered excluded-instrument F and Montiel Olea-Pflueger effective F.",
      "This is a geography-sensitivity check: the full-reviewed allocation rule is not substituted for the primary analysis panel."
    ),
    appendix_b3_consumption_reconstruction = paste(
      "Reconstructed national MPCE is compared with the registered official benchmark for every supported survey-sector cell.",
      "Only benchmarks that satisfy the survey-specific absolute-rupee tolerance enter this final validation exhibit."
    ),
    appendix_b4_hces_consistency_summary = paste(
      "Correlations use the common set of preferred-eligible Census-2001 districts observed in both HCES 2022-23 and 2023-24 for each registered outcome.",
      "The companion scatterplots show the same district pairs and a 45-degree reference line."
    ),
    appendix_b6_language_source_validation = paste(
      "The table combines source coverage, exact official-Census reconciliation, Helms-Lim agreement with the project 1991 construction, and persistence from 1991 to 2001.",
      "The rows use their native validation metric rather than forcing unlike checks onto one statistical scale."
    ),
    appendix_b8_census_universe_reconciliation = paste(
      "Rows summarize the registered migration, housing, household, and worker denominator reconciliations before longitudinal pooling or rate construction.",
      "Exact cross-table count checks must have zero discrepancy; the migration-stock check separately verifies that migrant stock never exceeds the Census population denominator.",
      "Support reports the native published district universe or the overlap range when source tables have structurally different coverage."
    ),
    appendix_b9_dise_publication_validation = paste(
      "Published DISE report cells are compared with values reconstructed from the archived machine-readable source.",
      "All registered publication checks must match exactly before this exhibit can be produced."
    ),
    appendix_c1_full_absorption_ladder = paste(
      "Every declared semantic first-stage absorption specification is shown on the same complete-case district support.",
      "Excluded-instrument F and partial R-squared are conventional first-stage diagnostics; this table does not substitute them for the separately reported MOP effective-F diagnostic."
    ),
    appendix_c3_control_block_absorption = paste(
      "Each row compares one registered Census-2001 control block entered alone with the main-control specification omitting that block, separately under six-region and state fixed effects.",
      "The table asks whether one control family mechanically explains the within-state first-stage collapse."
    ),
    appendix_c4_geographic_scale_sensitivity = paste(
      "The added-control rows compare the same common-support first stage before and after the Shastry Hindi-belt indicator or 2001 child-population control.",
      "Leave-one-state-out and district-influence rows summarize the registered state-deletion and influence diagnostics from the state-FE expanded-control first stage.",
      "Cook's distance is undefined for leverage-one observations; those saturated fixed-effect cells are reported in the row context and omitted only from the Cook's-distance maximum, while DFBETA retains every finite district."
    ),
    appendix_c5_alternative_scalar_distances = paste(
      "All scalar linguistic-distance constructions are estimated on the common alternative-distance support under unadjusted, six-region-plus-controls, and state-plus-controls specifications.",
      "The table is a relevance comparison, not evidence that the alternative measures satisfy exclusion."
    ),
    appendix_c6_mapping_composition_sensitivity = paste(
      "Mapping-coverage thresholds, distance-4 leave-one-language-out checks, Shastry composition adjustments, adjudication bounds, and richer distance-share vectors are summarized without printing the full diagnostic grid.",
      "The Kashmiri row reports its share of distance-4 speakers; the remaining rows report first-stage strength under the registered sensitivity specification."
    ),
    appendix_c7_historical_balance = paste(
      "Joint tests use the preferred historical geography and compare eventual EMI with Census-2001 linguistic distance and the independent Helms-Lim 1991 distance across five predetermined baseline domains.",
      "This is a historical-balance diagnostic, not evidence that any predictor is randomly assigned."
    ),
    appendix_c9_historical_first_stage = paste(
      "The same validated historical district support is used for the 1991 and 2001 distance constructions within each specification.",
      "The comparison asks whether historical reconstruction rescues within-state relevance; it is not pooled with the larger modern first-stage sample."
    ),
    appendix_c11_multiple_instruments = paste(
      "Rows report the registered five-share language-vector specifications under state fixed effects and predetermined controls.",
      "Sargan tests are shown alongside falsification-adaptive sets and constituent conditional first-stage strength.",
      "A wide FAS containing zero or weak constituent instruments means richer language vectors do not repair identification even when the joint first stage is predictive."
    ),
    appendix_c13_robustness_family_census = paste(
      "The seven rows exhaust the predeclared consumption-IV robustness families; the final row reconciles their model counts to the realized robustness grid.",
      "Strong first stage compares each model's effective F with its registered critical value; RF and AR signals use family-adjusted p-values.",
      "This table summarizes the robustness census and does not select a preferred specification from it."
    ),
    appendix_c14_exclusion_sensitivity = paste(
      "Rows use the four registered long-run HCES exclusion-sensitivity designs under exact exclusion as the benchmark.",
      "The minimum direct effect is the smallest bounded violation needed for beta=0 to enter the 95% Anderson-Rubin inversion; its scale is also reported as a share of the absolute reduced form.",
      "A zero threshold means beta=0 is already admitted under exact exclusion. These are sensitivity diagnostics, not point estimates of a direct effect."
    ),
    appendix_d1_migration = "All 48 registered migration reduced-form models are shown: eight outcomes crossed with region/state adjustment and the three predeclared linguistic-distance constructions. Standard errors and p-values use the canonical state-clustered inference; Holm p-values are adjusted within each outcome/specification family. These are associations, not identified migration mechanisms.",
    appendix_d2_migration_context = "The national rows use the preferred state-plus-controls Shastry-distance specification. The Hindi-belt row is the predeclared restricted-sample skilled-migrant comparison. The exhibit distinguishes skill composition from interstate-migration quantity.",
    appendix_d3_housing_assets = "All 48 registered 2001-11 housing, finance, and durable-asset reduced forms are shown with canonical state-clustered inference and within-family Holm adjustment. Because the outcome window overlaps the 2007-08 schooling measure, these are co-evolving local-development margins rather than post-treatment EMI mediators.",
    appendix_d4_economic_census = "All 36 registered 2005-13 Economic Census reduced forms are shown with canonical state-clustered inference and within-family Holm adjustment. The window straddles schooling measurement; sectoral changes therefore describe the broader local opportunity environment rather than a causal schooling mechanism.",
    appendix_d5_labor = "The full 36-model labor family combines NSS 2009-10, PLFS 2017-18 primary lineage, and the predeclared conservative-lineage PLFS sensitivity. Each wave contains two outcomes, two geographic adjustments, and three linguistic-distance constructions; standard errors and p-values use the canonical state-clustered inference.",
    appendix_d6_household_capacity = "The eight bounded 2001-11 household-capacity models compare linguistic opportunity with observed all-child EMI across four capacity outcomes. State-clustered inference and predictor-family Holm adjustment are preserved from the canonical diagnostic object; the schooling rows are descriptive associations.",
    appendix_d7_social_heterogeneity = "The table combines the registered social-group-gap-by-distance family with the predeclared ST-concentration heterogeneity family. Holm-adjusted p-values are preserved; null interactions are retained because this appendix is intended to document bounded negative as well as positive heterogeneity evidence.",
    appendix_d9_residual_spatial_diagnostics = "Moran's I uses the preferred rook-contiguity, row-standardized Census-2001 district weights on the active IV sample. The first- and second-stage residual diagnostics test whether strong raw spatial concentration remains after the preferred specification; they do not constitute a spatial-IV estimator.",
    appendix_e1_selection_sample = paste(
      "NSS 64th-round child-level enrollment sample used by the descriptive selection model.",
      "Numeric rows report mean, standard deviation, and range; categorical rows report the modal category and observed levels.",
      "This exhibit establishes sample composition only and is not a causal adjustment set."
    ),
    appendix_e4_missingness = paste(
      "Missing percentages are computed on the selection-analysis data before model-specific complete-case restriction.",
      "Pseudo R2 and significant-predictor counts come from the registered missingness-logit screen when a variable is screened; blank cells denote variables outside that screen.",
      "These diagnostics describe structured missingness and do not impute missing values."
    ),
    paper_identification_boundary = paste(
      "Panel A compares excluded-instrument F statistics on one common district support for the unadjusted and state-fixed-effects plus predetermined-controls specifications; partial R-squared refers to the within-state specification.",
      "The language-distance composition is a joint test, while the other rows contain one excluded scalar instrument. Historical 1991 reconstructions use a much smaller validated geography and are kept in the appendix rather than mixing noncomparable first-stage support into this panel.",
      "Panel B reports conventional 2SLS coefficients for the registered 2004-05 to 2022-23 and 2023-24 long changes, alongside Montiel Olea-Pflueger effective F and Anderson-Rubin confidence-set topology.",
      "Disconnected AR sets that span both signs do not identify the sign of an EMI effect even when beta=0 is rejected on the finite grid. These diagnostics define an identification boundary, not preferred causal estimates."
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

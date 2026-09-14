# Final-paper Appendix A exhibit builders.
#
# Appendix A is prose-led. These helpers retain only the compact tables that
# summarize source families or measurement registries used by the manuscript;
# detailed lineage ledgers and validation files remain available as CSVs.

appendix_a3_lineage_source_hierarchy <- function(district_lineage) {
  if (!is.list(district_lineage)) {
    stop("District-lineage source summary requires registered district lineage output.", call. = FALSE)
  }
  registry <- safe_df(district_lineage$source_registry)
  if (!all(c("source_id", "citation") %in% names(registry)) || !nrow(registry)) {
    stop("District-lineage source summary requires the lineage source registry.", call. = FALSE)
  }

  source_families <- list(
    c("datameet_census_2001_districts", "census_registry_2001_2011_continuity",
      "nss64_education_district_codes", "nss75_official_district_list_census2011_exact"),
    c("lgd_districts", "lgd_mod_districts_2001_2011", "lgd_mod_districts"),
    "kumar_somanathan_2016",
    c("isded_1951_2024", "india_district_tracker"),
    c("shrug_pc_keys", "shrug_pc11_district_geometry"),
    c("concordance_plfs_nss", "concordance_census_plfs")
  )
  required_ids <- unique(unlist(source_families, use.names = FALSE))
  missing_ids <- setdiff(required_ids, plain_chr(registry$source_id))
  if (length(missing_ids)) {
    stop(
      "District-lineage source summary is missing registered evidence: ",
      paste(missing_ids, collapse = ", "),
      call. = FALSE
    )
  }

  csv <- data.frame(
    source_family = c(
      "Census and NSS district records",
      "Local Government Directory",
      "Kumar and Somanathan district histories",
      "India State Stories / District Changes Tracker",
      "SHRUG locality and district keys",
      "Published survey concordances"
    ),
    coverage = c(
      "Census 2001 and 2011; NSS 64th and 75th rounds",
      "2001 onward",
      "1961-2001",
      "1951 onward / 2001-2020",
      "Population Censuses 1991-2011",
      "Census, NSS, and PLFS district codes"
    ),
    role = c(
      "Define reference districts and resolve survey identities",
      "Verify district creation, renaming, and Census-code continuity",
      "Benchmark historical partitions and population transfers",
      "Cross-check district histories and predecessor relationships",
      "Measure locality-based overlap when later districts cross 2001 boundaries",
      "Cross-check survey-to-Census district links"
    ),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Source = csv$source_family, Coverage = csv$coverage, Role = csv$role,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_a6_linguistic_measures <- function() {
  csv <- data.frame(
    measure_id = c("shastry_2001", "historical_1991", "glottolog", "dyen", "distant_share"),
    measure = c(
      paper_linguistic_distance_display_labels()[["nonzero_mean"]],
      paper_linguistic_distance_display_labels()[["historical_1991"]],
      paper_linguistic_distance_display_labels()[["glottolog_mean"]],
      paper_linguistic_distance_display_labels()[["dyen_noncognate"]],
      paper_linguistic_distance_display_labels()[["distant_share"]]
    ),
    source = c(
      "Shastry concordance + Census 2001 C-16", "1991 Census language reconstruction",
      "Glottolog 5.3", "Dyen lexicostatistical source", "Census 2001 C-16 + Shastry distance bins"
    ),
    concept = c(
      "Speaker-weighted mean distance from Hindi among non-Hindi languages", "Predetermined analogue of preferred distance",
      "Genealogical path distance from Hindi", "Lexical noncognacy relative to Hindi", "Population share at distance >= 3"
    ),
    role = c("Preferred", "Historical validation", "Alternative", "Alternative", "Nonlinear alternative"),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Measure = csv$measure, Source = csv$source, Concept = csv$concept, Role = csv$role,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_a7_consumption_construction <- function(consumption_survey_registry) {
  x <- validate_consumption_survey_registry(safe_df(consumption_survey_registry))
  keep <- plain_chr(x$survey_family) == "nss_schedule_1_0" |
    grepl("hces", plain_chr(x$survey_id), fixed = TRUE)
  x <- x[keep, , drop = FALSE]
  if (!nrow(x)) stop("Appendix A7 requires registered consumption surveys.", call. = FALSE)
  csv <- data.frame(
    survey_id = plain_chr(x$survey_id),
    round = plain_chr(x$survey_label),
    period = paste(plain_chr(x$survey_start), plain_chr(x$survey_end), sep = " to "),
    recall_design = plain_chr(x$schedule_variant),
    price_treatment = plain_chr(x$price_timing),
    district_identity = plain_chr(x$district_identity_source),
    analysis_role = plain_chr(x$analysis_role),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Round = csv$round, Period = csv$period, `Recall / schedule` = csv$recall_design,
    `Price treatment` = csv$price_treatment, `District identity` = csv$district_identity,
    Role = csv$analysis_role, check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_data_construction_exhibits <- function(district_lineage, consumption_survey_registry) {
  list(
    appendix_a3_lineage_source_hierarchy = appendix_a3_lineage_source_hierarchy(district_lineage),
    appendix_a6_linguistic_measures = appendix_a6_linguistic_measures(),
    appendix_a7_consumption_construction = appendix_a7_consumption_construction(consumption_survey_registry)
  )
}

save_appendix_data_construction_exhibits <- function(exhibits, cfg) {
  save_appendix_tables(
    exhibits,
    c(
      "appendix_a3_lineage_source_hierarchy",
      "appendix_a6_linguistic_measures",
      "appendix_a7_consumption_construction"
    ),
    cfg
  )
}

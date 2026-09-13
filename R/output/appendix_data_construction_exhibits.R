# Final-paper Appendix A exhibit builders.
#
# Appendix A documents construction and provenance. These functions reshape
# canonical registries and lineage metadata; they do not fit models or duplicate
# upstream data-cleaning logic.

appendix_a1_source_registry <- function() {
  data.frame(
    source_id = c(
      "census_2001_mother_tongue", "census_2001_bilingualism",
      "nss_2007_education", "dise_district_report_cards",
      "nss_2004_05_consumption", "nss_2009_10_consumption_type1",
      "nss_2009_10_consumption_type2", "nss_2011_12_consumption_type2",
      "hces_2022_23_consumption", "hces_2023_24_consumption", "prices",
      "census_2001_migration_tables", "census_2011_migration_tables",
      "census_housing_living_standards", "census_2001_household_tables",
      "census_2011_household_tables", "economic_census_raw",
      "nss_2009_10_employment", "plfs_labor_market"
    ),
    years = c(
      "2001", "2001", "2007-08", "2005-06 to 2007-08",
      "2004-05", "2009-10", "2009-10", "2011-12",
      "2022-23", "2023-24", "Multiple years",
      "2001", "2011", "2001 and 2011", "2001", "2011",
      "2005 and 2013-14", "2009-10", "2017-18"
    ),
    geography = c(
      "Census-2001 district", "State x native language", "NSS district",
      "DISE district", rep("Survey district -> Census-2001 district", 6),
      "State x rural/urban price cell", "Census-2001 district", "Census-2011 district",
      "Census district -> Census-2001 district", "Census-2001 district",
      "Census-2011 district", "District -> Census-2001 district",
      "NSS district -> Census-2001 district", "PLFS district -> Census-2001 district"
    ),
    analytical_role = c(
      "Inherited linguistic structure", "Language behavior",
      "Schooling exposure and access", "Administrative schooling validation",
      "Baseline welfare", "Early welfare sensitivity", "Early welfare",
      "Medium-run welfare", "Long-run welfare", "Long-run welfare replication",
      "Temporal and spatial deflation", "Baseline migration structure",
      "Follow-up migration and skill composition", "Assets and financial inclusion",
      "Baseline household capacity", "Follow-up household capacity",
      "Scale and sectoral structure", "Labor-market reference", "Long-run labor outcomes"
    ),
    stringsAsFactors = FALSE
  )
}

appendix_a1_data_source_timing <- function(data_sources) {
  sources <- safe_df(data_sources)
  required <- c("source_id", "source_name", "used_in_current_pipeline")
  missing <- setdiff(required, names(sources))
  if (length(missing)) {
    stop("Appendix A1 source registry lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  registry <- appendix_a1_source_registry()
  idx <- match(registry$source_id, plain_chr(sources$source_id))
  if (anyNA(idx)) {
    stop("Appendix A1 references source IDs absent from data_sources.csv: ",
         paste(registry$source_id[is.na(idx)], collapse = ", "), call. = FALSE)
  }
  selected <- sources[idx, , drop = FALSE]
  if (any(!as.logical(selected$used_in_current_pipeline))) {
    stop("Appendix A1 may only describe current-pipeline sources.", call. = FALSE)
  }
  csv <- data.frame(
    source_id = registry$source_id,
    dataset = plain_chr(selected$source_name),
    years = registry$years,
    geography = registry$geography,
    analytical_role = registry$analytical_role,
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Dataset = csv$dataset, Years = csv$years, `Geographic unit` = csv$geography,
    `Analytical role` = csv$analytical_role, check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_a2_lineage_logic_data <- function(district_lineage) {
  if (!is.list(district_lineage)) stop("Appendix A2 requires canonical district lineage output.", call. = FALSE)
  sensitivity <- safe_df(district_lineage$mapping_rule_sensitivity)
  required <- c("rule", "source_rows_2007_08", "source_rows_2017_18", "two_wave_target_districts")
  if (length(setdiff(required, names(sensitivity)))) {
    stop("Appendix A2 requires lineage mapping-rule sensitivity metadata.", call. = FALSE)
  }
  primary <- sensitivity[plain_chr(sensitivity$rule) == "primary", , drop = FALSE]
  if (nrow(primary) != 1L) stop("Appendix A2 requires exactly one primary lineage rule.", call. = FALSE)
  data.frame(
    stage = c("Observed survey identity", "Deterministic lineage rule", "Census-2001 analytical district"),
    detail = c(
      sprintf("%s NSS64 +\n%s NSS75 source identities", primary$source_rows_2007_08, primary$source_rows_2017_18),
      "Exact identity / accepted nested split /\npopulation-weighted allocation /\nexclude unresolved nonnested cases",
      sprintf("%s common two-wave districts\nunder the primary rule", primary$two_wave_target_districts)
    ),
    x = 1:3,
    stringsAsFactors = FALSE
  )
}

appendix_a2_lineage_logic_plot <- function(district_lineage) {
  need_pkg("ggplot2", "Appendix A lineage diagram")
  d <- appendix_a2_lineage_logic_data(district_lineage)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = 1)) +
    ggplot2::geom_segment(
      data = data.frame(x = c(1.34, 2.34), xend = c(1.66, 2.66), y = 1, yend = 1),
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      arrow = grid::arrow(length = grid::unit(0.16, "inches")), linewidth = 0.5
    ) +
    ggplot2::geom_label(
      ggplot2::aes(label = stage), size = 3.6, fontface = "bold",
      linewidth = 0.25, label.padding = grid::unit(0.28, "lines")
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = 0.82, label = detail), size = 3.0, lineheight = 0.95
    ) +
    ggplot2::coord_cartesian(xlim = c(0.55, 3.45), ylim = c(0.62, 1.17), clip = "off") +
    ggplot2::labs(
      title = "Appendix A2. District-lineage logic",
      subtitle = "Only deterministic, adjudicated mappings enter the primary Census-2001 analytical geography"
    ) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      plot.margin = ggplot2::margin(10, 18, 12, 18)
    )
  attr(p, "csv_data") <- d[c("stage", "detail")]
  p
}

appendix_lineage_evidence_tier <- function(role) {
  role <- plain_chr(role)
  qa_roles <- c(
    "adjudication", "allocation_adjudication", "event_adjudication",
    "geometry_adjudication", "primary_review", "source_registry"
  )
  official_roles <- c(
    "current_registry", "current_component_registry", "current_urban_registry",
    "urban_component_registry", "component_registry", "official_census_code_bridge_2001_2011",
    "census_2011_geometry", "production_census_2001_geometry", "census_locality_attributes",
    "changed_unit_roster_2011_2018", "post_2018_validation"
  )
  out <- rep("Independent concordance / geometry", length(role))
  out[role %in% official_roles] <- "Official administrative source"
  out[role %in% qa_roles] <- "QA / adjudication evidence"
  out
}

appendix_a3_lineage_source_hierarchy <- function(district_lineage) {
  if (!is.list(district_lineage)) stop("Appendix A3 requires canonical district lineage output.", call. = FALSE)
  registry <- safe_df(district_lineage$source_registry)
  inventory <- safe_df(district_lineage$source_inventory)
  if (!all(c("source_id", "citation") %in% names(registry)) || !nrow(registry) ||
      !all(c("source_id", "role") %in% names(inventory)) || !nrow(inventory)) {
    stop("Appendix A3 requires the lineage source registry and inventory.", call. = FALSE)
  }
  ids <- intersect(plain_chr(inventory$source_id), plain_chr(registry$source_id))
  if (!length(ids)) stop("Appendix A3 found no registered lineage evidence sources.", call. = FALSE)
  inv <- inventory[match(ids, plain_chr(inventory$source_id)), , drop = FALSE]
  reg <- registry[match(ids, plain_chr(registry$source_id)), , drop = FALSE]
  csv <- data.frame(
    tier = appendix_lineage_evidence_tier(inv$role),
    source_id = ids,
    source = plain_chr(reg$citation),
    role = plain_chr(inv$role),
    stringsAsFactors = FALSE
  )
  tier_order <- c(
    "Official administrative source", "Independent concordance / geometry", "QA / adjudication evidence"
  )
  csv <- csv[order(match(csv$tier, tier_order), csv$source_id), , drop = FALSE]
  out <- data.frame(
    `Evidence tier` = csv$tier, Source = csv$source, Role = csv$role,
    `Registry ID` = csv$source_id, check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_a4_nss_schooling_constructs <- function() {
  csv <- data.frame(
    construct_id = c("enrollment", "emi_enrolled", "emi_all_children", "public_emi", "private_emi"),
    variable = c(
      "enrollment_rate_0708", "emi_share_enrolled_0708", "emi_exposure_all_children_0708",
      "public_emi_exposure_all_children_0708", "private_emi_exposure_all_children_0708"
    ),
    numerator = c(
      "Enrolled children", "English-medium enrolled children", "English-medium enrolled children",
      "Public English-medium enrolled children", "Private English-medium enrolled children"
    ),
    denominator = c(
      "Eligible children age 5-19", "Enrolled children with known medium", "Eligible children age 5-19",
      "Eligible children age 5-19", "Eligible children age 5-19"
    ),
    interpretation = c(
      "Extensive schooling participation", "Medium conditional on schooling", "Population exposure to EMI",
      "Public-sector EMI exposure", "Private-sector EMI exposure"
    ),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Construct = csv$interpretation, Numerator = csv$numerator,
    Denominator = csv$denominator, `Canonical variable` = csv$variable,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_a5_dise_construction <- function() {
  x <- dise_construct_registry()
  required <- c("construct_id", "variable", "margin", "source_side", "paper_role", "label")
  if (length(setdiff(required, names(x))) || nrow(x) != 8L) {
    stop("Appendix A5 requires the canonical eight-row DISE construct registry.", call. = FALSE)
  }
  csv <- x[required]
  out <- data.frame(
    Construct = csv$label, Margin = csv$margin, `Source side` = csv$source_side,
    `Paper role` = csv$paper_role, check.names = FALSE, stringsAsFactors = FALSE
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

appendix_a8_other_outcome_panels <- function() {
  csv <- data.frame(
    panel_id = c("census_migration", "census_housing_households", "economic_census", "nss66_plfs"),
    panel = c("Census migration", "Census housing and households", "Economic Census", "NSS66 / PLFS labor"),
    timing = c("2001 and 2011", "2001 and 2011", "2005 and 2013-14", "2009-10 and 2017-18"),
    universe = c(
      "Migrants / recent work migrants", "Households and household members", "Nonfarm establishments and employment", "Working-age survey respondents"
    ),
    analytical_role = c(
      "Migration quantity and skill composition", "Assets, finance, and household capacity",
      "Scale and sectoral composition", "LFPR and employment"
    ),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Panel = csv$panel, Timing = csv$timing, Universe = csv$universe,
    `Analytical role` = csv$analytical_role, check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_data_construction_exhibits <- function(data_sources, district_lineage, consumption_survey_registry) {
  list(
    appendix_a1_data_source_timing = appendix_a1_data_source_timing(data_sources),
    appendix_a2_lineage_logic = appendix_a2_lineage_logic_plot(district_lineage),
    appendix_a3_lineage_source_hierarchy = appendix_a3_lineage_source_hierarchy(district_lineage),
    appendix_a4_nss_schooling_constructs = appendix_a4_nss_schooling_constructs(),
    appendix_a5_dise_construction = appendix_a5_dise_construction(),
    appendix_a6_linguistic_measures = appendix_a6_linguistic_measures(),
    appendix_a7_consumption_construction = appendix_a7_consumption_construction(consumption_survey_registry),
    appendix_a8_other_outcome_panels = appendix_a8_other_outcome_panels()
  )
}

save_appendix_data_construction_exhibits <- function(exhibits, cfg) {
  table_names <- c(
    "appendix_a1_data_source_timing", "appendix_a3_lineage_source_hierarchy",
    "appendix_a4_nss_schooling_constructs", "appendix_a5_dise_construction",
    "appendix_a6_linguistic_measures", "appendix_a7_consumption_construction",
    "appendix_a8_other_outcome_panels"
  )
  written <- save_appendix_tables(exhibits, table_names, cfg)
  if (!"appendix_a2_lineage_logic" %in% names(exhibits) || !inherits(exhibits$appendix_a2_lineage_logic, "ggplot")) {
    stop("Appendix A exhibit bundle is missing the lineage diagram.", call. = FALSE)
  }
  formats <- figure_formats(cfg)
  path_base <- appendix_figure_path_base("appendix_a2_lineage_logic")
  dir.create(dirname(path_base), recursive = TRUE, showWarnings = FALSE)
  written <- c(written, save_plot_formats(exhibits$appendix_a2_lineage_logic, path_base, formats, width = 8.2, height = 3.2))
  unique(normalizePath(written, mustWork = FALSE))
}

# Source discovery and explicit readers for the district-lineage v2 diagnostic.
# This pipeline runs in parallel with the reviewed production crosswalk until
# the new source-match and administrative-event ledgers are fully adjudicated.

#' District-lineage v2 input specification
#'
#' @return Data frame describing supported raw and metadata inputs. Missing
#'   optional files are reported in diagnostics rather than failing the public
#'   production pipeline.
district_lineage_v2_input_specs <- function(paths = build_paths()) {
  spec <- function(source_id, relative_path, reader, load_for_diagnostic, role) {
    data.frame(
      source_id = source_id,
      relative_path = relative_path,
      reader = reader,
      load_for_diagnostic = load_for_diagnostic,
      role = role,
      stringsAsFactors = FALSE
    )
  }
  rows <- list(
    spec("lgd_states", "data/raw/local_government_directory/states.json", "lgd_json", TRUE, "current_registry"),
    spec("lgd_districts", "data/raw/local_government_directory/districts.json", "lgd_json", TRUE, "current_registry"),
    spec("lgd_subdistricts", "data/raw/local_government_directory/subdistricts.json", "lgd_json", TRUE, "current_registry"),
    spec("lgd_villages", "data/raw/local_government_directory/villages.xlsx", "lgd_xlsx", FALSE, "current_component_registry"),
    spec("lgd_urban_local_bodies", "data/raw/local_government_directory/urbanLocalBody.xlsx", "lgd_xlsx", TRUE, "current_urban_registry"),
    spec("lgd_urban_coverage", "data/raw/local_government_directory/urbanLocalBody-coverage.xlsx", "lgd_xlsx", TRUE, "urban_component_registry"),
    spec("lgd_village_categories", "data/raw/local_government_directory/villages-category-urbanLocalBody.xlsx", "lgd_xlsx", FALSE, "urban_component_registry"),
    spec("lgd_development_blocks", "data/raw/local_government_directory/developmentBlocks-coveredVillages.xlsx", "lgd_xlsx", FALSE, "component_registry"),
    spec("lgd_mod_districts", "data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/districts.xls", "spreadsheetml", TRUE, "changed_unit_roster_2011_2018"),
    spec("lgd_mod_subdistricts", "data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/subdistricts.xls", "spreadsheetml", TRUE, "changed_unit_roster_2011_2018"),
    spec("lgd_mod_villages", "data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/villages.xls", "spreadsheetml", TRUE, "changed_unit_roster_2011_2018"),
    spec("lgd_mod_urban_local_bodies", "data/raw/local_government_directory/modifications_01-01-2011_30-06-2018/urbanLocalBody.xls", "spreadsheetml", TRUE, "changed_unit_roster_2011_2018"),
    spec("isded_1951_2024", "data/raw/district_changes/india_state_stories/isded/1951-2024/district_proliferation_1951_2024.xlsx", "xlsx", TRUE, "candidate_lineage"),
    spec("isded_admin_units_2025", "data/raw/district_changes/india_state_stories/isded/2025/admin_units_2025.xlsx", "xlsx", TRUE, "published_current_component_registry"),
    spec("iss_census_series_1901_2011", "data/raw/district_changes/india_state_stories/census_data_collection/1901-2011/1901-2011-State Districts-Population Time Series.xlsx", "inventory_only", FALSE, "historical_population_validation"),
    spec("iss_subdistricts_2026", "data/raw/district_changes/india_state_stories/census_data_collection/2026/2026_subdistricts_with_2011_census_pass2_loose.xlsx", "inventory_only", FALSE, "published_current_component_registry"),
    spec("shrug_pc01r", "data/raw/shrug/shrug-pc-keys-csv/pc01r_shrid_key.csv", "csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc01u", "data/raw/shrug/shrug-pc-keys-csv/pc01u_shrid_key.csv", "csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc11r", "data/raw/shrug/shrug-pc-keys-csv/pc11r_shrid_key.csv", "csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc11u", "data/raw/shrug/shrug-pc-keys-csv/pc11u_shrid_key.csv", "csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc01dist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc01dist_key.csv", "csv", TRUE, "stable_locality_district_membership"),
    spec("shrug_pc11dist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11dist_key.csv", "csv", TRUE, "stable_locality_district_membership"),
    spec("shrug_pc01subdist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc01subdist_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11subdist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdist_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11subdistu", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdistu_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11_district_geometry", "data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg", "gpkg", TRUE, "census_2011_geometry"),
    spec("shrug_pc11_subdistrict_geometry", "data/raw/shrug/open-polygons/shrug-pc11subdist-poly-gpkg/subdistrict.gpkg", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_pc11_state_geometry", "data/raw/shrug/open-polygons/shrug-pc11state-poly-gpkg/state.gpkg", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_pc11_village_geometry_zip", "data/raw/shrug/open-polygons/shrug-pc11-village-poly-gpkg.zip", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_shrid_geometry_zip", "data/raw/shrug/open-polygons/shrug-shrid-poly-gpkg.zip", "inventory_only", FALSE, "future_2001_geometry"),
    spec("shrug_pca01_zip", "data/raw/shrug/census_2001/shrug-pca01-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("shrug_pca11_zip", "data/raw/shrug/census_2011/shrug-pca11-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("shrug_td01_zip", "data/raw/shrug/census_2001/shrug-td01-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("shrug_td11_zip", "data/raw/shrug/census_2011/shrug-td11-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("shrug_vd01_zip", "data/raw/shrug/census_2001/shrug-vd01-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("shrug_vd11_zip", "data/raw/shrug/census_2011/shrug-vd11-csv.zip", "inventory_only", FALSE, "census_locality_attributes"),
    spec("ipums_geo2_1987_2009", "data/raw/ipums/geo2_in1987_2009/geo2_in1987_2009.shp", "inventory_only", FALSE, "stable_geography_sensitivity"),
    spec("concordance_plfs_nss", "data/raw/concordance/plfs_nss_distcodes.csv", "csv", TRUE, "published_concordance"),
    spec("concordance_census_plfs", "data/raw/concordance/census_plfs_distcodes.csv", "csv", TRUE, "published_concordance"),
    spec("concordance_nrlm_plfs", "data/raw/concordance/nrlm_plfs_distcodes.csv", "csv", TRUE, "published_concordance"),
    spec("concordance_telangana", "data/raw/concordance/telangana_plfs_districts.csv", "csv", TRUE, "published_concordance"),
    spec("concordance_census_region", "data/raw/concordance/census_region.csv", "csv", TRUE, "published_concordance"),
    spec("lineage_gold", "data/metadata/district_match_gold.csv", "csv", TRUE, "calibration"),
    spec("lineage_adjudications", "data/metadata/district_adjudications_v2.csv", "csv", TRUE, "adjudication"),
    spec("lineage_events", "data/metadata/district_admin_events_v2.csv", "csv", TRUE, "event_adjudication"),
    spec("lineage_allocation_weights", "data/metadata/district_allocation_weights_v2.csv", "csv", TRUE, "allocation_adjudication"),
    spec("lineage_sources", "data/metadata/district_sources_v2.csv", "csv", TRUE, "source_registry")
  )
  out <- do.call(rbind, rows)
  out$absolute_path <- path_project(paths, out$relative_path)
  out$exists <- file.exists(out$absolute_path)
  out$size_bytes <- ifelse(out$exists, file.info(out$absolute_path)$size, NA_real_)
  rownames(out) <- NULL
  out
}

#' Existing district-lineage input files
#'
#' @return Character vector suitable for a targets `format = "file"` target.
district_lineage_v2_existing_files <- function(specs) {
  specs <- safe_df(specs)
  unique(specs$absolute_path[specs$exists & specs$load_for_diagnostic])
}

read_lgd_json_records <- function(path) {
  need_pkg("jsonlite", "LGD JSON inputs")
  x <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  out <- x$records %||% data.frame()
  safe_df(out)
}

clean_source_names <- function(x) {
  names(x) <- make.unique(gsub("_+$", "", canon(names(x))))
  x
}

read_lgd_xlsx_table <- function(path) {
  need_pkg("readxl", "LGD Excel inputs")
  out <- readxl::read_excel(path, skip = 1, .name_repair = "minimal")
  out <- clean_source_names(safe_df(out))
  drop_export_footer(out)
}

spreadsheetml_rows <- function(path) {
  need_pkg("xml2", "LGD SpreadsheetML modification reports")
  doc <- xml2::read_xml(path)
  rows <- xml2::xml_find_all(doc, ".//*[local-name()='Row']")
  lapply(rows, function(row) {
    cells <- xml2::xml_find_all(row, "./*[local-name()='Cell']")
    values <- character()
    cursor <- 1L
    for (cell in cells) {
      attrs <- xml2::xml_attrs(cell)
      index_attr <- attrs[grepl("(^|:)Index$", names(attrs))]
      idx <- if (length(index_attr)) suppressWarnings(as.integer(index_attr[[1]])) else NA_integer_
      if (is.finite(idx)) cursor <- idx
      if (cursor > length(values) + 1L) values <- c(values, rep("", cursor - length(values) - 1L))
      data <- xml2::xml_find_first(cell, "./*[local-name()='Data']")
      value <- if (inherits(data, "xml_missing")) "" else xml2::xml_text(data)
      values[[cursor]] <- value
      cursor <- cursor + 1L
    }
    values
  })
}

read_lgd_spreadsheetml <- function(path) {
  rows <- spreadsheetml_rows(path)
  if (!length(rows)) return(data.frame())
  header_i <- which(vapply(rows, function(x) {
    vals <- canon(x)
    sum(nzchar(vals)) >= 3L && any(grepl("code", vals, fixed = TRUE)) && any(grepl("name", vals, fixed = TRUE))
  }, logical(1)))[1]
  if (!is.finite(header_i)) stop("Could not locate the data header in LGD modification report: ", path, call. = FALSE)

  width <- max(vapply(rows[header_i:length(rows)], length, integer(1)))
  pad <- function(x) c(x, rep("", width - length(x)))
  matrix_rows <- do.call(rbind, lapply(rows[header_i:length(rows)], pad))
  header <- make.unique(canon(matrix_rows[1, ]))
  out <- as.data.frame(matrix_rows[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(out) <- header
  out <- out[, nzchar(names(out)), drop = FALSE]
  drop_export_footer(out)
}

drop_export_footer <- function(x) {
  x <- safe_df(x)
  if (!nrow(x)) return(x)
  row_text <- apply(x, 1, function(z) paste(canon(z), collapse = " "))
  keep <- nzchar(trimws(row_text)) &
    !grepl("^total ", row_text) &
    !grepl("report generated", row_text, fixed = TRUE) &
    !grepl("local government directory$", row_text)
  x[keep, , drop = FALSE]
}

read_lineage_csv <- function(path) {
  need_pkg("readr", "district-lineage CSV inputs")
  safe_df(readr::read_csv(path, show_col_types = FALSE, progress = FALSE, name_repair = "minimal"))
}

read_lineage_source <- function(path, reader) {
  switch(
    reader,
    lgd_json = read_lgd_json_records(path),
    lgd_xlsx = read_lgd_xlsx_table(path),
    xlsx = {
      need_pkg("readxl", "district-lineage Excel inputs")
      safe_df(readxl::read_excel(path, .name_repair = "minimal"))
    },
    spreadsheetml = read_lgd_spreadsheetml(path),
    csv = read_lineage_csv(path),
    gpkg = {
      need_pkg("sf", "district-lineage geometries")
      sf::st_read(path, quiet = TRUE)
    },
    inventory_only = path,
    stop("Unknown district-lineage reader: ", reader, call. = FALSE)
  )
}

#' Read district-lineage v2 inputs
#'
#' Large locality attributes and SHRID geometry are deliberately inventoried
#' without loading them into every extended-diagnostics run. Dedicated bridge
#' and geometry functions consume them when needed.
read_district_lineage_v2_sources <- function(specs, existing_files = character()) {
  if (length(existing_files) && any(!file.exists(existing_files))) {
    stop("A tracked district-lineage input disappeared before it could be read.", call. = FALSE)
  }
  specs <- safe_df(specs)
  available <- specs[specs$exists & specs$load_for_diagnostic, , drop = FALSE]
  values <- stats::setNames(lapply(seq_len(nrow(available)), function(i) {
    read_lineage_source(available$absolute_path[[i]], available$reader[[i]])
  }), available$source_id)
  attr(values, "source_inventory") <- specs[c("source_id", "relative_path", "reader", "role", "load_for_diagnostic", "exists", "size_bytes")]
  values
}

#' Standardize a current LGD registry table
standardize_lgd_registry <- function(x, level) {
  x <- clean_source_names(safe_df(x))
  level <- match.arg(level, c("state", "district", "subdistrict"))
  state_code <- first_col(x, c("state_code", "state lgd code"))
  state_name <- first_col(x, c("state_name_english", "state_name", "state name in english", "state name"))
  district_code <- first_col(x, c("district_code", "district lgd code"))
  district_name <- first_col(x, c("district_name_english", "district_name", "district name in english", "district name"))
  subdistrict_code <- first_col(x, c("subdistrict_code", "sub_district_code", "sub district code"))
  subdistrict_name <- first_col(x, c("subdistrict_name_english", "subdistrict_name", "sub_district_name", "sub district name"))
  census_state <- first_col(x, c("state_census2011_code", "state census2011 code", "census 2011 state code"))
  census_district <- first_col(x, c("district_census2011_code", "district census2011 code", "census 2011 district code"))
  census_subdistrict <- first_col(x, c("subdistrict_census2011_code", "sub district census2011 code", "census 2011 subdistrict code"))

  n <- nrow(x)
  missing_chr <- function() rep(NA_character_, n)
  data.frame(
    level = rep(level, n),
    state_lgd_code = if (!is.null(state_code)) plain_chr(x[[state_code]]) else missing_chr(),
    state_name = if (!is.null(state_name)) plain_chr(x[[state_name]]) else missing_chr(),
    district_lgd_code = if (!is.null(district_code)) plain_chr(x[[district_code]]) else missing_chr(),
    district_name = if (!is.null(district_name)) plain_chr(x[[district_name]]) else missing_chr(),
    subdistrict_lgd_code = if (!is.null(subdistrict_code)) plain_chr(x[[subdistrict_code]]) else missing_chr(),
    subdistrict_name = if (!is.null(subdistrict_name)) plain_chr(x[[subdistrict_name]]) else missing_chr(),
    census2011_state_code = if (!is.null(census_state)) plain_chr(x[[census_state]]) else missing_chr(),
    census2011_district_code = if (!is.null(census_district)) plain_chr(x[[census_district]]) else missing_chr(),
    census2011_subdistrict_code = if (!is.null(census_subdistrict)) plain_chr(x[[census_subdistrict]]) else missing_chr(),
    stringsAsFactors = FALSE
  )
}

standardize_iss_admin_units_2025 <- function(x) {
  x <- clean_source_names(safe_df(x))
  required <- c("state", "district", "subdistrict")
  if (!all(required %in% names(x))) return(data.frame())
  unique(data.frame(
    level = "subdistrict",
    state_lgd_code = NA_character_,
    state_name = plain_chr(x$state),
    district_lgd_code = NA_character_,
    district_name = plain_chr(x$district),
    entity_code = NA_character_,
    entity_name = plain_chr(x$subdistrict),
    census2011_code = NA_character_,
    source_id = "isded_admin_units_2025",
    stringsAsFactors = FALSE
  ))
}

standardize_lgd_urban_local_bodies <- function(x) {
  x <- clean_source_names(safe_df(x))
  n <- nrow(x)
  value <- function(candidates) {
    col <- first_col(x, candidates)
    if (is.null(col)) rep(NA_character_, n) else plain_chr(x[[col]])
  }
  data.frame(
    level = rep("urban_local_body", n),
    state_lgd_code = value(c("state code", "state_code")),
    state_name = value(c("state name", "state name in english", "state_name")),
    entity_code = value(c("local body code", "localbody code", "local_body_code")),
    entity_name = value(c("local body name in english", "localbody name in english", "local_body_name")),
    entity_version = value(c("local body version", "localbody version", "local_body_version")),
    entity_type_code = value(c("localbody type code", "local body type code")),
    census2011_code = value(c("census 2011 code", "census2011_code")),
    stringsAsFactors = FALSE
  )
}

standardize_lgd_urban_coverage <- function(x) {
  x <- clean_source_names(safe_df(x))
  n <- nrow(x)
  value <- function(candidates) {
    col <- first_col(x, candidates)
    if (is.null(col)) rep(NA_character_, n) else plain_chr(x[[col]])
  }
  out <- data.frame(
    urban_local_body_code = value(c("local body code", "localbody code", "local_body_code")),
    urban_local_body_name = value(c("local body name in english", "localbody name in english", "local_body_name")),
    census2011_urban_code = value(c("census 2011 code", "census2011_code")),
    state_name = value(c("state name in english", "state name", "state_name")),
    district_lgd_code = value(c("district code", "district_code")),
    district_name = value(c("district name in english", "district name", "district_name")),
    subdistrict_lgd_code = value(c("subdistrict code", "sub district code", "subdistrict_code")),
    subdistrict_name = value(c("subdistrict name in english", "sub district name", "subdistrict_name")),
    village_lgd_code = value(c("village code", "village_code")),
    village_name = value(c("village name in english", "village name", "village_name")),
    stringsAsFactors = FALSE
  )
  unique(out[!is.na(out$urban_local_body_code) & nzchar(out$urban_local_body_code), , drop = FALSE])
}

standardize_lgd_modification_roster <- function(x, level, period_start = "2011-01-01", period_end = "2018-06-30") {
  x <- clean_source_names(safe_df(x))
  level <- match.arg(level, c("district", "subdistrict", "village", "urban_local_body"))
  state_code <- first_col(x, c("state code", "state_code"))
  state_name <- first_col(x, c("state name in english", "state name", "state_name_english"))
  district_code <- first_col(x, c("district code", "district_code"))
  district_name <- first_col(x, c("district name in english", "district name", "district_name_english"))
  subdistrict_code <- first_col(x, c("sub district code", "subdistrict code", "subdistrict_code"))
  subdistrict_name <- first_col(x, c("sub district name", "subdistrict name", "subdistrict_name"))
  entity_code <- switch(
    level,
    district = district_code,
    subdistrict = subdistrict_code,
    village = first_col(x, c("village code", "village_code")),
    urban_local_body = first_col(x, c("local body code", "localbody code", "local_body_code"))
  )
  entity_name <- switch(
    level,
    district = district_name,
    subdistrict = subdistrict_name,
    village = first_col(x, c("village name in english", "village name", "village_name")),
    urban_local_body = first_col(x, c("local body name in english", "localbody name in english", "local body name", "local_body_name"))
  )
  n <- nrow(x)
  missing_chr <- function() rep(NA_character_, n)
  out <- data.frame(
    level = rep(level, n),
    entity_code = if (!is.null(entity_code)) plain_chr(x[[entity_code]]) else missing_chr(),
    entity_name = if (!is.null(entity_name)) plain_chr(x[[entity_name]]) else missing_chr(),
    state_lgd_code = if (!is.null(state_code)) plain_chr(x[[state_code]]) else missing_chr(),
    state_name = if (!is.null(state_name)) plain_chr(x[[state_name]]) else missing_chr(),
    district_lgd_code = if (!is.null(district_code)) plain_chr(x[[district_code]]) else missing_chr(),
    district_name = if (!is.null(district_name)) plain_chr(x[[district_name]]) else missing_chr(),
    subdistrict_lgd_code = if (!is.null(subdistrict_code)) plain_chr(x[[subdistrict_code]]) else missing_chr(),
    subdistrict_name = if (!is.null(subdistrict_name)) plain_chr(x[[subdistrict_name]]) else missing_chr(),
    period_start = rep(period_start, n),
    period_end = rep(period_end, n),
    event_type = rep("unknown_modification", n),
    evidence_status = rep("changed_unit_roster_only", n),
    stringsAsFactors = FALSE
  )
  unique(out[!is.na(out$entity_code) & nzchar(out$entity_code), , drop = FALSE])
}

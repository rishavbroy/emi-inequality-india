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
    spec("lgd_changes_post_2018", "data/raw/local_government_directory/changes.csv", "inventory_only", FALSE, "post_2018_validation"),
    spec("isded_1951_2024", "data/raw/district_changes/india_state_stories/isded/1951-2024/district_proliferation_1951_2024.xlsx", "xlsx", TRUE, "candidate_lineage"),
    spec("isded_admin_units_2025", "data/raw/district_changes/india_state_stories/isded/2025/admin_units_2025.xlsx", "xlsx", TRUE, "published_current_component_registry"),
    spec("iss_census_series_1901_2011", "data/raw/district_changes/india_state_stories/census_data_collection/1901-2011/1901-2011-State Districts-Population Time Series.xlsx", "inventory_only", FALSE, "historical_population_validation"),
    spec("iss_subdistricts_2026", "data/raw/district_changes/india_state_stories/census_data_collection/2026/2026_subdistricts_with_2011_census_pass2_loose.xlsx", "inventory_only", FALSE, "published_current_component_registry"),
    spec("shrug_pc01r", "data/raw/shrug/shrug-pc-keys-csv/pc01r_shrid_key.csv", "shrug_locality_csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc01u", "data/raw/shrug/shrug-pc-keys-csv/pc01u_shrid_key.csv", "shrug_locality_csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc11r", "data/raw/shrug/shrug-pc-keys-csv/pc11r_shrid_key.csv", "shrug_locality_csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc11u", "data/raw/shrug/shrug-pc-keys-csv/pc11u_shrid_key.csv", "shrug_locality_csv", TRUE, "stable_locality_weight"),
    spec("shrug_pc01dist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc01dist_key.csv", "shrug_district_csv", TRUE, "stable_locality_district_membership"),
    spec("shrug_pc11dist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11dist_key.csv", "shrug_district_csv", TRUE, "stable_locality_district_membership"),
    spec("shrug_pc01subdist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc01subdist_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11subdist", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdist_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11subdistu", "data/raw/shrug/shrug-pc-keys-csv/shrid_pc11subdistu_key.csv", "inventory_only", FALSE, "stable_locality_subdistrict_membership"),
    spec("shrug_pc11_district_geometry", "data/raw/shrug/open-polygons/shrug-pc11dist-poly-gpkg/district.gpkg", "gpkg", TRUE, "census_2011_geometry"),
    spec("shrug_pc11_subdistrict_geometry", "data/raw/shrug/open-polygons/shrug-pc11subdist-poly-gpkg/subdistrict.gpkg", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_pc11_state_geometry", "data/raw/shrug/open-polygons/shrug-pc11state-poly-gpkg/state.gpkg", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_pc11_village_geometry_zip", "data/raw/shrug/open-polygons/shrug-pc11-village-poly-gpkg.zip", "inventory_only", FALSE, "census_2011_geometry"),
    spec("shrug_shrid_geometry_zip", "data/raw/shrug/open-polygons/shrug-shrid-poly-gpkg.zip", "inventory_only", FALSE, "future_2001_geometry"),
    spec("lineage_geometry_2001", "outputs/derived/district_lineage_v2/district_2001.gpkg", "gpkg", TRUE, "derived_2001_geometry"),
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
    spec("lineage_geometry_carrybacks", "data/metadata/district_geometry_carrybacks_v2.csv", "csv", TRUE, "geometry_adjudication"),
    spec("lineage_sources", "data/metadata/district_sources_v2.csv", "csv", TRUE, "source_registry")
  )
  out <- do.call(rbind, rows)
  out$absolute_path <- path_project(paths, out$relative_path)
  out$exists <- file.exists(out$absolute_path)
  out$size_bytes <- ifelse(out$exists, file.info(out$absolute_path)$size, NA_real_)
  rownames(out) <- NULL
  out
}

#' District-lineage source inventory
#'
#' @return Compact inventory retained independently from loaded source values.
district_lineage_v2_source_inventory <- function(specs) {
  specs <- safe_df(specs)
  specs[c(
    "source_id", "relative_path", "reader", "role",
    "load_for_diagnostic", "exists", "size_bytes"
  )]
}

#' Split available source specifications for dynamic branching
#'
#' @return List of one-row data frames, one per source loaded by the extended diagnostic.
split_district_lineage_v2_source_specs <- function(specs) {
  specs <- safe_df(specs)
  specs <- specs[specs$exists & specs$load_for_diagnostic, , drop = FALSE]
  lapply(seq_len(nrow(specs)), function(i) specs[i, , drop = FALSE])
}

district_lineage_v2_source_path <- function(spec) {
  spec <- safe_df(spec)
  if (nrow(spec) != 1L || is.na(spec$absolute_path[[1]]) || !nzchar(spec$absolute_path[[1]])) {
    stop("A district-lineage source branch must contain exactly one path.", call. = FALSE)
  }
  spec$absolute_path[[1]]
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

spreadsheetml_index <- function(attrs) {
  if (!length(attrs)) return(NA_integer_)
  hit <- attrs[grepl("(^|:)Index$", names(attrs))]
  if (!length(hit)) return(NA_integer_)
  suppressWarnings(as.integer(hit[[1]]))
}

spreadsheetml_handlers <- function() {
  state <- new.env(parent = emptyenv())
  state$in_row <- FALSE
  state$in_data <- FALSE
  state$cursor <- 1L
  state$current <- character()
  state$text <- ""
  state$header <- NULL
  state$rows <- list()

  local_name <- function(x) sub("^.*:", "", x)
  append_row <- function(values) {
    if (is.null(state$header)) {
      keys <- canon(values)
      is_header <- sum(nzchar(keys)) >= 3L &&
        any(grepl("code", keys, fixed = TRUE)) &&
        any(grepl("name", keys, fixed = TRUE))
      if (is_header) state$header <- make.unique(keys)
      return(invisible(NULL))
    }
    width <- length(state$header)
    values <- c(values, rep("", max(0L, width - length(values))))[seq_len(width)]
    row_text <- paste(canon(values), collapse = " ")
    if (!nzchar(trimws(row_text)) || grepl("^total ", row_text) ||
        grepl("report generated", row_text, fixed = TRUE) ||
        grepl("local government directory$", row_text)) {
      return(invisible(NULL))
    }
    state$rows[[length(state$rows) + 1L]] <- values
    invisible(NULL)
  }

  list(
    startElement = function(name, attrs) {
      tag <- local_name(name)
      if (identical(tag, "Row")) {
        state$in_row <- TRUE
        state$cursor <- 1L
        state$current <- character()
      } else if (state$in_row && identical(tag, "Cell")) {
        idx <- spreadsheetml_index(attrs)
        if (is.finite(idx)) state$cursor <- idx
      } else if (state$in_row && identical(tag, "Data")) {
        state$in_data <- TRUE
        state$text <- ""
      }
    },
    text = function(x) {
      if (state$in_data) state$text <- paste0(state$text, x)
    },
    endElement = function(name) {
      tag <- local_name(name)
      if (state$in_row && identical(tag, "Data")) {
        if (state$cursor > length(state$current) + 1L) {
          state$current <- c(
            state$current,
            rep("", state$cursor - length(state$current) - 1L)
          )
        }
        state$current[[state$cursor]] <- state$text
        state$cursor <- state$cursor + 1L
        state$in_data <- FALSE
      } else if (identical(tag, "Row")) {
        append_row(state$current)
        state$in_row <- FALSE
      }
    },
    result = function() {
      if (is.null(state$header)) return(NULL)
      if (!length(state$rows)) {
        out <- as.data.frame(matrix(character(), nrow = 0L, ncol = length(state$header)))
      } else {
        out <- as.data.frame(do.call(rbind, state$rows), stringsAsFactors = FALSE)
      }
      names(out) <- state$header
      out[, nzchar(names(out)), drop = FALSE]
    }
  )
}

read_lgd_spreadsheetml <- function(path) {
  need_pkg("XML", "LGD SpreadsheetML modification reports")
  handlers <- spreadsheetml_handlers()
  XML::xmlEventParse(
    path,
    handlers = handlers[c("startElement", "text", "endElement")],
    addContext = FALSE,
    useTagName = FALSE,
    trim = FALSE
  )
  out <- handlers$result()
  if (is.null(out)) {
    stop("Could not locate the data header in LGD modification report: ", path, call. = FALSE)
  }
  out
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

read_lineage_csv <- function(path, select = NULL) {
  need_pkg("data.table", "district-lineage CSV inputs")
  args <- list(
    file = path,
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE,
    na.strings = c("", "NA")
  )
  if (!is.null(select)) args$select <- select
  safe_df(do.call(data.table::fread, args))
}

resolve_shrug_input_columns <- function(path, role) {
  need_pkg("data.table", "SHRUG key inputs")
  header <- names(data.table::fread(
    path,
    nrows = 0L,
    data.table = FALSE,
    check.names = FALSE,
    showProgress = FALSE
  ))
  keys <- canon(header)
  patterns <- if (identical(role, "locality")) {
    c("shrid2", "shrid", "state id", "state code", "district id", "district code",
      "subdistrict id", "sub district id", "town village id", "village id", "town id",
      "pca tot p", "population", "pop total", "total population", "pop", "land area", "area")
  } else {
    c("shrid2", "shrid", "state id", "state code", "district id", "district code")
  }
  keep <- Reduce(`|`, lapply(patterns, function(pattern) grepl(pattern, keys, fixed = TRUE)))
  selected <- header[keep]
  if (!any(canon(selected) %in% c("shrid2", "shrid"))) {
    stop("SHRUG key is missing shrid2: ", path, call. = FALSE)
  }
  selected
}

read_shrug_key <- function(path, role) {
  read_lineage_csv(path, select = resolve_shrug_input_columns(path, role))
}

modification_level <- function(source_id) {
  levels <- c(
    lgd_mod_districts = "district",
    lgd_mod_subdistricts = "subdistrict",
    lgd_mod_villages = "village",
    lgd_mod_urban_local_bodies = "urban_local_body"
  )
  level <- unname(levels[[source_id]])
  if (is.null(level)) stop("Unknown LGD modification source: ", source_id, call. = FALSE)
  level
}

read_lineage_source <- function(path, reader, source_id = NA_character_) {
  switch(
    reader,
    lgd_json = read_lgd_json_records(path),
    lgd_xlsx = read_lgd_xlsx_table(path),
    xlsx = {
      need_pkg("readxl", "district-lineage Excel inputs")
      safe_df(readxl::read_excel(path, .name_repair = "minimal"))
    },
    spreadsheetml = standardize_lgd_modification_roster(
      read_lgd_spreadsheetml(path), modification_level(source_id)
    ),
    csv = read_lineage_csv(path),
    shrug_locality_csv = read_shrug_key(path, "locality"),
    shrug_district_csv = read_shrug_key(path, "district"),
    gpkg = {
      need_pkg("sf", "district-lineage geometries")
      sf::st_read(path, quiet = TRUE)
    },
    inventory_only = path,
    stop("Unknown district-lineage reader: ", reader, call. = FALSE)
  )
}

#' Read one district-lineage source branch
read_district_lineage_v2_source <- function(spec, path) {
  spec <- safe_df(spec)
  if (nrow(spec) != 1L) stop("Each source branch must contain one specification row.", call. = FALSE)
  if (!file.exists(path)) stop("A tracked district-lineage input disappeared before it could be read.", call. = FALSE)
  list(
    source_id = spec$source_id[[1]],
    value = read_lineage_source(path, spec$reader[[1]], spec$source_id[[1]])
  )
}

#' Assemble source branches by semantic source ID
assemble_district_lineage_v2_sources <- function(branches) {
  if (!length(branches)) return(list())
  ids <- vapply(branches, function(x) x$source_id %||% NA_character_, character(1))
  if (anyNA(ids) || any(!nzchar(ids))) {
    stop("Every district-lineage source branch must have a source_id.", call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    duplicate <- unique(ids[duplicated(ids)])
    stop("District-lineage source branches contain duplicate source IDs: ",
         paste(duplicate, collapse = ", "), call. = FALSE)
  }
  stats::setNames(lapply(branches, `[[`, "value"), ids)
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

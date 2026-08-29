# Source-specific readers for predetermined Census 2001 district controls.

pad_census_code <- function(x, width = 2L) {
  value <- suppressWarnings(as.integer(num(x)))
  out <- rep(NA_character_, length(value))
  keep <- is.finite(value)
  out[keep] <- sprintf(paste0("%0", width, "d"), value[keep])
  out
}

read_census_xls_directory <- function(path, skip) {
  if (!dir.exists(path)) stop("Missing Census table directory: ", path, call. = FALSE)
  files <- sort(list.files(path, pattern = "\\.xls$", full.names = TRUE, ignore.case = TRUE))
  if (!length(files)) stop("No .xls files found in Census table directory: ", path, call. = FALSE)
  need_pkg("readxl", "Census 2001 Excel tables")
  safe_bind_rows(lapply(files, function(file) {
    out <- readxl::read_excel(file, col_names = FALSE, skip = skip)
    out$.source_file <- basename(file)
    safe_df(out)
  }))
}

read_shrug_pca_2001_district <- function(paths) {
  root <- path_project(paths, "data", "raw", "shrug", "census_2001")
  csv_path <- file.path(root, "pc01_pca_clean_pc01dist.csv")
  zip_path <- file.path(root, "shrug-pca01-csv.zip")
  if (file.exists(csv_path)) return(utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE))
  read_shrug_district_archive(
    zip_path, "pc01_pca_clean_pc01dist.csv", "SHRUG Census-2001 PCA archive"
  )
}

read_census_2001_control_sources <- function(paths) {
  root <- path_project(paths, "data", "raw", "census_2001")
  list(
    shrug_pca = read_shrug_pca_2001_district(paths),
    c01 = read_census_xls_directory(file.path(root, "religion", "C01"), 7L),
    c08 = read_census_xls_directory(file.path(root, "education", "C08"), 7L),
    c14 = read_census_xls_directory(file.path(root, "age", "C14"), 7L),
    h09 = read_census_xls_directory(file.path(root, "housing", "H09"), 5L)
  )
}

census_key_frame <- function(state, district) {
  data.frame(
    state_code_2001 = pad_census_code(state, 2L),
    district_code_2001 = pad_census_code(district, 2L),
    stringsAsFactors = FALSE
  )
}

validate_census_source_shape <- function(x, minimum_columns, source) {
  x <- safe_df(x)
  if (ncol(x) < minimum_columns) {
    stop(source, " has ", ncol(x), " columns; expected at least ", minimum_columns, ".", call. = FALSE)
  }
  x
}

validate_census_district_rows <- function(x, source, expected_n = NULL) {
  keys <- census_2001_keys()
  if (!all(keys %in% names(x))) stop(source, " lacks standardized state-district keys.", call. = FALSE)
  if (any(!stats::complete.cases(x[keys]))) stop(source, " contains missing state-district keys.", call. = FALSE)
  if (anyDuplicated(x[keys])) stop(source, " contains duplicate state-district rows.", call. = FALSE)
  if (!is.null(expected_n) && nrow(x) != expected_n) {
    stop(source, " contains ", nrow(x), " districts; expected ", expected_n, ".", call. = FALSE)
  }
  x
}

clean_shrug_pca_2001_district <- function(x) {
  x <- safe_df(x)
  required <- c("pc01_state_id", "pc01_district_id", "pc01_pca_no_hh", "pc01_pca_tot_p",
    "pc01_pca_p_06", "pc01_pca_p_sc", "pc01_pca_p_st", "pc01_pca_p_lit",
    "pc01_pca_tot_work_p", "pc01_pca_main_cl_p", "pc01_pca_main_al_p",
    "pc01_pca_marg_cl_p", "pc01_pca_marg_al_p")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("SHRUG PCA is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  out <- census_key_frame(x$pc01_state_id, x$pc01_district_id)
  out$households_total <- num(x$pc01_pca_no_hh)
  out$population_total <- num(x$pc01_pca_tot_p)
  out$population_age_0_6 <- num(x$pc01_pca_p_06)
  out$population_age_7_plus <- out$population_total - out$population_age_0_6
  out$sc_population <- num(x$pc01_pca_p_sc)
  out$st_population <- num(x$pc01_pca_p_st)
  out$literate_population <- num(x$pc01_pca_p_lit)
  out$workers_total <- num(x$pc01_pca_tot_work_p)
  out$cultivators <- num(x$pc01_pca_main_cl_p) + num(x$pc01_pca_marg_cl_p)
  out$agricultural_labourers <- num(x$pc01_pca_main_al_p) + num(x$pc01_pca_marg_al_p)
  validate_census_district_rows(out, "SHRUG PCA")
}

clean_census_c01_district <- function(x) {
  x <- validate_census_source_shape(x, 14L, "Census C-01")
  keep <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 4L) == "0000" &
    pad_census_code(x[[5]], 8L) == "00000000" & canon(x[[7]]) == "total"
  x <- x[keep %in% TRUE, , drop = FALSE]
  out <- census_key_frame(x[[2]], x[[3]])
  out$religion_population_total <- num(x[[8]])
  out$hindu_population <- num(x[[11]])
  out$muslim_population <- num(x[[14]])
  validate_census_district_rows(out, "Census C-01")
}

clean_census_c08_district <- function(x) {
  x <- validate_census_source_shape(x, 41L, "Census C-08")
  district <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 4L) == "0000" & canon(x[[6]]) == "total"
  all_age <- x[district %in% TRUE & canon(x[[7]]) == "all ages", , drop = FALSE]
  child <- x[district %in% TRUE & canon(x[[7]]) == "0 6", , drop = FALSE]
  all_out <- census_key_frame(all_age[[2]], all_age[[3]])
  all_out$education_population_all <- num(all_age[[8]])
  all_out$adult_secondary_plus <- rowSums(cbind(num(all_age[[29]]), num(all_age[[32]]), num(all_age[[35]]), num(all_age[[38]]), num(all_age[[41]])), na.rm = FALSE)
  child_out <- census_key_frame(child[[2]], child[[3]])
  child_out$education_population_0_6 <- num(child[[8]])
  out <- merge(all_out, child_out, by = c("state_code_2001", "district_code_2001"), all = TRUE, sort = FALSE)
  out$education_population_age_7_plus <- out$education_population_all - out$education_population_0_6
  validate_census_district_rows(out, "Census C-08")
}

clean_census_c14_district <- function(x) {
  x <- validate_census_source_shape(x, 13L, "Census C-14")
  district <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 8L) == "00000000"
  x <- x[district %in% TRUE, , drop = FALSE]
  age <- canon(x[[6]])
  keys <- census_key_frame(x[[2]], x[[3]])
  x$state_code_2001 <- keys$state_code_2001
  x$district_code_2001 <- keys$district_code_2001
  x$.population <- num(x[[7]])
  x$.urban_population <- num(x[[13]])
  groups <- list(
    population_age_0_14 = c("0 4", "5 9", "10 14"),
    population_age_15_64 = c("15 19", "20 24", "25 29", "30 34", "35 39", "40 44", "45 49", "50 54", "55 59", "60 64"),
    population_age_65_plus = c("65 69", "70 74", "75 79", "80")
  )
  pieces <- lapply(names(groups), function(nm) {
    z <- x[age %in% groups[[nm]], c("state_code_2001", "district_code_2001", ".population"), drop = FALSE]
    z <- aggregate_census_2001_counts(z, ".population", keys = c("state_code_2001", "district_code_2001"))
    names(z)[names(z) == ".population"] <- nm
    z
  })
  all_age <- x[age == "all ages", c("state_code_2001", "district_code_2001", ".urban_population"), drop = FALSE]
  names(all_age)[[3]] <- "population_urban"
  out <- combine_census_2001_count_sources(c(pieces, list(all_age)), keys = census_2001_keys(), require_same_keys = TRUE)
  validate_census_district_rows(out, "Census C-14")
}

clean_census_h09_district <- function(x) {
  x <- validate_census_source_shape(x, 8L, "Census H-09")
  keep <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 4L) == "0000" & canon(x[[6]]) == "total"
  x <- x[keep %in% TRUE, , drop = FALSE]
  out <- census_key_frame(x[[2]], x[[3]])
  out$lighting_households_total <- num(x[[7]])
  out$households_electricity <- num(x[[8]])
  validate_census_district_rows(out, "Census H-09")
}

census_geometry_keys <- function(geometry) {
  x <- safe_df(geometry)
  keys <- census_2001_keys()
  if (all(keys %in% names(x))) {
    out <- x[keys]
  } else {
    if (!"unit_id" %in% names(x)) {
      stop(
        "Census geometry lacks both standardized district keys and canonical unit_id.",
        call. = FALSE
      )
    }
    unit_id <- trimws(plain_chr(x$unit_id))
    valid <- grepl("^pc2001__[0-9]{2}__[0-9]{2}$", unit_id)
    if (any(!valid)) {
      stop("Census geometry contains malformed Census-2001 unit_id values.", call. = FALSE)
    }
    parts <- strsplit(unit_id, "__", fixed = TRUE)
    out <- data.frame(
      state_code_2001 = vapply(parts, `[[`, character(1), 2L),
      district_code_2001 = vapply(parts, `[[`, character(1), 3L),
      stringsAsFactors = FALSE
    )
  }
  out$state_code_2001 <- pad_census_code(out$state_code_2001, 2L)
  out$district_code_2001 <- pad_census_code(out$district_code_2001, 2L)
  if (any(!stats::complete.cases(out[keys]))) {
    stop("Census geometry contains missing standardized district keys.", call. = FALSE)
  }
  if (anyDuplicated(out[keys])) {
    stop("Census geometry is not unique by district.", call. = FALSE)
  }
  out
}

census_geometry_area <- function(geometry) {
  need_pkg("sf", "Census 2001 district area")
  if (!inherits(geometry, "sf")) {
    stop("Census geometry is not an sf object.", call. = FALSE)
  }
  out <- census_geometry_keys(geometry)
  out$area_sq_km <- as.numeric(sf::st_area(sf::st_transform(geometry, 6933))) / 1e6
  out
}

build_census_2001_district_totals <- function(raw_sources, geometry) {
  count_sources <- list(
    shrug_pca = clean_shrug_pca_2001_district(raw_sources$shrug_pca),
    c01 = clean_census_c01_district(raw_sources$c01),
    c08 = clean_census_c08_district(raw_sources$c08),
    c14 = clean_census_c14_district(raw_sources$c14),
    h09 = clean_census_h09_district(raw_sources$h09)
  )
  count_sources <- Map(
    function(x, source) validate_census_district_rows(x, source, expected_n = 593L),
    count_sources,
    names(count_sources)
  )
  out <- combine_census_2001_count_sources(
    count_sources,
    keys = census_2001_keys(),
    require_same_keys = TRUE
  )
  area <- census_geometry_area(geometry)
  out <- merge(out, area, by = census_2001_keys(), all.x = TRUE, sort = FALSE)
  out$population_age_7_plus <- ifelse(
    is.finite(out$education_population_age_7_plus),
    out$education_population_age_7_plus,
    out$population_age_7_plus
  )
  out$households_total <- ifelse(
    is.finite(out$lighting_households_total),
    out$lighting_households_total,
    out$households_total
  )
  validate_census_district_rows(out, "Combined Census controls", expected_n = 593L)
}

summarise_census_2001_source_coverage <- function(raw_sources, geometry) {
  sources <- list(
    shrug_pca = clean_shrug_pca_2001_district(raw_sources$shrug_pca),
    c01 = clean_census_c01_district(raw_sources$c01),
    c08 = clean_census_c08_district(raw_sources$c08),
    c14 = clean_census_c14_district(raw_sources$c14),
    h09 = clean_census_h09_district(raw_sources$h09),
    geometry = census_geometry_area(geometry)
  )
  reference <- sources$shrug_pca[census_2001_keys()]
  reference_key <- do.call(paste, c(reference, sep = "__"))
  safe_bind_rows(lapply(names(sources), function(source) {
    x <- sources[[source]]
    observed_key <- do.call(paste, c(x[census_2001_keys()], sep = "__"))
    data.frame(
      source = source,
      rows = nrow(x),
      unique_districts = length(unique(observed_key)),
      missing_from_reference = length(setdiff(reference_key, observed_key)),
      unexpected_vs_reference = length(setdiff(observed_key, reference_key)),
      stringsAsFactors = FALSE
    )
  }))
}

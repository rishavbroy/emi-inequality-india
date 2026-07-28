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
  if (!file.exists(zip_path)) stop("Missing SHRUG Census 2001 PCA archive: ", zip_path, call. = FALSE)
  listing <- utils::unzip(zip_path, list = TRUE)
  hit <- listing$Name[basename(listing$Name) == "pc01_pca_clean_pc01dist.csv"]
  if (!length(hit)) stop("SHRUG PCA archive lacks pc01_pca_clean_pc01dist.csv", call. = FALSE)
  utils::read.csv(unz(zip_path, hit[[1]]), stringsAsFactors = FALSE, check.names = FALSE)
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
  out
}

clean_census_c01_district <- function(x) {
  x <- safe_df(x)
  keep <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 4L) == "0000" &
    pad_census_code(x[[5]], 8L) == "00000000" & canon(x[[7]]) == "total"
  x <- x[keep %in% TRUE, , drop = FALSE]
  out <- census_key_frame(x[[2]], x[[3]])
  out$religion_population_total <- num(x[[8]])
  out$hindu_population <- num(x[[11]])
  out$muslim_population <- num(x[[14]])
  out
}

clean_census_c08_district <- function(x) {
  x <- safe_df(x)
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
  out
}

clean_census_c14_district <- function(x) {
  x <- safe_df(x)
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
  combine_census_2001_count_sources(c(pieces, list(all_age)), keys = c("state_code_2001", "district_code_2001"))
}

clean_census_h09_district <- function(x) {
  x <- safe_df(x)
  keep <- pad_census_code(x[[3]], 2L) != "00" & pad_census_code(x[[4]], 4L) == "0000" & canon(x[[6]]) == "total"
  x <- x[keep %in% TRUE, , drop = FALSE]
  out <- census_key_frame(x[[2]], x[[3]])
  out$lighting_households_total <- num(x[[7]])
  out$households_electricity <- num(x[[8]])
  out
}

census_geometry_area <- function(geometry) {
  need_pkg("sf", "Census 2001 district area")
  required <- c("state_code_2001", "district_code_2001")
  if (!inherits(geometry, "sf") || !all(required %in% names(geometry))) stop("Census geometry lacks standardized district keys.", call. = FALSE)
  out <- sf::st_drop_geometry(geometry[required])
  out$area_sq_km <- as.numeric(sf::st_area(sf::st_transform(geometry, 6933))) / 1e6
  if (anyDuplicated(out[required])) stop("Census geometry is not unique by district.", call. = FALSE)
  out
}

build_census_2001_district_totals <- function(raw_sources, geometry) {
  out <- combine_census_2001_count_sources(list(
    clean_shrug_pca_2001_district(raw_sources$shrug_pca),
    clean_census_c01_district(raw_sources$c01),
    clean_census_c08_district(raw_sources$c08),
    clean_census_c14_district(raw_sources$c14),
    clean_census_h09_district(raw_sources$h09),
    census_geometry_area(geometry)
  ), keys = c("state_code_2001", "district_code_2001"))
  out$population_age_7_plus <- ifelse(is.finite(out$education_population_age_7_plus), out$education_population_age_7_plus, out$population_age_7_plus)
  out$households_total <- ifelse(is.finite(out$lighting_households_total), out$lighting_households_total, out$households_total)
  out
}

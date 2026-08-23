# Census C-13 single-year age returns used for elementary-age denominators.

census_c13_manifest_files <- function(paths, census_year, manifest_file = NULL) {
  census_year <- as.integer(census_year)
  if (!census_year %in% c(2001L, 2011L)) {
    stop("Census C-13 reader supports only 2001 and 2011.", call. = FALSE)
  }
  manifest_file <- manifest_file %||% path_metadata(
    paths, sprintf("census_%d_download_manifest.tsv", census_year)
  )
  manifest <- utils::read.delim(
    manifest_file,
    sep = "\t",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )
  required <- c("table", "state_code", "relative_path", "url")
  if (!identical(names(manifest), required)) {
    stop("Unexpected Census download-manifest schema: ", manifest_file, call. = FALSE)
  }
  rows <- manifest[manifest$table == "C13", , drop = FALSE]
  if (nrow(rows) != 35L || anyDuplicated(rows$state_code) ||
      !setequal(rows$state_code, sprintf("%02d", 1:35))) {
    stop(
      sprintf("Census %d C-13 manifest must contain one row for each state/UT code 01-35.", census_year),
      call. = FALSE
    )
  }
  files <- file.path(paths$root, rows$relative_path)
  missing <- files[!file.exists(files) | file.info(files)$size <= 0]
  if (length(missing)) {
    stop(
      "Missing Census C-13 files. Run `make download-census-tables` before extended diagnostics.\n",
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  files
}

normalize_census_code <- function(x, width) {
  raw <- gsub("[^0-9]", "", plain_chr(x))
  vapply(raw, function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_character_)
    sprintf(paste0("%0", width, "d"), as.integer(value))
  }, character(1))
}

clean_census_district_label <- function(x) {
  out <- sub("^District\\s*-\\s*", "", trimws(plain_chr(x)), ignore.case = TRUE)
  out <- sub("\\s*\\([0-9]+\\)\\s*$", "", out)
  out <- sub("\\s+[0-9]+\\s*$", "", out)
  trimws(out)
}

parse_census_c13_sheet <- function(raw, census_year) {
  raw <- safe_df(raw)
  census_year <- as.integer(census_year)
  required_columns <- if (census_year == 2001L) 7L else 6L
  if (ncol(raw) < required_columns) {
    stop("Census C-13 sheet has fewer columns than expected.", call. = FALSE)
  }

  if (census_year == 2001L) {
    out <- data.frame(
      table = plain_chr(raw[[1]]),
      state_code = normalize_census_code(raw[[2]], 2L),
      district_code = normalize_census_code(raw[[3]], 2L),
      subdistrict_code = normalize_census_code(raw[[4]], 4L),
      district_name = clean_census_district_label(raw[[5]]),
      age = suppressWarnings(as.integer(num(raw[[6]]))),
      persons = num(raw[[7]]),
      stringsAsFactors = FALSE
    )
    keep <- !is.na(out$table) & out$table == "C3713" &
      !is.na(out$district_code) & out$district_code != "00" &
      !is.na(out$subdistrict_code) & out$subdistrict_code == "0000" &
      out$age %in% 6:13
  } else if (census_year == 2011L) {
    out <- data.frame(
      table = plain_chr(raw[[1]]),
      state_code = normalize_census_code(raw[[2]], 2L),
      district_code = normalize_census_code(raw[[3]], 3L),
      district_name = clean_census_district_label(raw[[4]]),
      age = suppressWarnings(as.integer(num(raw[[5]]))),
      persons = num(raw[[6]]),
      stringsAsFactors = FALSE
    )
    keep <- !is.na(out$table) & out$table == "C3713" &
      !is.na(out$district_code) & out$district_code != "000" &
      out$age %in% 6:13
  } else {
    stop("Census C-13 parser supports only 2001 and 2011.", call. = FALSE)
  }
  out <- out[keep & is.finite(out$persons) & out$persons >= 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

read_census_c13_file <- function(path, census_year) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required for Census C-13 workbooks.", call. = FALSE)
  }
  sheets <- readxl::excel_sheets(path)
  rows <- lapply(sheets, function(sheet) {
    raw <- readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
    parse_census_c13_sheet(raw, census_year)
  })
  out <- safe_bind_rows(rows)
  out$source_file <- basename(path)
  out
}

summarise_census_c13_age_6_13 <- function(rows, census_year) {
  rows <- safe_df(rows)
  if (!nrow(rows)) return(data.frame())
  key <- paste(rows$state_code, rows$district_code, rows$age, sep = "|")
  if (anyDuplicated(key)) {
    stop("Census C-13 has duplicate district-by-single-age rows.", call. = FALSE)
  }
  district_key <- paste(rows$state_code, rows$district_code, sep = "|")
  groups <- split(seq_len(nrow(rows)), district_key)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- rows[index, , drop = FALSE]
    if (!identical(sort(part$age), 6:13)) {
      stop(
        "Census C-13 district does not contain exactly one observation for every age 6-13: ",
        part$state_code[[1]], "/", part$district_code[[1]],
        call. = FALSE
      )
    }
    data.frame(
      census_year = as.integer(census_year),
      state_code = part$state_code[[1]],
      district_code = part$district_code[[1]],
      district_name = part$district_name[[1]],
      census_age_6_13_population = sum(part$persons),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

read_census_c13_age_6_13 <- function(files, census_year) {
  rows <- safe_bind_rows(lapply(files, read_census_c13_file, census_year = census_year))
  summarise_census_c13_age_6_13(rows, census_year)
}

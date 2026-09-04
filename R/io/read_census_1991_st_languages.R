# Census-1991 Scheduled Tribe language tables used for the pre-treatment
# language-acquisition diagnostic. ST-17 district workbooks contain the
# bilingual/trilingual detail; ST-16 supplies an independent mother-tongue
# denominator check on the same historical geography.

census_1991_st17_district_files <- function(
    paths, manifest_file = path_metadata(paths, "census_1991_download_manifest.tsv")) {
  manifest <- read_census_download_manifest(manifest_file)
  rows <- manifest[toupper(manifest$table) == "ST17T_DISTRICT", , drop = FALSE]
  if (!nrow(rows) || anyDuplicated(rows$relative_path)) {
    stop("Census 1991 ST-17 district manifest is empty or duplicated.", call. = FALSE)
  }
  files <- file.path(paths$root, rows$relative_path)
  missing <- files[!file.exists(files) | file.info(files)$size <= 0]
  if (length(missing)) {
    stop(
      "Missing Census 1991 ST-17 district files. Run `make download-census-tables` before extended diagnostics.\n",
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  files
}

census_1991_st16_files <- function(
    paths, manifest_file = path_metadata(paths, "census_1991_download_manifest.tsv")) {
  manifest <- read_census_download_manifest(manifest_file)
  rows <- manifest[toupper(manifest$table) == "ST16T", , drop = FALSE]
  if (!nrow(rows) || anyDuplicated(rows$state_code) || anyDuplicated(rows$relative_path)) {
    stop("Census 1991 ST-16 manifest is empty or duplicated.", call. = FALSE)
  }
  files <- file.path(paths$root, rows$relative_path)
  missing <- files[!file.exists(files) | file.info(files)$size <= 0]
  if (length(missing)) {
    stop(
      "Missing Census 1991 ST-16 files. Run `make download-census-tables` before extended diagnostics.\n",
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  files
}

read_census_1991_st_language_sheet <- function(path) {
  need_pkg("readxl", "Census 1991 Scheduled Tribe language workbooks")
  sheets <- readxl::excel_sheets(path)
  if (length(sheets) != 1L) {
    stop("Census 1991 Scheduled Tribe language workbooks must contain one worksheet: ", path, call. = FALSE)
  }
  readxl::read_excel(
    path, sheet = sheets[[1L]], col_names = FALSE, col_types = "text",
    .name_repair = "minimal"
  )
}

census_1991_st_language_text <- function(x) {
  out <- trimws(plain_chr(x))
  out[is.na(out)] <- ""
  out
}

census_1991_st_language_count <- function(x) {
  raw <- census_1991_st_language_text(x)
  out <- num(gsub(",", "", raw, fixed = TRUE))
  out[tolower(raw) == "nil"] <- 0
  out
}

census_1991_st17_language_label <- function(x) {
  label <- census_1991_st_language_text(x)
  label <- sub("^[0-9]+\\.\\s*", "", label)
  normalize_language_label(label)
}

empty_census_1991_st17_rows <- function() {
  data.frame(
    state_code_1991 = character(), district_code_1991 = character(),
    district_name = character(), residence = character(), mother_tongue = character(),
    mother_tongue_speakers = numeric(), monolingual_speakers = numeric(),
    bilingual_speakers = numeric(), english_second_language = numeric(),
    english_third_language = numeric(), hindi_second_language = numeric(),
    hindi_third_language = numeric(), stringsAsFactors = FALSE
  )
}

parse_census_1991_st17_sheet <- function(raw, state_code_1991, district_code_1991) {
  raw <- safe_df(raw)
  if (ncol(raw) < 10L) stop("Census 1991 ST-17 sheet has fewer than ten columns.", call. = FALSE)
  state_code_1991 <- normalize_census_code(state_code_1991, 2L)
  district_code_1991 <- normalize_census_code(district_code_1991, 2L)
  if (is.na(state_code_1991) || is.na(district_code_1991)) {
    stop("Census 1991 ST-17 parser requires valid historical state and district codes.", call. = FALSE)
  }

  district_name <- clean_census_district_label(raw[[1L]][[8L]])
  rows <- list()
  current <- NULL
  current_second <- NA_character_
  residence <- NA_character_
  in_all_st <- FALSE
  finished <- FALSE

  finalize <- function() {
    if (is.null(current)) return(invisible(NULL))
    second <- current$second
    third <- current$third
    english_second <- if ("English" %in% names(second)) unname(second["English"]) else 0
    hindi_second <- if ("Hindi" %in% names(second)) unname(second["Hindi"]) else 0
    third_names <- names(third)
    english_third <- if (length(third)) sum(third[grepl("\\rEnglish$", third_names)], na.rm = TRUE) else 0
    hindi_third <- if (length(third)) sum(third[grepl("\\rHindi$", third_names)], na.rm = TRUE) else 0
    rows[[length(rows) + 1L]] <<- data.frame(
      state_code_1991 = state_code_1991,
      district_code_1991 = district_code_1991,
      district_name = district_name,
      residence = residence,
      mother_tongue = current$mother_tongue,
      mother_tongue_speakers = current$mother_tongue_speakers,
      monolingual_speakers = current$monolingual_speakers,
      bilingual_speakers = current$bilingual_speakers,
      english_second_language = english_second,
      english_third_language = english_third,
      hindi_second_language = hindi_second,
      hindi_third_language = hindi_third,
      stringsAsFactors = FALSE
    )
    current <<- NULL
    current_second <<- NA_character_
    invisible(NULL)
  }

  for (i in seq_len(nrow(raw))) {
    first <- census_1991_st_language_text(raw[[1L]][[i]])
    second_col <- census_1991_st_language_text(raw[[2L]][[i]])
    third_col <- census_1991_st_language_text(raw[[6L]][[i]])

    if (!in_all_st) {
      if (identical(first, "All Scheduled Tribes")) in_all_st <- TRUE
      next
    }
    if (first %in% c("Rural", "Urban")) {
      finalize()
      residence <- first
      next
    }
    if (nzchar(first) && !grepl("^[0-9]+\\.\\s*", first) &&
        !first %in% c("Bilinguals", "Name of Second", "Language")) {
      finalize()
      finished <- TRUE
      break
    }

    tongue <- sub("^.*Total No\\. of\\s+(.*?)\\s*speakers.*$", "\\1", second_col, ignore.case = TRUE)
    is_tongue_total <- grepl("Total No\\. of\\s+.+?\\s*speakers", second_col, ignore.case = TRUE)
    if (is_tongue_total) {
      finalize()
      current <- list(
        mother_tongue = normalize_language_label(tongue),
        mother_tongue_speakers = census_1991_st_language_count(raw[[6L]][[i]]),
        monolingual_speakers = NA_real_,
        bilingual_speakers = NA_real_,
        second = numeric(),
        third = numeric()
      )
      next
    }
    if (is.null(current)) next

    if (grepl("^Monolinguals$", second_col, ignore.case = TRUE)) {
      value <- census_1991_st_language_count(raw[[6L]][[i]])
      if (is.finite(value)) {
        current$monolingual_speakers <- if (is.finite(current$monolingual_speakers)) {
          max(current$monolingual_speakers, value)
        } else value
      }
    }
    if (grepl("^Bilinguals \\(including trilinguals\\)$", second_col, ignore.case = TRUE)) {
      value <- census_1991_st_language_count(raw[[6L]][[i]])
      if (is.finite(value)) {
        current$bilingual_speakers <- if (is.finite(current$bilingual_speakers)) {
          max(current$bilingual_speakers, value)
        } else value
      }
    }

    if (grepl("^[0-9]+\\.\\s*", first)) {
      current_second <- census_1991_st17_language_label(first)
      value <- census_1991_st_language_count(raw[[2L]][[i]])
      if (!is.finite(value) || value < 0) {
        stop("Census 1991 ST-17 contains an invalid second-language count.", call. = FALSE)
      }
      prior <- current$second[current_second]
      current$second[current_second] <- if (!length(prior) || is.na(prior)) value else max(prior, value)
    }

    ignored_third <- c("Total Trilinguals", "Not Speaking A", "Third Language", "Total", "Males", "Females")
    third_language <- normalize_language_label(third_col)
    is_third_header <- tolower(third_language) %in% tolower(ignored_third)
    if (!is.na(current_second) && nzchar(third_col) && !is_third_header) {
      value <- census_1991_st_language_count(raw[[8L]][[i]])
      if (!is.finite(value) || value < 0) {
        stop("Census 1991 ST-17 contains an invalid third-language count.", call. = FALSE)
      }
      key <- paste(current_second, third_language, sep = "\r")
      prior <- current$third[key]
      current$third[key] <- if (!length(prior) || is.na(prior)) value else max(prior, value)
    }
  }
  if (!finished) finalize()
  out <- if (length(rows)) safe_bind_rows(rows) else empty_census_1991_st17_rows()
  if (!nrow(out)) return(out)

  count_fields <- c(
    "mother_tongue_speakers", "monolingual_speakers", "bilingual_speakers",
    "english_second_language", "english_third_language",
    "hindi_second_language", "hindi_third_language"
  )
  validate_census_1991_nonnegative_counts(out, count_fields, "Census 1991 ST-17")
  if (any(out$monolingual_speakers + out$bilingual_speakers != out$mother_tongue_speakers)) {
    stop("Census 1991 ST-17 monolingual and bilingual counts must exhaust mother-tongue speakers.", call. = FALSE)
  }
  if (any(out$english_second_language + out$english_third_language > out$bilingual_speakers) ||
      any(out$hindi_second_language + out$hindi_third_language > out$bilingual_speakers)) {
    stop("Census 1991 ST-17 English/Hindi acquisition counts exceed published bilingual counts.", call. = FALSE)
  }
  out
}

census_1991_st17_file_codes <- function(path) {
  match <- regexec("1991-ST17T-([0-9]{2})([0-9]{2})\\.xlsx$", basename(path), ignore.case = TRUE)
  parts <- regmatches(basename(path), match)[[1L]]
  if (length(parts) != 3L || parts[[3L]] == "00") {
    stop("Unexpected Census 1991 ST-17 district filename: ", path, call. = FALSE)
  }
  c(state_code_1991 = parts[[2L]], district_code_1991 = parts[[3L]])
}

aggregate_census_1991_st17_residence <- function(rows) {
  rows <- safe_df(rows)
  if (!nrow(rows)) return(rows)
  keys <- c("state_code_1991", "district_code_1991", "district_name", "mother_tongue")
  counts <- c(
    "mother_tongue_speakers", "monolingual_speakers", "bilingual_speakers",
    "english_second_language", "english_third_language",
    "hindi_second_language", "hindi_third_language"
  )
  out <- stats::aggregate(rows[counts], rows[keys], sum)
  out$english_acquisition_speakers <- out$english_second_language + out$english_third_language
  out$hindi_acquisition_speakers <- out$hindi_second_language + out$hindi_third_language
  out$bilingual_share <- safe_count_share(out$bilingual_speakers, out$mother_tongue_speakers)
  out$english_acquisition_share <- safe_count_share(
    out$english_acquisition_speakers, out$mother_tongue_speakers
  )
  out$hindi_acquisition_share <- safe_count_share(
    out$hindi_acquisition_speakers, out$mother_tongue_speakers
  )
  out$english_minus_hindi_acquisition_share <-
    out$english_acquisition_share - out$hindi_acquisition_share
  out
}

read_census_1991_st17_districts <- function(files) {
  if (!length(files) || any(!file.exists(files))) stop("Census 1991 ST-17 district files are missing.", call. = FALSE)
  rows <- safe_bind_rows(lapply(files, function(path) {
    code <- census_1991_st17_file_codes(path)
    parse_census_1991_st17_sheet(
      read_census_1991_st_language_sheet(path),
      code[["state_code_1991"]], code[["district_code_1991"]]
    )
  }))
  aggregate_census_1991_st17_residence(rows)
}

parse_census_1991_st16_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 12L) stop("Census 1991 ST-16 sheet has fewer than twelve columns.", call. = FALSE)
  out <- data.frame(
    state_code_1991 = normalize_census_code(raw[[1L]], 2L),
    district_code_1991 = normalize_census_code(raw[[3L]], 2L),
    district_name = clean_census_district_label(raw[[4L]]),
    scheduled_tribe = normalize_language_label(raw[[5L]]),
    mother_tongue = normalize_language_label(raw[[6L]]),
    rural_speakers = census_1991_st_language_count(raw[[7L]]),
    urban_speakers = census_1991_st_language_count(raw[[10L]]),
    stringsAsFactors = FALSE
  )
  all_st <- out$scheduled_tribe %in% c("All Scheduled Tribes", "All Sheduled Tribes")
  keep <- all_st & !is.na(out$state_code_1991) & !is.na(out$district_code_1991) &
    out$district_code_1991 != "00" & nzchar(out$mother_tongue)
  out <- out[keep %in% TRUE, , drop = FALSE]
  validate_census_1991_nonnegative_counts(out, c("rural_speakers", "urban_speakers"), "Census 1991 ST-16")
  out$mother_tongue_speakers_st16 <- out$rural_speakers + out$urban_speakers
  out[c("state_code_1991", "district_code_1991", "district_name", "mother_tongue", "mother_tongue_speakers_st16")]
}

read_census_1991_st16 <- function(files) {
  if (!length(files) || any(!file.exists(files))) stop("Census 1991 ST-16 files are missing.", call. = FALSE)
  out <- safe_bind_rows(lapply(files, function(path) {
    parse_census_1991_st16_sheet(read_census_1991_st_language_sheet(path))
  }))
  keys <- c("state_code_1991", "district_code_1991", "mother_tongue")
  if (anyDuplicated(out[keys])) stop("Census 1991 ST-16 has duplicate district mother-tongue rows.", call. = FALSE)
  out
}

# Census 2001 C-17 bilingualism/trilingualism reader and denominator contracts.
#
# C-17 is hierarchical: native-language totals contain first-subsidiary
# language counts, and each first-subsidiary parent may contain second-
# subsidiary language counts. The hierarchy must be preserved so trilingual
# speakers are never added twice to the multilingual denominator.

census_c17_manifest_files <- function(paths, manifest_file = NULL) {
  census_manifest_files(paths, 2001L, "C17", manifest_file)
}

clean_census_state_label <- function(x) {
  out <- sub("^STATE\\s*-\\s*", "", trimws(plain_chr(x)), ignore.case = TRUE)
  trimws(sub("\\s+[0-9]{2}\\s*$", "", out))
}

census_c17_validate_sex_triplet <- function(persons, males, females, label) {
  complete <- is.finite(persons) & is.finite(males) & is.finite(females)
  if (any(complete & persons != males + females)) {
    stop("Census 2001 C-17 ", label, " violates Persons = Males + Females.", call. = FALSE)
  }
}

parse_census_c17_sheet <- function(raw) {
  raw <- safe_df(raw)
  if (ncol(raw) < 17L) stop("Census 2001 C-17 sheet has fewer than 17 columns.", call. = FALSE)

  state_code_raw <- normalize_census_code(raw[[1]], 4L)
  data_rows <- !is.na(state_code_raw) & grepl("^[0-9]{4}$", state_code_raw)
  invalid_state_rows <- data_rows & substr(state_code_raw, 3L, 4L) != "00"
  if (any(invalid_state_rows)) {
    stop("Census 2001 C-17 contains a non-state geographic code.", call. = FALSE)
  }

  wide <- data.frame(
    state_code = ifelse(data_rows, substr(state_code_raw, 1L, 2L), NA_character_),
    state_name = clean_census_state_label(raw[[2]]),
    native_language_code = normalize_census_code(raw[[3]], 6L),
    native_language = normalize_census_language_label(raw[[4]]),
    native_persons = num(raw[[5]]),
    native_males = num(raw[[6]]),
    native_females = num(raw[[7]]),
    first_subsidiary_code = normalize_census_code(raw[[8]], 6L),
    first_subsidiary_language = normalize_census_language_label(raw[[9]]),
    first_persons = num(raw[[10]]),
    first_males = num(raw[[11]]),
    first_females = num(raw[[12]]),
    second_subsidiary_code = normalize_census_code(raw[[13]], 6L),
    second_subsidiary_language = normalize_census_language_label(raw[[14]]),
    second_persons = num(raw[[15]]),
    second_males = num(raw[[16]]),
    second_females = num(raw[[17]]),
    stringsAsFactors = FALSE
  )
  wide <- wide[
    data_rows & !is.na(wide$native_language_code) &
      !is.na(wide$native_language) & nzchar(wide$native_language),
    , drop = FALSE
  ]
  if (!nrow(wide)) return(data.frame())

  census_c17_validate_sex_triplet(wide$native_persons, wide$native_males, wide$native_females, "native-speaker count")
  census_c17_validate_sex_triplet(wide$first_persons, wide$first_males, wide$first_females, "first-subsidiary count")
  census_c17_validate_sex_triplet(wide$second_persons, wide$second_males, wide$second_females, "second-subsidiary count")

  sex_spec <- list(
    Persons = c("native_persons", "first_persons", "second_persons"),
    Males = c("native_males", "first_males", "second_males"),
    Females = c("native_females", "first_females", "second_females")
  )
  out <- safe_bind_rows(lapply(names(sex_spec), function(sex) {
    columns <- sex_spec[[sex]]
    data.frame(
      state_code = wide$state_code,
      state_name = wide$state_name,
      native_language_code = wide$native_language_code,
      native_language = wide$native_language,
      native_speakers = wide[[columns[[1L]]]],
      first_subsidiary_code = wide$first_subsidiary_code,
      first_subsidiary_language = wide$first_subsidiary_language,
      first_subsidiary_speakers = wide[[columns[[2L]]]],
      second_subsidiary_code = wide$second_subsidiary_code,
      second_subsidiary_language = wide$second_subsidiary_language,
      second_subsidiary_speakers = wide[[columns[[3L]]]],
      sex = sex,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

read_census_c17_file <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required for Census C-17 workbooks.", call. = FALSE)
  }
  rows <- safe_bind_rows(lapply(readxl::excel_sheets(path), function(sheet) {
    raw <- readxl::read_excel(
      path,
      sheet = sheet,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
    parse_census_c17_sheet(raw)
  }))
  if (!nrow(rows)) stop("Census 2001 C-17 workbook contains no data rows: ", path, call. = FALSE)

  expected_state <- sub("^.*_([0-9]{2})\\.[^.]+$", "\\1", basename(path))
  observed_states <- unique(rows$state_code)
  if (length(observed_states) != 1L || !identical(observed_states, expected_state)) {
    stop("Census 2001 C-17 workbook state code disagrees with its file name: ", path, call. = FALSE)
  }
  rows$source_file <- basename(path)
  rows
}

validate_census_c17_language_codes <- function(rows) {
  rows <- safe_df(rows)
  pairs <- safe_bind_rows(list(
    data.frame(code = rows$native_language_code, label = rows$native_language),
    data.frame(code = rows$first_subsidiary_code, label = rows$first_subsidiary_language),
    data.frame(code = rows$second_subsidiary_code, label = rows$second_subsidiary_language)
  ))
  pairs <- pairs[!is.na(pairs$code) & nzchar(pairs$code) & !is.na(pairs$label) & nzchar(pairs$label), , drop = FALSE]
  by_code <- split(pairs$label, pairs$code)
  conflicts <- names(by_code)[vapply(by_code, function(x) length(unique(x)) != 1L, logical(1))]
  if (length(conflicts)) {
    stop("Census 2001 C-17 language codes map to conflicting labels: ", paste(conflicts, collapse = ", "), call. = FALSE)
  }

  required <- c(`006000` = "Hindi", `040000` = "English")
  observed <- vapply(names(required), function(code) {
    values <- unique(pairs$label[pairs$code == code])
    if (length(values) == 1L) values[[1L]] else NA_character_
  }, character(1))
  if (!identical(unname(observed), unname(required))) {
    stop("Census 2001 C-17 does not resolve Hindi and English codes deterministically.", call. = FALSE)
  }
  invisible(rows)
}

validate_census_c17_hierarchy <- function(rows) {
  rows <- safe_df(rows)
  parent <- rows[
    !is.na(rows$first_subsidiary_code) & is.finite(rows$first_subsidiary_speakers),
    c("state_code", "native_language_code", "sex", "first_subsidiary_code", "first_subsidiary_speakers"),
    drop = FALSE
  ]
  parent_key <- paste(parent$state_code, parent$native_language_code, parent$sex, parent$first_subsidiary_code, sep = "|")
  if (anyDuplicated(parent_key)) stop("Census 2001 C-17 repeats a first-subsidiary parent count.", call. = FALSE)

  child <- rows[
    !is.na(rows$second_subsidiary_code) & is.finite(rows$second_subsidiary_speakers),
    c("state_code", "native_language_code", "sex", "first_subsidiary_code", "second_subsidiary_speakers"),
    drop = FALSE
  ]
  if (nrow(child) && any(is.na(child$first_subsidiary_code))) {
    stop("Census 2001 C-17 second-subsidiary rows lack a first-subsidiary parent.", call. = FALSE)
  }
  if (nrow(child)) {
    child_key <- paste(child$state_code, child$native_language_code, child$sex, child$first_subsidiary_code, sep = "|")
    parent_value <- parent$first_subsidiary_speakers[match(child_key, parent_key)]
    if (any(!is.finite(parent_value))) {
      stop("Census 2001 C-17 second-subsidiary rows cannot be matched to a parent count.", call. = FALSE)
    }
    child_sum <- tapply(child$second_subsidiary_speakers, child_key, sum)
    parent_sum <- parent$first_subsidiary_speakers[match(names(child_sum), parent_key)]
    if (any(child_sum > parent_sum)) {
      stop("Census 2001 C-17 second-subsidiary counts exceed their first-subsidiary parent.", call. = FALSE)
    }
  }
  invisible(rows)
}

read_census_c17_records <- function(files) {
  rows <- safe_bind_rows(lapply(files, read_census_c17_file))
  expected_states <- sprintf("%02d", 1:35)
  if (!setequal(unique(rows$state_code), expected_states)) {
    stop("Census 2001 C-17 records do not cover all state/UT codes 01-35.", call. = FALSE)
  }
  validate_census_c17_language_codes(rows)
  validate_census_c17_hierarchy(rows)
  rows
}

collapse_census_c17_state_languages <- function(rows) {
  rows <- safe_df(rows)
  validate_census_c17_hierarchy(rows)
  key <- paste(rows$state_code, rows$native_language_code, rows$sex, sep = "|")
  groups <- split(seq_len(nrow(rows)), key)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- rows[index, , drop = FALSE]
    native <- unique(part$native_speakers[is.finite(part$native_speakers)])
    if (length(native) != 1L) {
      stop("Census 2001 C-17 requires one native-speaker total per state-language-sex cell.", call. = FALSE)
    }
    native_labels <- unique(part$native_language[!is.na(part$native_language) & nzchar(part$native_language)])
    state_labels <- unique(part$state_name[!is.na(part$state_name) & nzchar(part$state_name)])
    if (length(native_labels) != 1L || length(state_labels) != 1L) {
      stop("Census 2001 C-17 state/native-language labels are not unique within a cell.", call. = FALSE)
    }

    first <- part[!is.na(part$first_subsidiary_code) & is.finite(part$first_subsidiary_speakers), , drop = FALSE]
    multilingual <- sum(first$first_subsidiary_speakers)
    english_first <- sum(first$first_subsidiary_speakers[first$first_subsidiary_code == "040000"])
    hindi_first <- sum(first$first_subsidiary_speakers[first$first_subsidiary_code == "006000"])
    english_second <- sum(part$second_subsidiary_speakers[
      part$second_subsidiary_code == "040000" & part$first_subsidiary_code != "040000"
    ], na.rm = TRUE)
    hindi_second <- sum(part$second_subsidiary_speakers[
      part$second_subsidiary_code == "006000" & part$first_subsidiary_code != "006000"
    ], na.rm = TRUE)
    english <- english_first + english_second
    hindi <- hindi_first + hindi_second

    data.frame(
      state_code = part$state_code[[1L]],
      state_name = state_labels[[1L]],
      native_language_code = part$native_language_code[[1L]],
      native_language = native_labels[[1L]],
      sex = part$sex[[1L]],
      native_speakers = native[[1L]],
      multilingual_speakers = multilingual,
      english_multilingual_speakers = english,
      hindi_multilingual_speakers = hindi,
      multilingual_share_native = if (native[[1L]] > 0) 100 * multilingual / native[[1L]] else NA_real_,
      english_share_multilingual = if (multilingual > 0) 100 * english / multilingual else NA_real_,
      hindi_share_multilingual = if (multilingual > 0) 100 * hindi / multilingual else NA_real_,
      english_share_native = if (native[[1L]] > 0) 100 * english / native[[1L]] else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  if (any(out$english_multilingual_speakers < 0 | out$hindi_multilingual_speakers < 0 |
      out$multilingual_speakers < 0 | out$native_speakers < 0) ||
      any(out$english_multilingual_speakers > out$multilingual_speakers) ||
      any(out$hindi_multilingual_speakers > out$multilingual_speakers) ||
      any(out$multilingual_speakers > out$native_speakers)) {
    stop("Census 2001 C-17 violates 0 <= language acquisition <= multilingual <= native speakers.", call. = FALSE)
  }
  cell_key <- paste(out$state_code, out$native_language_code, out$sex, sep = "|")
  if (anyDuplicated(cell_key)) stop("Collapsed Census 2001 C-17 cells are not unique.", call. = FALSE)
  rownames(out) <- NULL
  out
}

validate_census_c17_against_c16 <- function(c17, c16_state_totals) {
  c17 <- safe_df(c17)
  c16 <- safe_df(c16_state_totals)
  persons <- c17[c17$sex == "Persons", c(
    "state_code", "native_language_code", "native_language", "native_speakers"
  ), drop = FALSE]
  names(persons)[3:4] <- c("native_language_c17", "native_speakers_c17")
  names(c16)[names(c16) == "native_language"] <- "native_language_c16"
  names(c16)[names(c16) == "native_speakers"] <- "native_speakers_c16"
  comparison <- merge(
    persons, c16,
    by = c("state_code", "native_language_code"), all = TRUE, sort = FALSE
  )
  bad <- is.na(comparison$native_speakers_c17) | is.na(comparison$native_speakers_c16) |
    is.na(comparison$native_language_c17) | is.na(comparison$native_language_c16) |
    comparison$native_speakers_c17 != comparison$native_speakers_c16 |
    comparison$native_language_c17 != comparison$native_language_c16
  if (any(bad)) {
    stop(
      "Census 2001 C-17 native-speaker totals do not reconcile exactly to C-16 state language totals (",
      sum(bad), " mismatched cells).",
      call. = FALSE
    )
  }
  invisible(c17)
}

read_census_c17_state_languages <- function(files, c16_state_totals) {
  rows <- read_census_c17_records(files)
  out <- collapse_census_c17_state_languages(rows)
  validate_census_c17_against_c16(out, c16_state_totals)
  out
}

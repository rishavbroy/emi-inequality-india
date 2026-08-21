# This file is part of the EMI inequality research pipeline.

dyen_list_number <- function(dyen, list) {
  if (is.numeric(list)) {
    value <- as.integer(list)
  } else {
    hit <- dyen$lists$list_number[plain_chr(dyen$lists$list_name) == plain_chr(list)]
    if (length(hit) != 1L) stop("Dyen list must resolve uniquely: ", list, call. = FALSE)
    value <- as.integer(hit[[1]])
  }
  value
}

dyen_pairwise_cognacy <- function(dyen, list_a, list_b) {
  a <- dyen$forms[dyen$forms$list_number == dyen_list_number(dyen, list_a), c("meaning", "ccn"), drop = FALSE]
  b <- dyen$forms[dyen$forms$list_number == dyen_list_number(dyen, list_b), c("meaning", "ccn"), drop = FALSE]
  names(a)[[2]] <- "ccn_a"
  names(b)[[2]] <- "ccn_b"
  out <- merge(a, b, by = "meaning", all = FALSE, sort = TRUE)
  out <- out[out$ccn_a != 0L & out$ccn_b != 0L, , drop = FALSE]
  if (!nrow(out)) return(data.frame())

  out$ccn1 <- pmin(out$ccn_a, out$ccn_b)
  out$ccn2 <- pmax(out$ccn_a, out$ccn_b)
  relationships <- dyen$relationships
  if (nrow(relationships)) {
    out <- merge(out, relationships, by = c("meaning", "ccn1", "ccn2"), all.x = TRUE, sort = FALSE)
  } else {
    out$relationship <- NA_integer_
  }

  same <- out$ccn_a == out$ccn_b
  positive_same <- same & (
    (out$ccn_a >= 2L & out$ccn_a <= 99L) |
      (out$ccn_a >= 200L & out$ccn_a <= 399L)
  )
  doubtful_same <- same & (
    (out$ccn_a >= 100L & out$ccn_a <= 199L) |
      (out$ccn_a >= 400L & out$ccn_a <= 499L)
  )

  out$status <- "not_cognate"
  out$status[positive_same | (!same & out$relationship == 2L)] <- "cognate"
  out$status[doubtful_same | (!same & out$relationship == 3L)] <- "doubtful"
  out
}

dyen_pairwise_cognate_percent <- function(dyen, list_a, list_b) {
  if (identical(dyen_list_number(dyen, list_a), dyen_list_number(dyen, list_b))) return(100)
  rows <- dyen_pairwise_cognacy(dyen, list_a, list_b)
  determinate <- rows$status %in% c("cognate", "not_cognate")
  denominator <- sum(determinate)
  if (!denominator) return(NA_real_)
  100 * sum(rows$status[determinate] == "cognate") / denominator
}

dyen_hindi_cognate_table <- function(dyen, hindi = "Hindi") {
  safe_bind_rows(lapply(seq_len(nrow(dyen$lists)), function(i) {
    list_name <- plain_chr(dyen$lists$list_name[[i]])
    data.frame(
      list_number = as.integer(dyen$lists$list_number[[i]]),
      list_name = list_name,
      percent_cognates_with_hindi = dyen_pairwise_cognate_percent(dyen, hindi, list_name),
      stringsAsFactors = FALSE
    )
  }))
}

dyen_shastry_benchmarks <- function() {
  data.frame(
    list_name = c(
      "Bengali", "English ST", "Gujarati", "Kashmiri",
      "Marathi", "Nepali List", "Panjabi ST"
    ),
    expected_percent = c(64.1, 14.6, 64.6, 42.4, 56.4, 64.2, 74.5),
    stringsAsFactors = FALSE
  )
}

validate_dyen_shastry_benchmarks <- function(x) {
  expected <- dyen_shastry_benchmarks()
  observed <- x$percent_cognates_with_hindi[match(expected$list_name, x$list_name)]
  if (any(!is.finite(observed)) || !identical(round(observed, 1), expected$expected_percent)) {
    stop("Parsed Dyen cognate percentages do not reproduce Shastry's reported benchmark values.", call. = FALSE)
  }
  invisible(TRUE)
}

read_lexical_language_index <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "lexical_language_index.csv")
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  required <- c("language", "dyen_list_name", "kogan_code", "match_basis", "source_note")
  if (!all(required %in% names(out)) || anyDuplicated(normalize_language_label(out$language))) {
    stop("Lexical language index has an invalid schema or duplicate language rows.", call. = FALSE)
  }
  out
}

lexical_language_evidence <- function(language, dyen_hindi, index = read_lexical_language_index()) {
  key <- normalize_language_label(language)
  idx <- match(key, normalize_language_label(index$language))
  dyen_name <- rep(NA_character_, length(key))
  kogan_code <- rep(NA_character_, length(key))
  found <- !is.na(idx)
  dyen_name[found] <- plain_chr(index$dyen_list_name[idx[found]])
  kogan_code[found] <- plain_chr(index$kogan_code[idx[found]])
  dyen_name[!nzchar(dyen_name)] <- NA_character_
  kogan_code[!nzchar(kogan_code)] <- NA_character_
  dyen_pct <- dyen_hindi$percent_cognates_with_hindi[match(dyen_name, dyen_hindi$list_name)]
  data.frame(
    dyen_list_name = dyen_name,
    dyen_cognate_pct_hindi = dyen_pct,
    kogan_code = kogan_code,
    stringsAsFactors = FALSE
  )
}

save_dyen_hindi_cognates <- function(
  x,
  path = "outputs/diagnostics/extended/instrument_relevance/dyen_hindi_cognates.csv"
) {
  write_diagnostic_csv(x, path)
}

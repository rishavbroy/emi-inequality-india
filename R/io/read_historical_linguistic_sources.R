# This file is part of the EMI inequality research pipeline.

historical_linguistic_file <- function(rows, file_id) {
  hit <- rows$absolute_path[rows$file_id == file_id]
  if (length(hit) != 1L) {
    stop("Historical linguistic-source manifest must contain exactly one row for ", file_id, ".", call. = FALSE)
  }
  hit[[1]]
}

read_ethnologue_newick_proxy <- function(path) {
  out <- utils::read.delim(
    path,
    sep = "\t",
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  required <- c("Family", "Success", "Comments", "Tree")
  if (!identical(names(out), required)) {
    stop("Ethnologue Newick export must be the four-column tab-separated Dediu format.", call. = FALSE)
  }
  indo <- out[out$Family == "Indo-European", , drop = FALSE]
  if (nrow(indo) != 1L || !identical(indo$Success[[1]], "SUCCESS")) {
    stop("Ethnologue Newick export must contain one successful Indo-European tree.", call. = FALSE)
  }
  if (!grepl("atomic branch length = 1", indo$Comments[[1]], fixed = TRUE)) {
    stop("Ethnologue Newick export does not declare unit split-count branch lengths.", call. = FALSE)
  }
  if (!startsWith(trimws(indo$Tree[[1]]), "(")) {
    stop("Ethnologue Indo-European tree is malformed.", call. = FALSE)
  }
  list(table = out, indo_european_tree = indo$Tree[[1]])
}

parse_dyen_1997_lines <- function(lines) {
  current_meaning <- NA_integer_
  current_ccn <- NA_integer_
  form_rows <- list()
  relationship_rows <- list()
  form_i <- 0L
  relationship_i <- 0L

  for (line in lines) {
    if (grepl("^a\\s+[0-9]{3}\\s", line)) {
      current_meaning <- suppressWarnings(as.integer(substr(line, 3L, 5L)))
      current_ccn <- NA_integer_
      next
    }
    if (grepl("^b", line) && is.finite(current_meaning)) {
      match <- regmatches(line, regexpr("[0-9]{3}\\s*$", line))
      current_ccn <- suppressWarnings(as.integer(trimws(match)))
      next
    }
    if (grepl("^c", line) && is.finite(current_meaning)) {
      numbers <- regmatches(line, gregexpr("[0-9]+", line))[[1]]
      if (length(numbers) >= 3L) {
        values <- as.integer(tail(numbers, 3L))
        relationship_i <- relationship_i + 1L
        relationship_rows[[relationship_i]] <- data.frame(
          meaning = current_meaning,
          ccn1 = min(values[[1]], values[[3]]),
          relationship = values[[2]],
          ccn2 = max(values[[1]], values[[3]]),
          stringsAsFactors = FALSE
        )
      }
      next
    }
    if (startsWith(line, "  ") && is.finite(current_meaning) && is.finite(current_ccn)) {
      meaning <- suppressWarnings(as.integer(trimws(substr(line, 3L, 5L))))
      list_number <- suppressWarnings(as.integer(trimws(substr(line, 7L, 8L))))
      if (!is.finite(meaning) || !is.finite(list_number)) next
      if (!identical(meaning, current_meaning)) {
        stop("Dyen form row does not match its current meaning header.", call. = FALSE)
      }
      form_i <- form_i + 1L
      form_rows[[form_i]] <- data.frame(
        meaning = meaning,
        list_number = list_number,
        list_name = trimws(substr(line, 10L, 24L)),
        ccn = current_ccn,
        stringsAsFactors = FALSE
      )
    }
  }

  forms <- safe_bind_rows(form_rows)
  relationships <- safe_bind_rows(relationship_rows)
  if (!nrow(forms)) stop("Dyen source contains no parsed form rows.", call. = FALSE)
  if (anyDuplicated(forms[c("meaning", "list_number")])) {
    stop("Dyen source contains duplicate meaning/list form rows.", call. = FALSE)
  }
  list(forms = forms, relationships = relationships)
}

read_dyen_1997 <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!any(grepl("COMPARATIVE INDOEUROPEAN DATABASE COLLECTED BY ISIDORE DYEN", lines, fixed = TRUE))) {
    stop("Dyen source header is missing.", call. = FALSE)
  }
  parsed <- parse_dyen_1997_lines(lines)
  parsed$lists <- unique(parsed$forms[c("list_number", "list_name")])
  parsed$path <- path
  parsed
}

read_historical_linguistic_sources <- function(paths = build_paths()) {
  rows <- require_manifest_files(
    paths,
    source_id = c("ethnologue_newick_proxy", "dyen_1997", "kogan_2017")
  )
  ethnologue <- read_ethnologue_newick_proxy(
    historical_linguistic_file(rows, "ethnologue_newick_proxy")
  )
  dyen <- read_dyen_1997(historical_linguistic_file(rows, "dyen1997_raw"))
  dyen_hindi <- dyen_hindi_cognate_table(dyen)
  validate_dyen_shastry_benchmarks(dyen_hindi)

  list(
    ethnologue_proxy = ethnologue,
    dyen = dyen,
    dyen_hindi = dyen_hindi,
    kogan_pdf = historical_linguistic_file(rows, "kogan2017_pdf")
  )
}

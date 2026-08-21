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

dyen_data_lines <- function(lines) {
  markers <- which(trimws(lines) == "5. THE DATA")
  if (!length(markers)) {
    stop("Dyen source is missing the '5. THE DATA' section.", call. = FALSE)
  }

  # The archived file repeats the section title in its table of contents.
  # The final exact marker introduces the observations themselves.
  marker <- tail(markers, 1L)
  data <- lines[seq.int(marker + 1L, length(lines))]
  first_header <- which(grepl("^a [0-9]{3} ", data))[1]
  if (!is.finite(first_header)) stop("Dyen data section contains no meaning header.", call. = FALSE)
  data[seq.int(first_header, length(data))]
}

dyen_record_type <- function(line) {
  if (grepl("^a [0-9]{3} ", line)) return("header")
  if (grepl("^b\\s+[0-9]{3}\\s*$", line)) return("subheader")
  if (grepl("^c\\s+[0-9]{3}\\s+[23]\\s+[0-9]{3}\\s*$", line)) return("relationship")
  if (grepl("^  [0-9]{3} [0-9]{2} ", line)) return("form")
  "other"
}

parse_dyen_1997_lines <- function(lines) {
  current_meaning <- NA_integer_
  current_ccn <- NA_integer_
  form_rows <- list()
  relationship_rows <- list()
  form_i <- 0L
  relationship_i <- 0L

  for (line in dyen_data_lines(lines)) {
    type <- dyen_record_type(line)

    if (identical(type, "header")) {
      current_meaning <- as.integer(substr(line, 3L, 5L))
      current_ccn <- NA_integer_
      next
    }

    if (identical(type, "subheader")) {
      current_ccn <- as.integer(substr(line, 24L, 26L))
      next
    }

    if (identical(type, "relationship")) {
      if (!is.finite(current_meaning)) {
        stop("Dyen relationship row appears before a meaning header.", call. = FALSE)
      }
      relationship_i <- relationship_i + 1L
      relationship_rows[[relationship_i]] <- data.frame(
        meaning = current_meaning,
        ccn1 = as.integer(substr(line, 27L, 29L)),
        relationship = as.integer(substr(line, 32L, 32L)),
        ccn2 = as.integer(substr(line, 35L, 37L)),
        stringsAsFactors = FALSE
      )
      next
    }

    if (identical(type, "form")) {
      if (!is.finite(current_meaning) || !is.finite(current_ccn)) {
        stop("Dyen form row appears before a complete header/subheader pair.", call. = FALSE)
      }
      meaning <- as.integer(substr(line, 3L, 5L))
      list_number <- as.integer(substr(line, 7L, 8L))
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
  relationships <- unique(safe_bind_rows(relationship_rows))
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
  if (!identical(sort(unique(parsed$forms$meaning)), seq_len(200L))) {
    stop("Dyen source must contain exactly the 200 documented Swadesh meanings.", call. = FALSE)
  }
  if (nrow(parsed$lists) != 95L || length(unique(parsed$lists$list_number)) != 95L) {
    stop("Dyen source must contain exactly the 95 documented speech-variety lists.", call. = FALSE)
  }
  if (nrow(parsed$forms) != 19000L) {
    stop("Dyen source must contain one form record for each of 200 meanings x 95 lists.", call. = FALSE)
  }
  parsed$path <- path
  parsed
}

read_historical_linguistic_sources <- function(paths = build_paths()) {
  rows <- require_manifest_files(
    paths,
    source_id = c("ethnologue_newick_proxy", "dyen_1997", "kogan_2017", "asjp_v21")
  )
  ethnologue <- read_ethnologue_newick_proxy(
    historical_linguistic_file(rows, "ethnologue_newick_proxy")
  )
  dyen <- read_dyen_1997(historical_linguistic_file(rows, "dyen1997_raw"))
  dyen_hindi <- dyen_hindi_cognate_table(dyen)
  validate_dyen_shastry_benchmarks(dyen_hindi)

  asjp_index <- read_asjp_language_index()
  anchors <- asjp_shastry_anchors()
  asjp_forms <- read_asjp_v21(
    historical_linguistic_file(rows, "asjp_v21_archive"),
    list_names = asjp_index$asjp_list_name,
    iso_codes = unique(c(asjp_index$asjp_iso639P3code, anchors$asjp_iso639P3code))
  )
  asjp_distances <- asjp_review_anchor_distances(asjp_forms, asjp_index)

  list(
    ethnologue_proxy = ethnologue,
    dyen = dyen,
    dyen_hindi = dyen_hindi,
    kogan_pdf = historical_linguistic_file(rows, "kogan2017_pdf"),
    kogan_anchor_similarity = read_kogan_2017_anchor_similarity(),
    asjp_review_anchor_distances = asjp_distances,
    asjp_review_summary = asjp_review_summary(asjp_distances)
  )
}

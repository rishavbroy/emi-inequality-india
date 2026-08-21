# This file is part of the EMI inequality research pipeline.

asjp_core_meanings <- function() {
  c(1L, 2L, 3L, 11L, 12L, 18L, 19L, 21L, 22L, 23L, 25L, 28L, 30L, 31L,
    34L, 39L, 40L, 41L, 43L, 44L, 47L, 48L, 51L, 53L, 54L, 57L, 58L, 61L,
    66L, 72L, 74L, 75L, 77L, 82L, 85L, 86L, 92L, 95L, 96L, 100L)
}

read_asjp_language_index <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "asjp_language_index.csv")
  }
  out <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(mother_tongue_code = "character")
  )
  required <- c(
    "mother_tongue_code", "mother_tongue", "asjp_list_name",
    "asjp_iso639P3code", "match_basis", "source_note"
  )
  if (!identical(names(out), required) || anyDuplicated(out$mother_tongue_code)) {
    stop("ASJP language index has an invalid schema or duplicate Census codes.", call. = FALSE)
  }
  out
}

read_kogan_2017_anchor_similarity <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "kogan_2017_anchor_similarity.csv")
  }
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "language_code", "anchor_code", "anchor", "shastry_degree",
    "similarity_pct", "source_page"
  )
  if (!identical(names(out), required) ||
      any(!is.finite(num(out$similarity_pct))) ||
      any(num(out$similarity_pct) < 0 | num(out$similarity_pct) > 100)) {
    stop("Kogan anchor-similarity evidence has an invalid schema or value.", call. = FALSE)
  }
  out
}

empty_kogan_anchor_summary <- function() {
  data.frame(
    nearest_anchor = NA_character_, nearest_degree = NA_real_,
    nearest_similarity = NA_real_, runner_up_anchor = NA_character_,
    runner_up_degree = NA_real_, runner_up_similarity = NA_real_,
    margin_pct = NA_real_, stringsAsFactors = FALSE
  )
}

kogan_anchor_summary <- function(evidence, language_code) {
  required <- c("language_code", "anchor", "shastry_degree", "similarity_pct")
  if (is.null(evidence) || !is.data.frame(evidence) ||
      !all(required %in% names(evidence)) ||
      is.na(language_code) || !nzchar(plain_chr(language_code))) {
    return(empty_kogan_anchor_summary())
  }
  x <- evidence[evidence$language_code == language_code, , drop = FALSE]
  if (!nrow(x)) return(empty_kogan_anchor_summary())
  x <- x[order(num(x$similarity_pct), decreasing = TRUE), , drop = FALSE]
  runner <- if (nrow(x) >= 2L) x[2L, , drop = FALSE] else x[1L, , drop = FALSE]
  data.frame(
    nearest_anchor = x$anchor[[1]], nearest_degree = num(x$shastry_degree[[1]]),
    nearest_similarity = num(x$similarity_pct[[1]]),
    runner_up_anchor = runner$anchor[[1]],
    runner_up_degree = num(runner$shastry_degree[[1]]),
    runner_up_similarity = num(runner$similarity_pct[[1]]),
    margin_pct = num(x$similarity_pct[[1]]) - num(runner$similarity_pct[[1]])
  )
}

asjp_wordlist_headers <- function(lines) {
  grep("^[^[:space:]].*\\{.*\\}$", lines)
}

asjp_parse_transcription <- function(line) {
  fields <- strsplit(line, "\\t", fixed = FALSE)[[1]]
  if (length(fields) < 2L) return(character())
  value <- paste(fields[-1L], collapse = "\\t")
  value <- sub("\\s+//.*$", "", value)
  value <- sub("\\s{2,}.*$", "", value)
  words <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  words <- words[nzchar(words) & words != "XXX" & !startsWith(words, "%")]
  words <- gsub(" ", "", words, fixed = TRUE)
  head(words, 2L)
}

read_asjp_v21 <- function(path, list_names = NULL, iso_codes = NULL) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || !grepl("^\\s*2\\s", lines[[1]])) {
    stop("ASJP v21 text file does not have the expected software header.", call. = FALSE)
  }
  headers <- asjp_wordlist_headers(lines)
  if (!length(headers)) stop("ASJP v21 text file contains no word lists.", call. = FALSE)

  wanted_names <- plain_chr(list_names)
  wanted_iso <- plain_chr(iso_codes)
  rows <- list()
  out_i <- 0L
  core <- asjp_core_meanings()

  for (i in seq_along(headers)) {
    start <- headers[[i]]
    stop <- if (i < length(headers)) headers[[i + 1L]] - 1L else length(lines)
    name <- sub("\\{.*$", "", lines[[start]])
    if (start + 1L > stop) next
    metadata <- lines[[start + 1L]]
    iso <- trimws(substr(metadata, 40L, 42L))
    selected <- (!length(wanted_names) && !length(wanted_iso)) ||
      name %in% wanted_names || iso %in% wanted_iso
    if (!selected) next

    block <- lines[seq.int(start + 2L, stop)]
    form_lines <- block[grepl("^[0-9]{1,3}[[:space:]]", block)]
    for (line in form_lines) {
      concept <- suppressWarnings(as.integer(sub("^([0-9]{1,3}).*$", "\\1", line)))
      if (!is.finite(concept) || !concept %in% core) next
      words <- asjp_parse_transcription(line)
      for (word in words) {
        out_i <- out_i + 1L
        rows[[out_i]] <- data.frame(
          list_name = name,
          iso639P3code = if (grepl("^[a-z]{3}$", iso)) iso else NA_character_,
          concept = concept, form = word, stringsAsFactors = FALSE
        )
      }
    }
  }
  forms <- safe_bind_rows(rows)
  if (!nrow(forms)) stop("ASJP v21 parser found no requested lexical forms.", call. = FALSE)
  forms
}

asjp_attested_meanings <- function(x) length(unique(x$concept))

asjp_normalized_levenshtein <- function(a, b) {
  need_pkg("stringdist", "ASJP LDND lexical evidence")
  denom <- max(nchar(a, type = "chars"), nchar(b, type = "chars"))
  if (!is.finite(denom) || denom == 0L) return(NA_real_)
  stringdist::stringdist(a, b, method = "lv") / denom
}

asjp_meaning_distance <- function(a, b) {
  values <- outer(
    a, b,
    Vectorize(function(x, y) asjp_normalized_levenshtein(x, y))
  )
  mean(values, na.rm = TRUE)
}

asjp_ldnd <- function(list_a, list_b, min_attested = 28L) {
  if (asjp_attested_meanings(list_a) < min_attested ||
      asjp_attested_meanings(list_b) < min_attested) {
    return(data.frame(ldnd = NA_real_, shared_meanings = NA_integer_))
  }
  common <- intersect(unique(list_a$concept), unique(list_b$concept))
  if (!length(common)) return(data.frame(ldnd = NA_real_, shared_meanings = 0L))

  same <- vapply(common, function(concept) {
    asjp_meaning_distance(
      list_a$form[list_a$concept == concept],
      list_b$form[list_b$concept == concept]
    )
  }, numeric(1))

  different <- unlist(lapply(common, function(a_concept) {
    vapply(setdiff(common, a_concept), function(b_concept) {
      asjp_meaning_distance(
        list_a$form[list_a$concept == a_concept],
        list_b$form[list_b$concept == b_concept]
      )
    }, numeric(1))
  }), use.names = FALSE)

  baseline <- mean(different, na.rm = TRUE)
  data.frame(
    ldnd = mean(same, na.rm = TRUE) / baseline,
    shared_meanings = length(common)
  )
}

asjp_shastry_anchors <- function() {
  data.frame(
    anchor = c(
      "Hindi", "Punjabi", "Rajasthani", "Gujarati", "Marathi",
      "Bengali", "Assamese", "Oriya", "Kashmiri", "Sindhi", "Sinhalese"
    ),
    asjp_iso639P3code = c(
      "hin", "pan", "mwr", "guj", "mar", "ben", "asm", "ori", "kas", "snd", "sin"
    ),
    shastry_degree = c(0, 1, 1, 1, 2, 3, 3, 3, 4, 4, 4),
    stringsAsFactors = FALSE
  )
}

asjp_review_anchor_distances <- function(forms, index = read_asjp_language_index()) {
  lists <- split(forms, forms$list_name)
  meta <- unique(forms[c("list_name", "iso639P3code")])
  anchors <- asjp_shastry_anchors()
  rows <- list()
  out_i <- 0L
  for (i in seq_len(nrow(index))) {
    candidates <- unique(meta$list_name[
      meta$list_name == index$asjp_list_name[[i]] |
        (!is.na(meta$iso639P3code) & meta$iso639P3code == index$asjp_iso639P3code[[i]])
    ])
    for (j in seq_len(nrow(anchors))) {
      anchor_lists <- unique(meta$list_name[
        !is.na(meta$iso639P3code) & meta$iso639P3code == anchors$asjp_iso639P3code[[j]]
      ])
      pairs <- list()
      pair_i <- 0L
      for (candidate in candidates) for (anchor_list in anchor_lists) {
        value <- asjp_ldnd(lists[[candidate]], lists[[anchor_list]])
        if (!is.finite(value$ldnd[[1]])) next
        pair_i <- pair_i + 1L
        pairs[[pair_i]] <- value
      }
      pair_df <- safe_bind_rows(pairs)
      out_i <- out_i + 1L
      rows[[out_i]] <- data.frame(
        mother_tongue_code = index$mother_tongue_code[[i]],
        mother_tongue = index$mother_tongue[[i]],
        asjp_list_name = index$asjp_list_name[[i]],
        anchor = anchors$anchor[[j]],
        shastry_degree = anchors$shastry_degree[[j]],
        mean_ldnd = if (nrow(pair_df)) mean(pair_df$ldnd) else NA_real_,
        n_list_pairs = nrow(pair_df),
        min_shared_meanings = if (nrow(pair_df)) min(pair_df$shared_meanings) else NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }
  safe_bind_rows(rows)
}

empty_asjp_review_summary_row <- function(mother_tongue_code = NA_character_) {
  data.frame(
    mother_tongue_code = mother_tongue_code,
    mother_tongue = NA_character_,
    asjp_list_name = NA_character_,
    nearest_anchor = NA_character_,
    nearest_degree = NA_real_,
    nearest_ldnd = NA_real_,
    runner_up_anchor = NA_character_,
    runner_up_degree = NA_real_,
    runner_up_ldnd = NA_real_,
    margin_ldnd = NA_real_,
    status = "not_available",
    stringsAsFactors = FALSE
  )
}

asjp_review_summary_row <- function(summary, mother_tongue_code) {
  required <- c(
    "mother_tongue_code", "nearest_anchor", "nearest_degree",
    "nearest_ldnd", "margin_ldnd", "status"
  )
  if (is.null(summary) || !is.data.frame(summary) ||
      !all(required %in% names(summary))) {
    return(empty_asjp_review_summary_row(mother_tongue_code))
  }
  hit <- summary[
    plain_chr(summary$mother_tongue_code) == plain_chr(mother_tongue_code),
    ,
    drop = FALSE
  ]
  if (!nrow(hit)) return(empty_asjp_review_summary_row(mother_tongue_code))
  hit[1L, , drop = FALSE]
}

asjp_review_summary <- function(distances) {
  safe_bind_rows(lapply(split(distances, distances$mother_tongue_code), function(x) {
    available <- x[is.finite(x$mean_ldnd), , drop = FALSE]
    if (!nrow(available)) return(data.frame(
      mother_tongue_code = x$mother_tongue_code[[1]],
      mother_tongue = x$mother_tongue[[1]],
      asjp_list_name = x$asjp_list_name[[1]],
      nearest_anchor = NA_character_, nearest_degree = NA_real_,
      nearest_ldnd = NA_real_, runner_up_anchor = NA_character_,
      runner_up_degree = NA_real_, runner_up_ldnd = NA_real_,
      margin_ldnd = NA_real_, status = "insufficient_28_item_list",
      stringsAsFactors = FALSE
    ))
    available <- available[order(available$mean_ldnd), , drop = FALSE]
    runner <- if (nrow(available) >= 2L) available[2L, , drop = FALSE] else available[1L, , drop = FALSE]
    data.frame(
      mother_tongue_code = x$mother_tongue_code[[1]],
      mother_tongue = x$mother_tongue[[1]], asjp_list_name = x$asjp_list_name[[1]],
      nearest_anchor = available$anchor[[1]], nearest_degree = available$shastry_degree[[1]],
      nearest_ldnd = available$mean_ldnd[[1]], runner_up_anchor = runner$anchor[[1]],
      runner_up_degree = runner$shastry_degree[[1]], runner_up_ldnd = runner$mean_ldnd[[1]],
      margin_ldnd = runner$mean_ldnd[[1]] - available$mean_ldnd[[1]],
      status = "available", stringsAsFactors = FALSE
    )
  }))
}

save_asjp_review_anchor_distances <- function(
  x,
  path = "outputs/diagnostics/extended/instrument_relevance/asjp_review_anchor_distances.csv"
) write_diagnostic_csv(x, path)

save_asjp_review_summary <- function(
  x,
  path = "outputs/diagnostics/extended/instrument_relevance/asjp_review_summary.csv"
) write_diagnostic_csv(x, path)

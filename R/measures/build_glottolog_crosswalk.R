# This file is part of the EMI inequality research pipeline.

normalize_language_match_name <- function(x) {
  original <- plain_chr(x)
  value <- iconv(original, from = "", to = "ASCII//TRANSLIT")
  value[is.na(value)] <- original[is.na(value)]
  value <- tolower(gsub("[^[:alnum:]]+", " ", value))
  trimws(gsub("\\s+", " ", value))
}

census_language_match_terms <- function(mother_tongue, canonical_language) {
  labels <- c(
    mother_tongue = plain_chr(mother_tongue),
    canonical_language = plain_chr(canonical_language)
  )
  rows <- lapply(names(labels), function(source) {
    value <- trimws(labels[[source]])
    if (is.na(value) || !nzchar(value)) return(NULL)
    pieces <- unique(trimws(c(value, unlist(strsplit(value, "/", fixed = TRUE)))))
    pieces <- pieces[nzchar(pieces)]
    data.frame(
      match_term = pieces,
      normalized_name = normalize_language_match_name(pieces),
      term_source = ifelse(pieces == value, source, paste0(source, "_component")),
      stringsAsFactors = FALSE
    )
  })
  out <- safe_bind_rows(rows)
  out[!duplicated(out[c("normalized_name", "term_source")]), , drop = FALSE]
}

glottolog_alias_index <- function(languoids, cldf) {
  if (!all(c("languages", "names") %in% names(cldf))) {
    stop("Glottolog CLDF bundle lacks language/name tables required for matching.", call. = FALSE)
  }
  primary <- data.frame(
    source_glottocode = plain_chr(cldf$languages$ID),
    alias_name = plain_chr(cldf$languages$Name),
    alias_basis = "primary_name",
    provider = "glottolog",
    stringsAsFactors = FALSE
  )
  aliases <- data.frame(
    source_glottocode = plain_chr(cldf$names$Language_ID),
    alias_name = plain_chr(cldf$names$Name),
    alias_basis = "alternate_name",
    provider = plain_chr(cldf$names$Provider),
    stringsAsFactors = FALSE
  )
  out <- rbind(primary, aliases)
  out$normalized_name <- normalize_language_match_name(out$alias_name)
  out <- out[nzchar(out$normalized_name), , drop = FALSE]
  out$language_glottocode <- vapply(
    out$source_glottocode,
    glottolog_language_node,
    languoids = languoids,
    FUN.VALUE = character(1)
  )
  out <- out[!is.na(out$language_glottocode) & nzchar(out$language_glottocode), , drop = FALSE]

  language_rows <- cldf$languages[match(out$language_glottocode, cldf$languages$ID), , drop = FALSE]
  out$glottolog_name <- plain_chr(language_rows$Name)
  out$family_id <- plain_chr(language_rows$Family_ID)
  out$countries <- plain_chr(language_rows$Countries)
  out$iso639P3code <- plain_chr(language_rows$ISO639P3code)
  out$is_india_language <- grepl("(^|;)IN(;|$)", out$countries)

  basis_rank <- match(out$alias_basis, c("primary_name", "alternate_name"))
  out <- out[order(out$normalized_name, out$language_glottocode, basis_rank, out$provider), , drop = FALSE]
  out <- out[!duplicated(out[c("normalized_name", "language_glottocode")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

census_language_identities <- function(census_2001_languages) {
  rows <- safe_df(census_2001_languages)
  required <- c(
    "state_std", "district_std", "mother_tongue_code",
    "mother_tongue", "canonical_language", "spkr_tot"
  )
  if (!all(required %in% names(rows))) {
    stop("Glottolog matching requires cleaned C-16 mother-tongue leaf rows.", call. = FALSE)
  }
  key <- c("mother_tongue_code", "mother_tongue", "canonical_language")
  speakers <- stats::aggregate(num(rows$spkr_tot), rows[key], sum, na.rm = TRUE)
  names(speakers)[[ncol(speakers)]] <- "national_speakers"

  district_rows <- unique(data.frame(
    rows[key],
    district_panel_id = make_district_key(rows$state_std, rows$district_std, 2001L),
    stringsAsFactors = FALSE
  ))
  districts <- stats::aggregate(
    district_rows$district_panel_id,
    district_rows[key],
    function(x) length(unique(x))
  )
  names(districts)[[ncol(districts)]] <- "n_districts"
  merge(speakers, districts, by = key, all = TRUE, sort = FALSE)
}

glottolog_candidate_tier <- function(term_source, alias_basis) {
  term_rank <- match(
    plain_chr(term_source),
    c("mother_tongue", "mother_tongue_component",
      "canonical_language", "canonical_language_component")
  )
  alias_rank <- match(plain_chr(alias_basis), c("primary_name", "alternate_name"))
  2L * (term_rank - 1L) + alias_rank
}

glottolog_candidate_order <- function(hit) {
  order(
    glottolog_candidate_tier(hit$term_source, hit$alias_basis),
    !hit$is_india_language,
    hit$language_glottocode
  )
}

match_census_language_identity <- function(identity, aliases) {
  terms <- census_language_match_terms(identity$mother_tongue[[1]], identity$canonical_language[[1]])
  hit <- merge(terms, aliases, by = "normalized_name", all = FALSE, sort = FALSE)
  if (!nrow(hit)) {
    out <- identity
    out$match_term <- NA_character_
    out$term_source <- NA_character_
    out$language_glottocode <- NA_character_
    out$glottolog_name <- NA_character_
    out$family_id <- NA_character_
    out$countries <- NA_character_
    out$iso639P3code <- NA_character_
    out$alias_basis <- NA_character_
    out$provider <- NA_character_
    out$is_india_language <- NA
    out$candidate_count <- 0L
    out$candidate_status <- "no_exact_match"
    return(out)
  }

  hit$candidate_tier <- glottolog_candidate_tier(hit$term_source, hit$alias_basis)
  hit <- hit[hit$candidate_tier == min(hit$candidate_tier, na.rm = TRUE), , drop = FALSE]
  hit <- hit[glottolog_candidate_order(hit), , drop = FALSE]
  hit <- hit[!duplicated(hit$language_glottocode), , drop = FALSE]
  out <- identity[rep(1L, nrow(hit)), , drop = FALSE]
  keep <- c(
    "match_term", "term_source", "language_glottocode", "glottolog_name",
    "family_id", "countries", "iso639P3code", "alias_basis", "provider", "is_india_language"
  )
  out <- cbind(out, hit[keep])
  out$candidate_count <- nrow(hit)
  out$candidate_status <- if (nrow(hit) == 1L) "exact_unique" else "exact_ambiguous"
  out
}

#' Generate exact Census-to-Glottolog candidates for manual review
#'
#' This queue is deliberately non-authoritative. It never mutates the maintained
#' Shastry concordance and never turns an exact candidate into a production
#' mapping without a separately reviewed metadata decision.
build_census_glottolog_match_candidates <- function(census_2001_languages, glottolog, cldf, reviews = NULL) {
  identities <- census_language_identities(census_2001_languages)
  aliases <- glottolog_alias_index(glottolog$languoids, cldf)
  out <- safe_bind_rows(lapply(seq_len(nrow(identities)), function(i) {
    match_census_language_identity(identities[i, , drop = FALSE], aliases)
  }))
  if (!nrow(out)) return(out)
  out$review_status <- "unreviewed"
  out$reviewed_glottocode <- NA_character_
  if (!is.null(reviews) && nrow(reviews)) {
    review_index <- match(
      sprintf("%06d", suppressWarnings(as.integer(out$mother_tongue_code))),
      reviews$mother_tongue_code
    )
    found <- !is.na(review_index)
    out$review_status[found] <- plain_chr(reviews$review_status[review_index[found]])
    out$reviewed_glottocode[found] <- plain_chr(reviews$language_glottocode[review_index[found]])
  }
  out <- out[order(-num(out$national_speakers), out$mother_tongue_code, out$language_glottocode), , drop = FALSE]
  rownames(out) <- NULL
  out
}

save_census_glottolog_match_candidates <- function(
  candidates,
  path = "outputs/diagnostics/extended/instrument_relevance/census_glottolog_match_candidates.csv"
) {
  write_diagnostic_csv(candidates, path)
}
